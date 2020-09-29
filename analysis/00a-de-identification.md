Workflow of de-identification approach
================

We are cognizant that the use of LBS data, and inferring meaningful
locations in particular, brings forward specific privacy considerations
and obliges us to seriously consider research ethics and potential harm.
For this reason, we de-identify the dataset used through perturbation,
random noise as well as spatial and temporal aggregation. Although this
undoubtedly affects the precision and accuracy of any location
inference, we argue that this is a fine trade-off for most analytical
applications at the urban scale.

Ultimately, we only retain a (randomly generated) user ID, a location
attribute, and a date/time attribute while any other variables such as
content of tweet, user description, etc are removed from the source
data.

## Load dataset

``` r
# load your original dataset 
df <-   readRDS("path_to_original_dataset") %>%
  dplyr::select(u_id, created_at, lon, lat) %>%   # only keep essential information
  data.table::as.data.table()

# nest dataset by user 
df_nested <- df %>% nest(data = !u_id) 
```

## Step 1: Remove explicit identifiers

Replace original user IDs with random generated numbers.

``` r
df_nested <- df_nested %>% 
  mutate(u_id = sample(1:100000000, n(), replace = FALSE))
```

## Step 2: Remove users with too few data points

``` r
# users with fewer than 10 tweets are removed 
df_nested <- df_nested %>% 
  mutate(n_tweets = map_dbl(data, nrow)) %>% 
  filter(n_tweets >= 10)
```

## Step 3: Remove top active users

Users with too many data points are likely to be bots, so we remove the
top 0.1% the most active users according to the frequency of tweets.

``` r
#remove top 0.1% the most active users based on the frequency of tweets 
n_user <- n_distinct(df_nested$u_id)
df_nested <- df_nested %>% 
  arrange(desc(n_tweets)) %>% 
  slice(round(n_user*0.1/100):n_user) %>% 
  dplyr::select(-n_tweets)
```

## Step 4: Swapping tweets

For each user, a 5% portion of their tweets is randomly swapped with
other users, effectively introducing a small amount of noise.

``` r
#tweets pool 
df_pool <- df_nested %>% unnest(col = data) %>% data.table::as.data.table()
pool_ids <- df_pool$id

swap_tweets <- function(user_data){
    n_tweets <- nrow(user_data)# number of tweets sent by the user 
    n_keep_tweets <- ceiling(0.95 * n_tweets)  # number of tweets to kept by the user 
    n_swap_tweets <- n_tweets - n_keep_tweets # number of tweets to swap 
    # tweets ids from the user 
    user_tweets_ids <- user_data$id
    # sampling tweets ids from other users 
    noise_pool_ids <- setdiff(pool_ids %>% sample(5000), user_tweets_ids)
    
    # select 5% random tweets (noise) from the pool
    random_noise_ids <- sample(noise_pool_ids, size = n_swap_tweets, replace = FALSE)
    random_noise <- df_pool[id %in% random_noise_ids] %>% dplyr::select(-u_id)
    
    # select 95% random tweets from user's tweets 
    random_user_data <- user_data %>% sample_n(., size = n_keep_tweets) 
    output <- bind_rows(random_user_data, random_noise)
    return(output)
}

df_nested <- df_nested %>% mutate(data = map(data, with_progress(swap_tweets)))
```

## Step 5: Temporal masking

The timestamps are shifted by a random number of seconds.

``` r
offset_timestamps <- function(user_data, timestamp){
  timestamp <- rlang::sym(timestamp)
  user_data %>% 
    mutate(hr = lubridate::hour({{timestamp}}), 
           min = lubridate::minute({{timestamp}}),
           noise = runif(nrow(.), min = -3600, max = 3600)) %>%  # generate random time is second
    mutate({{timestamp}} := case_when(
      hr == 0 & (noise/60 + min) < 0  ~ {{timestamp}} + abs(noise), # add positive noise avoiding date move to the previous day
      hr == 23 & (noise/60 + min) >= 59 ~ {{timestamp}} - abs(noise), # add negative noise avoiding date move to the next day
      TRUE ~ {{timestamp}} + noise
    )) %>% 
      dplyr::select(-c(noise, hr, min))
}
df_nested <- df_nested %>% mutate(data = map(data, with_progress(function(x) offset_timestamps(x, timestamp = "created_at_sg"))))
```

## Step 6: Swapping day of the week

The day of the week is swapped with a similar day (weekday or weekend).

``` r
# swap day of the week (weekdays, weekends)
random_wday <- function(day){
  if(day %in% seq(1, 5)){
    random_day <- sample(seq(1, 5), 1, replace = T)
  } else{
    random_day <- sample(c(6, 7), 1, replace = T)
  }
  return(random_day)
}

swap_days <- function(user_data, timestamp){
  timestamp <- rlang::sym(timestamp)
  user_data %>% 
    mutate(ori_day = lubridate::wday({{timestamp}}, week_start = getOption("lubridate.week.start", 1))) %>% 
    mutate(random_day = map_dbl(ori_day, random_wday),
           offsets = random_day - ori_day,
           {{timestamp}} := {{timestamp}} + lubridate::days(offsets)) %>% 
    dplyr::select(-c(ori_day, random_day, offsets))
}

df_nested <- df_nested %>% mutate(data = map(data, with_progress(function(x) swap_days(x, timestamp = "created_at_sg"))))
```

## Step 7 & 8: Geomasking

Locations are offset by a random distance \< 100 meters and then are
aggregated to 750 hexagoral grid cells.

``` r
#generate aggregated grid cells 
grids <- st_read(here("analysis/data/raw_data/MP14_SUBZONE_NO_SEA_PL.shp"), quiet = T) %>% # read Singapore map
  st_transform(., crs = 3414) %>% 
  st_make_valid() %>% 
  st_make_grid(., cellsize = 750, square = F) %>% # generate 750m hexagonal grid cells 
  st_sf() %>% 
  rowid_to_column("grid_id")
```

The generated hexagonal grids dataset is in
`analysis/data/derived_data`.

``` r
st_write(grids, here("analysis/data/derived_data/spatial_hex_grid.shp"))
```

``` r
offset_point <- function(point){
  #create 100m buffer for a geom point and get 1 random sample within the buffer 
  random_pt <- point %>% 
    st_buffer(., dist = 100) %>% 
    st_sample(., size = 1, type = "random") 
  while(length(random_pt) == 0){ # repeat the process until get a random point in the buffer 
    random_pt <- point %>% 
      st_buffer(., dist = 100) %>% 
      st_sample(., size = 1, type = "random")
  }
  return(random_pt)
}

geomasking <- function(user_data, grids){
  #convert to sf object 
  user_data_sf <- user_data %>% 
    st_as_sf(., coords = c("lon", "lat"), crs = 4326) %>% 
    st_transform(crs = 3414) 
  #offset points and aggregate point to grid
  grid_id <- st_sfc(unlist(map(st_geometry(user_data_sf), offset_point), recursive = FALSE), crs = 3414) %>% 
    st_sf(crs = 3414) %>% # offset points
    st_join(., grids) %>% # aggregate point to grid 
    st_set_geometry(NULL) # drop geometry 
  
  user_data_sf %>% 
    st_set_geometry(NULL) %>% # drop original geometry
    bind_cols(., grid_id) 
}

df_nested <- df_nested %>% mutate(data = map(data, with_progress(function(x) geomasking(x, grids))))
```

## Step 9: Remove grids with too few users

Remove grids with fewer than 5 visited users.

``` r
df <- df_nested %>% 
    unnest(cols = "data") %>% 
    filter(!is.na(grid_id)) %>% 
    group_by(grid_id) %>% 
    mutate(n_user = n_distinct(u_id), 
           n_tweets = n()) %>% 
    ungroup() %>% 
    filter(n_user >= 5 & n_tweets >= 5) %>% 
    dplyr::select(-c(n_user, n_tweets))
```

## Step 10: Remove users at grids that have fewer than 5 residents

``` r
#load inferred home locations 
hm_apdm <- read_csv(here("analysis/data/derived_data/hm_apdm.csv")) %>% mutate(name = "APDM")
hm_freq <- read_csv(here("analysis/data/derived_data/hm_freq.csv")) %>% mutate(name = "FREQ")
hm_hmlc <- read_csv(here("analysis/data/derived_data/hm_hmlc.csv")) %>% mutate(name = "HMLC")
hm_osna <- read_csv(here("analysis/data/derived_data/hm_osna.csv")) %>% mutate(name = "OSNA")
hm_all <- bind_rows(hm_apdm, hm_freq, hm_hmlc, hm_osna)

## get users with home that four recipes matched 
shared_user_uniqe_hm <- hm_all %>% 
  group_by(u_id) %>% 
  dplyr::summarise(n_method = n_distinct(name), 
                   n_home = n_distinct(home)) %>% 
  filter(n_method == 4 & n_home == 1)

# get home grids that have less than 5 identified home users
users2rm <- hm_all %>%
  filter(u_id %in% shared_user_uniqe_hm$u_id) %>%
  dplyr::select(u_id, home) %>%
  distinct(u_id, .keep_all = TRUE) %>%
  group_by(home) %>%
  mutate(n_user = n_distinct(u_id)) %>%
  filter(n_user < 5) %>%
  ungroup() %>%
  pull(u_id)

# remove extracted users from dataset 
df <- df %>% filter(!u_id %in% users2rm)
```

The de-identified dataset is in `analysis/data/derived_data`.

``` r
#save de-identified dataset 
write_csv(df, file = here("analysis/data/derived_data/deidentified_sg_tweets.csv"))
```
