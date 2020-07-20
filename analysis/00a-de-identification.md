Workflow of de-identification approach
================
Chen Qingqing
7/20/2020

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
# load original dataset 
df <- readRDS(here("analysis/data/raw_data/data_original.rds")) %>% 
  dplyr::select(u_id, created_at, lon, lat)  # only keep essential information

# nest dataset by user 
df_nested <- df %>% 
  group_by(u_id) %>% 
  nest() %>% 
  ungroup()
```

## Step 1: Remove explicit identifiers

Replace original user IDs with random generated numbers.

``` r
#generate random number to replace the user IDs
generate_random_number <- function(df) {
  floor(runif(nrow(df), min = 1, max = 100000000))
}

df_nested <- df_nested %>% 
  mutate(u_id = generate_random_number(.) %>% as.character())
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
df_pool <- df_nested %>% unnest(col = data)

swap_tweets <- function(user_id, user_data, df_pool){
    n_tweets <- nrow(user_data)# number of tweets sent by the user 
    n_keep_tweets <- ceiling(0.95 * n_tweets)  # number of tweets to kept by the user 
    n_swap_tweets <- n_tweets - n_keep_tweets # number of tweets to swap 
    noise_pool <- df_pool %>% filter(u_id != user_id) #all tweets that sent by other users 
    df_keep_tweets <- user_data %>% sample_n(., size = n_keep_tweets) 
    df_noise <- noise_pool %>% 
      sample_n(., size = n_swap_tweets) %>% 
      dplyr::select(-u_id)
    new_user_data <- rbind(df_keep_tweets, df_noise) %>% 
      mutate(u_id = user_id) %>% 
      group_by(u_id) %>% 
      nest() %>% 
      ungroup()
    return(new_user_data)
}

df_nested <- do.call(rbind, map2(df_nested$u_id, df_nested$data, with_progress(function(x, y) swap_tweets(x, y, df_pool))))
```

## Step 5: Temporal masking

The timestamps are shifted by a random number of seconds.

``` r
#make sure the timestamp column is 'dttm' type 
offset_timestamps <- function(user_id, user_data){
  user_data %>% 
    mutate(created_at = as.POSIXct(created_at, format="%Y-%m-%d %H:%M:%S", tz = "Asia/Singapore")) %>% 
    mutate(noise = runif(nrow(.), min = -3600, max = 3600)) %>% # generate random time is second
    mutate(hr = hour(created_at)) %>% 
    mutate(created_at = case_when(
            hr %in% c(12, 18) ~ created_at + abs(noise), # make sure the shifted time still in the same timeframe 
            TRUE ~ created_at + noise
           )) %>% 
    dplyr::select(-c(noise, hr)) %>% 
    mutate(u_id = user_id) %>% 
    group_by(u_id) %>% 
    nest() %>% 
    ungroup()
}

df_nested <- do.call(rbind, map2(df_nested$u_id, df_nested$data, with_progress(function(x, y) offset_timestamps(x, y))))
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

swap_days <- function(user_id, user_data){
   user_data %>% 
    mutate(date = as.Date(created_at),
           ori_day = lubridate::wday(date, week_start = getOption("lubridate.week.start", 1))) %>% 
    mutate(random_day = map_dbl(ori_day, random_wday),
           offsets = random_day - ori_day,
           date = date + offsets,
           time = strftime(created_at, format="%H:%M:%S")) %>% 
    unite(created_at, c("date", "time"), sep = " ") %>%
    mutate(created_at = as.POSIXct(created_at, format="%Y-%m-%d %H:%M:%S")) %>% 
    dplyr::select(created_at, lon, lat) %>% 
    mutate(u_id = user_id) %>% 
    group_by(u_id) %>% 
    nest() %>% 
    ungroup()
}

df_nested <- do.call(rbind, map2(df_nested$u_id, df_nested$data, with_progress(function(x, y) swap_days(x, y))))
```

## Step 7 & 8: Geomasking

Locations are offset by a random distance \< 100 meters and then are
aggregated to 750 hexagoral grid cells.

``` r
#generate aggregated grid cells 
grids <- read_sf(here("analysis/data/raw_data/Shp/MP14_SUBZONE_NO_SEA_PL.shp")) %>% # read sg map
  st_transform(., crs = 3414) %>% 
  st_make_valid() %>% 
  st_make_grid(., cellsize = 750, square = F) %>% # generate 750m grid cells 
  st_sf() %>% 
  rowid_to_column("grid_id")

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

geomasking <- function(user_id, user_data){
  #convert to sf object 
  user_data_sf <- user_data %>% st_as_sf(., coords = c("lon", "lat"), crs = 3414)
  #offset points 
  points <- st_sfc(unlist(map(st_geometry(user_data_sf), offset_point), recursive = FALSE), crs = 3414) %>% as_tibble()
  
  user_data_sf %>% 
    st_set_geometry(NULL) %>% # drop original geometry
    bind_cols(., points) %>% # add shifted geometry
    st_as_sf() %>% # convert to sf object 
    st_join(., grids) %>% # aggregate to grid cells 
    st_set_geometry(NULL) %>% # drop geometry
    mutate(u_id = user_id) %>% 
    group_by(u_id) %>% 
    nest() %>% 
    ungroup()
}

df_nested <- do.call(rbind, map2(df_nested$u_id, df_nested$data, with_progress(function(x, y) geomasking(x, y))))
```

## Step 9:

Remove grids with fewer than 5 visted users.

``` r
df <- df_nested %>% 
  unnest(cols = "data") %>% 
  group_by(grid_id) %>% 
  mutate(n_user = n_distinct(u_id), 
         n_tweets = n()) %>% 
  ungroup() %>% 
  filter(n_user >= 5 & n_tweets >= 5) %>% 
  dplyr::select(-c(n_user, n_tweets))
```

The de-identified dataset is in `analysis/data/derived_data`.

``` r
#save de-identified dataset 
saveRDS(df, file = here("analysis/data/derived_data/data_anonymized.rds"))
```
