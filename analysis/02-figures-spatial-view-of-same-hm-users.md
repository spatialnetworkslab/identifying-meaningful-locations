Spatial views
================

## Load dataset

``` r
#load de-identified dataset
df <- read_csv(here("analysis/data/derived_data/deidentified_sg_tweets.csv")) %>% 
  mutate(created_at = with_tz(created_at, tzone = "Asia/Singapore")) # the tweets were sent in Singapore, so must convert the timezone to SGT, the default timezone is UTC! 
  
#load grid cells 
grids <-  st_read(here("analysis/data/derived_data/spatial_hex_grid.shp"), quiet = T) %>% 
  st_transform(crs = 3414)

#load central region 
df_central_region <- st_read(here("analysis/data/raw_data/central_region.shp"), quiet = T) 

#load inferred home locations 
hm_apdm <- read_csv(here("analysis/data/derived_data/hm_apdm_updated.csv"))
hm_freq <- read_csv(here("analysis/data/derived_data/hm_freq_updated.csv"))
hm_hmlc <- read_csv(here("analysis/data/derived_data/hm_hmlc_updated.csv"))
hm_osna <- read_csv(here("analysis/data/derived_data/hm_osna_updated.csv"))
hm_all <- bind_rows(hm_apdm, hm_freq, hm_hmlc, hm_osna)
head(hm_all)
```

    ## # A tibble: 6 x 3
    ##       u_id  home name 
    ##      <dbl> <dbl> <chr>
    ## 1 42151437  1622 APDM 
    ## 2 44523269   962 APDM 
    ## 3 71380694  1449 APDM 
    ## 4 59406560   559 APDM 
    ## 5 97357036  1394 APDM 
    ## 6 80510132  1101 APDM

## Spatial views

### Users with inferred home locations that all four approaches agree

``` r
# users with inferred home locations that all four approaches agree
same_hm_users <- hm_all %>% 
  group_by(u_id) %>% 
  dplyr::summarise(n_methods = n_distinct(name),
                   n_homes = n_distinct(home)) %>% 
  filter(n_methods == 4 & n_homes == 1) %>% 
  pull(u_id)
```

### Normalization

``` r
df_tweets_same_hm_users <- df %>% filter(u_id %in% same_hm_users)
considered_grid_ids <- df_tweets_same_hm_users$grid_id %>% unique()

#normalized number of tweets 
norm_tweets <- df_tweets_same_hm_users %>% 
  group_by(grid_id) %>% 
  dplyr::summarise(n_tweets = n()) %>% 
  mutate(norm_n_tweets = (n_tweets - min(n_tweets))/(max(n_tweets)-min(n_tweets))) %>% 
  inner_join(., grids) %>% 
  st_as_sf()

#normalized number of users 
norm_users <- df_tweets_same_hm_users %>% 
  group_by(grid_id) %>%
  summarise(n_users = n_distinct(u_id)) %>% 
  mutate(norm_n_users = (n_users - min(n_users))/(max(n_users)-min(n_users))) %>% 
  inner_join(., grids) %>% 
  st_as_sf()

#normalized number of inferred homes
hm_same_users <- hm_all %>% 
  filter(u_id %in% same_hm_users) %>% 
  dplyr::select(-name) %>% 
  unique()

norm_home_users <- hm_same_users %>% 
  group_by(home) %>% 
  summarise(n_users_home = n_distinct(u_id)) %>% 
  mutate(norm_n_users_home = (n_users_home - min(n_users_home))/(max(n_users_home)-min(n_users_home))) %>%
  left_join(grids %>% filter(grid_id %in% considered_grid_ids), ., by = c("grid_id" = "home")) %>% 
  replace(., is.na(.), 0) %>% 
  st_as_sf() 
```

### Residents in Singapore 2015

``` r
#residents in Singapore 2015
pop2015 <- st_read(here("analysis/data/raw_data/PLAN_BDY_DWELLING_TYPE_2015.shp"), quiet = T) %>%
  st_transform(., crs = 3414) %>% 
  st_make_valid()
norm_pop2015 <- pop2015 %>% 
  st_join(norm_home_users, .) %>% 
  group_by(PLN_AREA_N, TOTAL) %>% 
  summarise() %>% 
  ungroup() %>% 
  mutate(norm_total = (TOTAL - min(TOTAL))/(max(TOTAL) - min(TOTAL)))
```

### Geographical distribution of number of tweets (normalized)

``` r
map_view <- function(grids, df_normalized, var_fill, df_central, legend.nm, breaks,  show_central = F){
  tm_basic <- tm_shape(grids) + 
    tm_borders(col = "grey") + 
    tm_shape(df_normalized) +
    tm_fill(var_fill, 
            palette = "YlOrRd",
            style = "fixed",
            breaks = breaks,
            legend.hist = TRUE,
            title = legend.nm)
  
  tm_adjust <- tm_layout(title.position = c("left", "top"),
            title.size = 0.7,
            legend.outside = F,
            legend.position = c("right", "bottom"),
            legend.title.size  = 0.7,
            legend.text.size = 0.55,
            legend.hist.height = 0.1,
            legend.hist.width = 0.3,
            legend.hist.size = 0.5)
  if(show_central){
    tm_basic + 
      tm_shape(df_central) + 
      tm_borders(col = "black", lty = 2) +
      tm_adjust
  } else{
    tm_basic +
      tm_adjust
  }
}
```

``` r
map_view(grids, norm_tweets, var_fill = "norm_n_tweets", legend.nm = "Normalized # of tweets", breaks = c(0, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9, 1.0))
```

<img src="02-figures-spatial-view-of-same-hm-users_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

### Geographical distribution of number of unique users (normalized)

``` r
map_view(grids, norm_users, var_fill = "norm_n_users", df_central_region, legend.nm = "Normalized # of unique users", show_central = T, breaks = c(0, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9, 1.0))
```

<img src="02-figures-spatial-view-of-same-hm-users_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

### Geographical distribution of number of inferred home locations (normalized)

``` r
map_view(grids, norm_home_users, var_fill = "norm_n_users_home", legend.nm = "Normalized # of inferred homes", breaks = c(0, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9, 1.0))
```

<img src="02-figures-spatial-view-of-same-hm-users_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

### Geographical distribution of number of residents

``` r
map_view(grids, norm_pop2015, var_fill = "norm_total", legend.nm = "Normalized # of residents", breaks = c(0, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9, 1.0))
```

<img src="02-figures-spatial-view-of-same-hm-users_files/figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />
