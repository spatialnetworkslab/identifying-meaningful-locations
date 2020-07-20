Identifying home locations for users
================
Chen Qingqing
7/20/2020

## Identifying home locations for users

To identify home locations for users, we use four built-in “recipes” in
[homelocator](https://github.com/spatialnetworkslab/homelocator)
package.

### Load de-identified dataset

``` r
df <- readRDS(here("analysis/data/derived_data/data_anonymized.rds"))
```

### Recipe: APDM

Recipe ‘APDM’ (Rein Ahas et al., 2010) calculates both the average and
standard deviation timestamps in each location for each user.

#### Generate grid neighbors

``` r
#generate grid neighbors 
grids <- readRDS(here("analysis/data/derived_data/grid_750.rds"))
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
neighbors <- st_queen(grids)

#convert list to tibble
list_to_tibble <- function(index, neighbors){
  tibble(grid_id = as.character(index)) %>% 
    mutate(neighbor = list(neighbors[[index]]))
}
df_neighbors <- do.call(rbind, map(1:length(neighbors), function(x) list_to_tibble(x, neighbors)))
```

The neighbors dataset is in `analysis/data/derived_data`.

``` r
saveRDS(df_neighbors, here("analysis/data/derived_data/neighbors.rds"))
```

``` r
#must load neighbors when using APDM recipe 
df_neighbors <- readRDS(here("analysis/data/derived_data/neighbors.rds"))
hm_apdm <- identify_location(df, user = "u_id", timestamp = "created_at", location = "grid_id", 
                             tz = "Asia/Singapore", keep_score = F, recipe = "APDM")
```

### Recipe: FREQ

Recipe ‘FREQ’ simply selects the most frequently ‘visited’ location as
users’ home locations.

``` r
hm_freq <- identify_location(df_anonymized, user = "u_id", timestamp = "created_at", tz = "Asia/Singapore", location = "grid_id", show_n_loc = 1, recipe = "FREQ")
```

### Recipe: HMLC

Recipe ‘HMLC’ weighs data points across multiple time frames to ‘score’
potentially meaningful locations.

``` r
hm_hmlc <- identify_location(df, user = "u_id", timestamp = "created_at", location = "grid_id", 
                             tz = "Asia/Singapore", show_n_loc = 1, keep_score = F, recipe = "HMLC")
```

### Recipe: OSNA

Recipe ‘OSNA’ (Efstathiades et al., 2015), only considers data points
sent on weekdays and divides a day into three time frames - ‘rest time’,
‘leisure time’ and ‘active time’. The algorithm finds the most ‘popular’
location during ‘rest’ and ‘leisure’ time as the home locations for
users.

``` r
hm_osna <- identify_location(df, user = "u_id", timestamp = "created_at", location = "grid_id", 
                             tz = "Asia/Singapore", show_n_loc = 1, recipe = "OSNA")
```

The identified homes are in `analysis/data/derived_data`.

``` r
saveRDS(hm_apdm, file = here("analysis/data/derived_data/hm_apdm.rds"))
saveRDS(hm_freq, file = here("analysis/data/derived_data/hm_freq.rds"))
saveRDS(hm_hmlc, file = here("analysis/data/derived_data/hm_hmlc.rds"))
saveRDS(hm_osna, file = here("analysis/data/derived_data/hm_osna.rds"))
```