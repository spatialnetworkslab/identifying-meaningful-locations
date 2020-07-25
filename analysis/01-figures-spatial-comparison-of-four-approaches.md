Spatial comparison of four approaches
================

## Load dataset

### Load grid cells

``` r
#grids 
grids <- st_read(here("analysis/data/derived_data/spatial_hex_grid.shp"), quiet = T) %>% 
  st_transform(crs = 3414)
head(grids)
```

    ## Simple feature collection with 6 features and 1 field
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 15792.54 ymin: 15315.71 xmax: 18417.54 ymax: 17480.77
    ## CRS:            EPSG:3414
    ##   grid_id                       geometry
    ## 1       1 POLYGON ((17292.54 15965.23...
    ## 2       2 POLYGON ((16917.54 16614.75...
    ## 3       3 POLYGON ((17667.54 16614.75...
    ## 4       4 POLYGON ((15792.54 17264.27...
    ## 5       5 POLYGON ((16542.54 17264.27...
    ## 6       6 POLYGON ((17292.54 17264.27...

### Load de-identified dataset

``` r
#de-identified dataset 
df <- read_csv(here("analysis/data/derived_data/deidentified_data.csv"))
head(df)
```

    ## # A tibble: 6 x 3
    ##       u_id created_at          grid_id
    ##      <dbl> <dttm>                <dbl>
    ## 1 33677220 2016-03-24 07:01:53     478
    ## 2 83769538 2014-01-27 07:10:07    1783
    ## 3 38521033 2013-06-16 05:21:43     949
    ## 4 88243722 2012-07-23 02:33:23    1295
    ## 5 41751665 2013-07-06 01:27:50     523
    ## 6 36532412 2013-10-12 18:46:27     634

### Load inferred home locations

``` r
#load inferred home locations of four approaches 
hm_apdm <- read_csv(here("analysis/data/derived_data/hm_apdm.csv")) %>% mutate(name = "APDM")
hm_freq <- read_csv(here("analysis/data/derived_data/hm_freq.csv")) %>% mutate(name = "FREQ")
hm_hmlc <- read_csv(here("analysis/data/derived_data/hm_hmlc.csv")) %>% mutate(name = "HMLC")
hm_osna <- read_csv(here("analysis/data/derived_data/hm_osna.csv")) %>% mutate(name = "OSNA")
hm_all <- bind_rows(hm_apdm, hm_freq, hm_hmlc, hm_osna)
head(hm_all)
```

    ## # A tibble: 6 x 3
    ##       u_id  home name 
    ##      <dbl> <dbl> <chr>
    ## 1  3191716   849 APDM 
    ## 2  4408429  1852 APDM 
    ## 3 71391177  1126 APDM 
    ## 4 18690315  1067 APDM 
    ## 5 19578598  1475 APDM 
    ## 6 64822321   759 APDM

### Calculate odds ratio

``` r
cal_OR <- function(df_hm, df, grids){
  #number of users with inferred homes at each grid
  df_locals_grid <- df_hm %>% 
      group_by(home) %>% 
      summarise(n_locals_grid = n_distinct(u_id))
  
  #grids that have fewer than 5 home users 
  grids_fewer5hm <- df_locals_grid %>% 
      filter(n_locals_grid < 5) %>% 
      pull(home)
  
  #users in grids that have fewer than 5 home users 
  users_in_grids_fewer5hm <- df_hm %>% 
      filter(home %in% grids_fewer5hm) %>% 
      pull(u_id) 
  
  #remove those users from inferred home dataset 
  df_hm_updated <- df_hm %>% filter(!u_id %in% users_in_grids_fewer5hm)
  
  #number of users with inferred homes at each grid
  df_locals_grids <- df_hm_updated %>% 
    group_by(home) %>% 
    summarise(n_locals_grid = n_distinct(u_id))
  
  #remove those users from de-identified dataset 
  df_updated <- df %>% filter(!u_id %in% users_in_grids_fewer5hm)
  
  df_users_grids <- df_updated %>% 
    dplyr::select(u_id, grid_id) %>% 
    unique()
  
  #calculate Odds Ratio
  df_OR <- df_users_grids %>% 
    left_join(., (df_hm_updated %>% select(-name)), by = c("u_id" = "u_id")) %>% 
    replace(., is.na(.), 0) %>% 
    mutate(type = if_else(grid_id == home, "local", "visitor")) %>% 
    group_by(grid_id, type) %>% 
    dplyr::summarise(n_user = n_distinct(u_id)) %>% 
    spread(key = "type", value = "n_user") %>% 
    replace(., is.na(.), 0) %>% 
    ungroup() %>% 
    filter(grid_id %in% df_hm_updated$home) %>% 
    mutate(total_local = sum(local), 
           total_visitor = sum(visitor)) %>% 
    mutate(OR = (local/total_local)/(visitor/total_visitor)) 
  return(df_OR)
}
```

The calculated odds ratios are in `analysis/data/derived_data`.

``` r
#APDM
if(file.exists(here("analysis/data/derived_data/OR_apdm.csv"))){
  OR_apdm <- read_csv(here("analysis/data/derived_data/OR_apdm.csv"))
} else{
  OR_apdm <- cal_OR(hm_apdm, df, grids)
  write_csv(OR_apdm, path = here("analysis/data/derived_data/OR_apdm.csv"))
}

#FREQ
if(file.exists(here("analysis/data/derived_data/OR_freq.csv"))){
  OR_freq <- read_csv(here("analysis/data/derived_data/OR_freq.csv"))
} else{
  OR_freq <- cal_OR(hm_freq, df, grids)
  write_csv(OR_freq, path = here("analysis/data/derived_data/OR_freq.csv"))
}

#HMLC
if(file.exists(here("analysis/data/derived_data/OR_hmlc.csv"))){
  OR_hmlc <- read_csv(here("analysis/data/derived_data/OR_hmlc.csv"))
} else{
  OR_hmlc <- cal_OR(hm_hmlc, df, grids)
  write_csv(OR_hmlc, path = here("analysis/data/derived_data/OR_hmlc.csv"))
}


#OSNA
if(file.exists(here("analysis/data/derived_data/OR_osna.csv"))){
  OR_osna <- read_csv(here("analysis/data/derived_data/OR_osna.csv"))
} else{
  OR_osna <- cal_OR(hm_osna, df, grids)
  write_csv(OR_osna, path = here("analysis/data/derived_data/OR_osna.csv"))
}

## convert to sf object
OR_apdm <- OR_apdm %>% left_join(., grids) %>% st_as_sf()
OR_freq <- OR_freq %>% left_join(., grids) %>% st_as_sf()
OR_hmlc <- OR_hmlc %>% left_join(., grids) %>% st_as_sf()
OR_osna <- OR_osna %>% left_join(., grids) %>% st_as_sf()
```

### Geospatial distribution of inferred home locations

``` r
spatial_view <- function(grids, df_OR, n = 8, method_nm, breaks){
  tm_shape(grids) +
  tm_borders(col = "grey") +
  tm_shape(df_OR) +
  tm_fill("OR", 
          palette = "YlOrRd",
          style = "fixed",
          breaks = breaks,
          legend.hist = TRUE,
          title = "Odds Ratio") +
  tm_layout(title = method_nm,
            title.position = c("left", "top"),
            legend.outside = F,
            legend.position = c("right", "bottom"),
            legend.hist.height = 0.1,
            legend.hist.width = 0.3,
            legend.hist.size = 0.5)
}
```

#### APDM

``` r
spatial_view(grids, OR_apdm, method_nm = "APDM", breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5))
```

![](01-figures-spatial-comparison-of-four-approaches_files/figure-gfm/apdm-1.png)<!-- -->

#### FREQ

``` r
spatial_view(grids, OR_freq, method_nm = "FREQ", breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5))
```

![](01-figures-spatial-comparison-of-four-approaches_files/figure-gfm/freq-1.png)<!-- -->

#### HMLC

``` r
spatial_view(grids, OR_hmlc, method_nm = "HMLC", breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5))
```

![](01-figures-spatial-comparison-of-four-approaches_files/figure-gfm/hmlc-1.png)<!-- -->

#### OSNA

``` r
spatial_view(grids, OR_osna, method_nm = "OSNA", breaks = c(0, 0.3, 0.5, 1, 3, 5, 7, 9))
```

![](01-figures-spatial-comparison-of-four-approaches_files/figure-gfm/osna-1.png)<!-- -->
