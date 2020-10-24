# prepare necessary functions for sensitivity analysis
## read results of different grid cell sizes
readfile <- function(file, grid.size){
  readRDS(file) %>% mutate(size = grid.size)
}

## count number of identified users for different grid cell sizes
count_identified_users <- function(df_hm){
  df_hm %>%
    group_by(size) %>%
    dplyr::summarise(n_user = n_distinct(u_id))
}

## get shared users (users has identified home location with all 6 grid cell sizes)
get_shared_users <- function(df_hm){
  df_hm %>%
    group_by(u_id) %>%
    dplyr::summarise(n_size = n_distinct(size)) %>%
    filter(n_size == 6)
}

# convert df to sf with shared users
convert2sf <- function(grid.size, df_grids, df_hm, shared_users){
  df_hm %>%
    filter(size == grid.size) %>%
    filter(u_id %in% shared_users$u_id) %>%
    mutate(home = as.numeric(home)) %>%
    left_join(., df_grids, by = c("home" = "grid_id")) %>%
    st_sf() %>%
    st_transform(crs = 3414)
}

# check intersects between two different grid cell sizes
check_intersects <- function(hm_sf, size1, size2){
  sf1 <- hm_sf %>% filter(size == size1)
  sf2 <- hm_sf %>% filter(size == size2)

  #check intersect by user
  cal_intersects <- function(user){
    st_intersects(sf1 %>% filter(u_id == user), sf2 %>% filter(u_id == user),
                  sparse = F) %>%
      as.logical()
  }

  sf1 %>%
    st_set_geometry(NULL) %>%
    left_join(., sf2 %>% st_set_geometry(NULL), by = c("u_id" = "u_id")) %>%
    as_tibble() %>%
    mutate(intersect = map_lgl(u_id, with_progress(cal_intersects)))
}

# check intersects across all 6 grid cell sizes
check_allsize_intersects <- function(hm_sf, hm_intersects){
  sf1 <- hm_sf %>% filter(size == 750)
  sf2 <- hm_sf %>% filter(size == 250)
  sf3 <- hm_sf %>% filter(size == 500)
  sf4 <- hm_sf %>% filter(size == 750)
  sf5 <- hm_sf %>% filter(size == 1000)
  sf6 <- hm_sf %>% filter(size == 1250)
  sf7 <- hm_sf %>% filter(size == 1500)

  ## users that have intersects with 750m grid across all other five grid sizes
  user_ids <- map(hm_intersects, function(x) filter(x, intersect == TRUE)) %>%
    map(., function(x) pull(x, u_id)) %>%
    Reduce(intersect, .)

  cal_intersects <- function(user){
    # get geometry in different grid size for the user
    sf1_user <- sf1 %>% filter(u_id == user)
    sf2_user <- sf2 %>% filter(u_id == user)
    sf3_user <- sf3 %>% filter(u_id == user)
    sf4_user <- sf4 %>% filter(u_id == user)
    sf5_user <- sf5 %>% filter(u_id == user)
    sf6_user <- sf6 %>% filter(u_id == user)
    sf7_user <- sf7 %>% filter(u_id == user)

    # check intersect
    overlap <- st_intersection(sf1_user, sf2_user)

    if(nrow(overlap) != 0){
      overlap <- st_intersection(overlap, sf3_user)
      if(nrow(overlap) != 0){
        overlap <- st_intersection(overlap, sf4_user)
        if(nrow(overlap) != 0){
          overlap <- st_intersection(overlap, sf5_user)
          if(nrow(overlap) != 0){
            overlap <- st_intersection(overlap, sf6_user)
            if(nrow(overlap) != 0){
              st_intersects(overlap, sf7_user, sparse = F) %>% as.logical()
            }else{
              return(FALSE)
            }
          }else{
            return(FALSE)
          }
        } else{
          return(FALSE)
        }
      }else{
        return(FALSE)
      }
    }else{
      return(FALSE)
    }
  }
  intersect <-  map_lgl(user_ids, with_progress(cal_intersects))
  tibble(u_id = user_ids, intersect = intersect)
}
