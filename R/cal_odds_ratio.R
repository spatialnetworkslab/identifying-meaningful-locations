# Calculate Odds Ratio
cal_OR <- function(df_hm, df, grids){
  # if a grid has less than 5 identified users, remove users in that grid
  df_hm_updated <- df_hm %>%
    group_by(home) %>%
    mutate(n_locals_hm = n_distinct(u_id)) %>%
    ungroup() %>%
    filter(n_locals_hm >= 5) %>%
    dplyr::select(-n_locals_hm, -name)
  # users to remove
  rm_users <- setdiff(df_hm$u_id, df_hm_updated$u_id)

  # get users in each grid from tweets dataset
  df_users_grids <- df %>%
    filter(!u_id %in% rm_users) %>% # remove user tweets from dataset
    dplyr::select(u_id, grid_id) %>%
    distinct(u_id, grid_id, .keep_all = TRUE)

  #calculate Odds Ratio
  df_OR <- df_users_grids %>%
    left_join(., df_hm_updated, by = c("u_id" = "u_id")) %>%
    replace(., is.na(.), 0) %>%
    mutate(type = if_else(grid_id == home, "local", "visitor")) %>%
    group_by(grid_id, type) %>%
    dplyr::summarise(n_user = n_distinct(u_id)) %>%
    spread(key = "type", value = "n_user") %>%
    na.omit() %>%
    ungroup() %>%
    mutate(total_local = sum(local),
           total_visitor = sum(visitor)) %>%
    mutate(OR = (local/total_local)/(visitor/total_visitor))
  return(df_OR)
}
