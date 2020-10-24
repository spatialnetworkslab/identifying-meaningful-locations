# visualize odds ratio
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
              legend.hist.height = 0.08,
              legend.hist.width = 0.3,
              legend.hist.size = 0.5)
}

## spatial distribution overview
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
