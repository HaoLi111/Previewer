get_data_names = function(...){
  data(...)$results[,3]
}


#sort(get_data_names())
get_visualization_methods = function(){
  c("Pairwise Plots",
    "3D Pairwise Plots",
    "ParallelCoordinate/Andrew\'s Plot",
    "1D_Summary",
    "")
}
