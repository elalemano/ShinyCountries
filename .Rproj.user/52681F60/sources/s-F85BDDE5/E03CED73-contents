namedListToFrame <- function(l){
  
  df <- matrix(l %>% unlist, nrow = 1, ncol = length(l)) %>% as.data.frame(stringsAsFactors = F)
  
  names(df) <- names(l)
  
  return(df)
}