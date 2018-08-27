
# list of buffers polygons at different lenght

st_multibuffer <- function(data=x, from, to, by, nQuadSegs = 30) {
# get a multi buffered polygon. Require an sf object
  library(sf)
  library(dplyr)
  
  seq.buffer <-seq(from,to, by) # allocate a  vector of length equal to number of buffer required
  
  if(inherits(x,"sf")) {
    
    # create a list of buffers 
    k <- vector('list', length(seq.buffer))
    for (i in 1:length(seq.buffer)) {
      k[[i]] <- st_buffer(x =x, dist = seq.buffer[i], nQuadSegs = nQuadSegs)
      k[[i]]$idx <- i
      k[[i]]$distance <- seq.buffer[i]
      st_agr(k[[i]]) <- "constant"
    }

    # clip from the bigger to the lower 
    
    l <- vector('list', length(seq.buffer))
    
    for (i in length(seq.buffer):1) {
      
      if(i==1){
        l[[i]] <- st_difference(k[[i]], st_geometry(x))
      } 
      else {
        l[[i]] <- st_difference(k[[i]], st_geometry(k[[i - 1]]))
      }
    }
    # Join all multipolygon
    
    if (length(seq.buffer) == 2) {
      temp <- rbind(l[[1]], l[[2]])
    } else {
      temp <- rbind(l[[1]], l[[2]])
      for (m in 3:length(seq.buffer)) {
       temp <- rbind(temp, l[[m]])
      }
    }
    
    return(temp)
    
  } else {
    
    stop("x is not a sf object")
    
  }
  
}
