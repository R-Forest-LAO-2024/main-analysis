

##
## gg_showplot() - show circular plot boundaries on x,y tree map ######
##

## !! For testing only 
# center = c(0,0)
# radius = 8
# n = 100
## !!

gg_showplot <- function(center, vec_radius, n){
  
  theta = seq(0, 2*pi, length = n)
  
  map(vec_radius, function(r){
    data_circle <- data.frame(
      theta = theta,
      x = center[1] + r * cos(theta),
      y = center[2] + r * sin(theta)
    )
    
    geom_path(data = data_circle, aes(x = x, y = y))
    
  })
  
}


