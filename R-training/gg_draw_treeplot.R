

##
## gg_showplot() - show circular plot boundaries on x,y tree map ######
##

## !! For testing only 
# center = c(0,0)
# radius = 8
# n = 100
## !!

gg_draw_treeplot <- function(center, vec_radius, n){
  
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

gg_treeplot_center <- gg_showplot(center = c(0, 0), vec_radius = c(8, 16), n = 100)

gg_treeplot_all <- list(
  ## A
  gg_treeplot_center,
  ## B
  gg_showplot(center = c(0, 60),vec_radius = c(8, 16), n = 100),
  ## C
  gg_showplot(center = c(0, 120),vec_radius = c(8, 16), n = 100),
  ## D
  gg_showplot(center = c(0, 180),vec_radius = c(8, 16), n = 100),
  ## E
  gg_showplot(center = c(60, 0),vec_radius = c(8, 16), n = 100),
  ## F
  gg_showplot(center = c(120, 0),vec_radius = c(8, 16), n = 100),
  ## G
  gg_showplot(center = c(180, 0),vec_radius = c(8, 16), n = 100),
  coord_fixed(),
  theme_bw()
)


