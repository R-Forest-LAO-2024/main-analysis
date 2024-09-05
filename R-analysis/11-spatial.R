

##
## Add LAO boundaries
##

#geodata::gadm(country = "LAO", level = 0, path = "data", )

sf_lao <- st_read("data/gadm/gadm41_LAO_0.json")
sf_lao 


ggplot() +
  geom_sf(data = sf_lao)

names(tree)
