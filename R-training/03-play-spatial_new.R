

## Load libraries, install if needed
if (!require(sf)) install.packages("sf")
if (!require(terra)) install.packages("terra")
if (!require(sf)) install.packages("tmap")
if (!require(sf)) install.packages("tidyverse")

library(sf)
library(terra)
library(tmap)
library(tidyverse)

## Set theme for all graphs
theme_set(theme_bw())

##
## make spatial objects ######
##

plot <- read_csv("data/plot.csv")
treeplot <- read_csv("data/treeplot.csv")

sf_lao <- st_read("data/gadm/gadm41_LAO_0.json")

sf_plot <- plot |> 
  st_as_sf(coords = c("plot_gps_lon", "plot_gps_lat"), crs = 4326)

## !! EX
## + What are the column for lat/lon coordinates in "treeplot"? Use names()
## + Create sf_treeplot based on treeplot.
## !!

##
## Basic sf plot ######
##

ggplot() +
  geom_sf(data = sf_plot) +
  geom_sf(data = sf_lao)

## TIP: ggplot stack geometries, place the one on top at the end
ggplot() +
  geom_sf(data = sf_lao) +
  geom_sf(data = sf_plot)

## TIP: add transparency
ggplot() +
  geom_sf(data = sf_lao, fill = NA) +
  geom_sf(data = sf_plot)

## TIP: Zoom in
ggplot() +
  geom_sf(data = sf_lao, fill = NA) +
  geom_sf(data = sf_plot) +
  coord_sf(xlim = c(103, 104), ylim = c(18, 20))

## New plot with treeplots
sf_treeplot |>
  filter(plot_no == 3) |>
  ggplot() +
  geom_sf()


## !! EX 
## Make treeplot maps for plot 6 and 26
## !!



## Extract Raster info 


## Add tree positioning


##
## Interactive maps ######
##

sf_treeplot <- treeplot |> 
  st_as_sf(coords = c("treeplot_gps_lon", "treeplot_gps_lat"), crs = 4326)

tmap_mode("view")

tm_basemap("Esri.WorldImagery") +
  tm_shape(sf_lao) +
  tm_borders(col = "red") +
  tm_shape(sf_treeplot) +
  tm_symbols(fill = "treeplot_no", size = 0.5)

## !! EX
## + What spatial data would you like to visualize?
## !!

