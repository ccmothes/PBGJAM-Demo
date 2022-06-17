# ArcGIS ImageServer test

library(leaflet)
library(leaflet.extras)
library(leaflet.esri)


leaflet() %>%
  addEsriBasemapLayer(esriBasemapLayers$Gray) %>%
  setView(30, 0, 3) %>%
  addEsriImageMapLayer(
    url = "https://sampleserver3.arcgisonline.com/ArcGIS/rest/services/World/MODIS/ImageServer",
    options = imageMapLayerOptions(useCors = FALSE))


leaflet() %>%
  addEsriBasemapLayer(esriBasemapLayers$Gray) %>%
  setView(-96.8, 38.5, 4) %>%
  addEsriImageMapLayer(
    url = "https://seamlessrnc.nauticalcharts.noaa.gov/arcgis/rest/services/RNC/NOAA_RNC/ImageServer")

leaflet() %>%
  addEsriBasemapLayer(esriBasemapLayers$Imagery) %>%
  setView(-120.23, 43.5, 5)  %>%
  addEsriImageMapLayer(
    url = paste0(
      "http://imagery.oregonexplorer.info/arcgis/rest/services/", 
      "NAIP_2011/NAIP_2011_Dynamic/ImageServer"
    ),
    options = imageMapLayerOptions(bandIds = c(3, 0, 1)))


## leaflet.esri not working....

leaflet() %>% 
  addTiles() %>% 
  setView(lat = 39, lng = -86, zoom = 3.5) %>% 
  addWMSTiles(baseUrl = "https://tiledimageservices1.arcgis.com/KNdRU5cN6ENqCTjk/ArcGIS/rest/services/Beetlesmean_carabuGoryi_hist/ImageServer/WMSServer",
              layers = "Beetlesmean_carabuGoryi_hist", options = WMSTileOptions(format = "image/png", transparent = FALSE)
              )


#Try arcpullr

#read in current tif to get bounds
tif <- raster::raster("data/RCP45_2040_2069/R_SPQR_Agonoleptus.conjunctus.tif")

library(arcpullr)

token <- "PP0Ye9FY_8IXQj29GWpXmUOKGH5YJn7OFjrR0y9pAgYoy7L6xAcLZR2bNA4NZ3fcB0rppQWsJOtpO88spAFZt4Aon5jOGOtmDoPmkDzuMvPjMCof8UFz5NVEdqHUjAEnm5O4QiIn1WoLYx2ewy3Y1MDwo_tTQv7bxytk2jJmiK_UH9a-_2ZjjL129m09XYNB9FL5T-O0SFx3PoRapmUe-z0VELZS39yj2CivodnxNiV7m5PZgUHknlwta3ACp695"

test_url <- "https://tiledimageservices1.arcgis.com/KNdRU5cN6ENqCTjk/arcgis/rest/services/2019_06_02_dense_ortho_4cm_px/ImageServer"

test_bbox <- sf::st_bbox(tif) %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  st_transform(4326)
  


# doesn't work...error when trying to get extent from raster layer
test_img <- get_image_layer(url = "https://tiledimageservices1.arcgis.com/KNdRU5cN6ENqCTjk/arcgis/rest/services/Beetlesmean_carabuGoryi_hist/ImageServer",
                        clip_raster = FALSE,
                        sf_object = test_bbox)


get_service_type(url = "https://tiledimageservices1.arcgis.com/KNdRU5cN6ENqCTjk/ArcGIS/rest/services/Beetlesmean_carabuGoryi_hist/ImageServer")
# returns "f32has_layer"



# test with WI DNR Image Server data
# WDNR Server
image_server <- "https://dnrmaps.wi.gov/arcgis_image/rest/services/"

# WI Landcover Type URL
landcover_path <- "DW_Land_Cover/EN_Land_Cover2_Lev2/MapServer"
landcover_url <- paste0(image_server, landcover_path)

# WI Leaf-off Aerial Imagery URL
wi_leaf_off_path <- "DW_Image/EN_Image_Basemap_Leaf_Off/ImageServer"
wi_aerial_imagery_url <- paste0(image_server, wi_leaf_off_path)

wi_aerial_imagery <- get_image_layer(wi_aerial_imagery_url, wis_poly)
plot_layer(wi_aerial_imagery)

test <- get_image_layer(test_url, test_bbox) # doesn't work


# had to edit CRS to plot with leaflet
crs(wi_aerial_imagery) <- 4326

leaflet() %>% 
  addTiles() %>% 
  addRasterImage(wi_aerial_imagery[[1]], )

get_service_type(wi_aerial_imagery_url)


# read in from map server

url <- "https://tiles.arcgis.com/tiles/KNdRU5cN6ENqCTjk/arcgis/rest/services/B_mean_selenoPlanip_hist/MapServer/tile/{z}/{y}/{x}.png"
test <- get_map_layer(url, wis_poly)

leaflet() %>% 
  addTiles() %>% 
  addWMSTiles("https://tiles.arcgis.com/tiles/KNdRU5cN6ENqCTjk/arcgis/rest/services/B_mean_selenoPlanip_hist/MapServer/WMTS",
              layers = "0",
              options = WMSTileOptions(format = "image/png", transparent = TRUE))

leaflet() %>% 
  #addTiles() %>% 
  addTiles(urlTemplate = "https://tiles.arcgis.com/tiles/KNdRU5cN6ENqCTjk/arcgis/rest/services/B_mean_selenoPlanip_hist/MapServer/tile/{z}/{y}/{x}.png")

data("World")

## Interactive map
tmap_mode("view")
tm_shape(World) +
  tm_tiles(url, group = "LABELS") +
  tm_symbols(size = "gdp_cap_est")

gdw <- rosm::as.tile_source(url, extension = "png")


leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 7) %>%addWMSTiles(
  "http://basemap.nationalmap.gov/arcgis/services/USGSHydroNHD/MapServer/WMSServer?",
  layers = "0",
  options = WMSTileOptions(format = "image/png", transparent = TRUE),
  attribution = "") 


#test arcpullr function
export_url <- paste(url, "export", sep = "/")

