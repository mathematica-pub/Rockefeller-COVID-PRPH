#################################################################################
#################################################################################
# CREATE BASE SHAPEFILES TO SAVE TO DATA FOLDER
#################################################################################
#################################################################################
library(dplyr)
library(maptools)
library(raster)
library(readxl)
library(rgdal)
library(rgeos)
library(rmapshaper)
library(sf)
library(sp)
library(stringi)


# This program should be run once in advance of using the dashboard.
# The program will result in shapefiles saved to the data folder that
# will be loaded by the dashboard app.


# accent_drop -----------------------------------------------------------------
accent_drop_string <- function(x){
  stringi::stri_trans_general(str = x, id = "Latin-ASCII")
}


# pull geo features
iso_df <- read_xlsx(here::here("data", "geography-iso-list.xlsx"))

get_shape <- function(geo_level,
                      region,
                      subregion_1,
                      subregion_2,
                      subregion_3,
                      all = FALSE) {
  
  # create a map for the geography of interest
  # Region: create a map of the continent (or countries) with country as the lowest geographic resolution
  # Subregion 1 (country): create a map fo the country with state/province as the lowest geographic resolution
  # Subregion 2 (state/province): create a map of the country containing the state/province, with state/province
  #         as the lowest level of geographic resolution
  # Subregion 3 (district): create a map of the state containing the district, with state as the lowest
  #         level of geographic resolution
  
  # Required arguments:
  #' Region - region only
  #' subregion 1 - subregion 1 only
  #' subregion 2 - subregion 1, subregion 2
  #' subregion 3 - subregion 1, subregion 3
  #' Note: subregion 1 is used to find the iso code to download the shapefile
  
  # read in the list of iso codes by country and region
  if (geo_level == "region") {
      iso_list <-
        iso_df %>% 
        filter(Region == !!region) %>%
        pull(iso) # populate vector with list of appropriate iso codes
  } # end pull iso codes for the full region
  else{
    iso_list <- 
      iso_df %>% 
      filter(Country == !!subregion_1) %>%
      pull(iso) # populate vector with list of appropriate iso codes
  } # end pull geo specific iso code
  
  if(geo_level == "region") {

    map <- 
      sf::st_as_sf(
        raster::getData(
          name     = "GADM", 
          country  = iso_list[1], # pull countries one at a time
          level    = 0,  # only include country shape (not subregions)
          download = TRUE, 
          path     = here::here("data", "temp")))
        
    # sf updates s3 method for base::rbind()
    # https://r-spatial.github.io/sf/reference/bind.html
    
    if (length(iso_list) > 1) {
      for (i in 2:length(iso_list)) {
        
        map <-
          rbind(
            map,
            sf::st_as_sf(
              raster::getData(
                name     = "GADM", 
                country  = iso_list[i], # pull countries one at a time
                level    = 0,  # only include country shape (not subregions)
                download = TRUE, 
                path     = here::here("data", "temp"))))
          
      } # end loop through iso country codes
    } #end length(iso_list) > 1
    
    map <- 
      map %>% 
      rmapshaper::ms_simplify(., keep = 0.0025) %>%
      sf::st_transform(., 4326)
    
  } # end region processing
  
  else if(geo_level == "country") {
    
    map <- 
      sf::st_as_sf(
        raster::getData(
          name     = "GADM", 
          country  = iso_list[1], # pull countries one at a time
          level    = 0,  # only include country shape (not subregions)
          download = TRUE, 
          path     = here::here("data", "temp"))) %>%
      rmapshaper::ms_simplify(., keep = 0.0025) %>%
      sf::st_transform(., 4326)
    
  } # end country processing
  
  else if(geo_level == "sub-national") {
    
    map <- 
      sf::st_as_sf(
        raster::getData(
          name     = "GADM", 
          country  = iso_list[1], # pull countries one at a time
          level    = 1, # only include country shape (not subregions)
          download = TRUE, 
          path     = here::here("data", "temp"))) %>%
      rmapshaper::ms_simplify(., keep = 0.0025) %>%
      sf::st_transform(., 4326)
    
  } # end sub-national processing
  
  else if(geo_level == "district") {
    
    map <- 
      sf::st_as_sf(
        raster::getData(
          name     = "GADM", 
          country  = iso_list[1], # pull countries one at a time
          level    = 2, # include districts
          download = TRUE, 
          path     = here::here("data", "temp"))) %>%
      rmapshaper::ms_simplify(., keep = 0.0025) %>%
      sf::st_transform(., 4326)
    
    
    # optionally subset to a single state
    if (all == FALSE) {
      
      # select the state containing the district
      state_name <- dplyr::filter(map, NAME_2 == c(subregion_3)) %>% pull(NAME_1) 
      
      # select only the state of interest
      map <- dplyr::filter(map, NAME_1 == c(state_name))
    }
    
  } # end district processing
  
  return(map)
  
}


# region data frame
sf_lvl_0 <- NULL

regions <- c("Africa", "India", "Latin America")

for (name in c(regions)) {
  
  map <-
    get_shape(
      geo_level = "region",
      region    = c(name)
    )
  
  sf_lvl_0 <- 
    rbind(
      sf_lvl_0,
      map %>%
        dplyr::mutate(
          OBJECTID = NA,
          REGION   = c(name)
        ) %>%
        dplyr::rename("NAME_0" = "NAME_ENGLISH") %>%
        mutate_at(c("NAME_0"), accent_drop_string) %>%
        dplyr::select(OBJECTID, REGION, ISO, NAME_0, geometry)
    )
  
}


# sub-national data frame
sf_lvl_1 <- NULL

regions   <- c("Latin America", "Africa",  "India", "Africa")
countries <- c("Brazil",        "Nigeria", "India", "South Africa")

for (x in 1:length(countries)) {
  
  region <- regions[x]
  name   <- countries[x]
  
  map <-
    get_shape(
      geo_level   = "sub-national",
      subregion_1 = name
    )
    
  if (name %in% c("Nigeria")) {
    map$NAME_1[map$NAME_1 == "Nassarawa"] <- "Nasarawa"
  }
    
  sf_lvl_1 <- 
    rbind(
      sf_lvl_1,
      map %>%
        dplyr::mutate(
          OBJECTID = NA,
          REGION   = c(region)
        ) %>%
        mutate_at(c("NAME_0", "NAME_1"), accent_drop_string) %>%
        dplyr::select(OBJECTID, REGION, ISO, NAME_0, NAME_1, geometry)
    )

}


# district data frame
map <- get_shape(
         geo_level   = "district",
         subregion_1 = "India", 
         all         = TRUE
       )

sf_lvl_2 <- 
  map %>%
  dplyr::mutate(
    OBJECTID = NA,
    REGION   = "India"
  ) %>%
  mutate_at(c("NAME_0", "NAME_1", "NAME_2"), accent_drop_string) %>%
  dplyr::select(OBJECTID, REGION, ISO, NAME_0, NAME_1, NAME_2, geometry)




# separate processing for united states
# additional steps to move alaska and hawaii below the continental US
# based on 
# https://www.storybench.org/how-to-shift-alaska-and-hawaii-below-the-lower-48-for-your-interactive-choropleth-map/
map <-
  raster::getData(
    name     = "GADM", 
    country  = "United States",
    level    = 1, 
    download = TRUE, 
    path     = here::here("data", "temp"))

# move alaska and hawaii
map_aes <- spTransform(map, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
alaska <- map_aes[map_aes$NAME_1=="Alaska",]
alaska <- elide(alaska, rotate=-39)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-3700000, -2000000))
proj4string(alaska) <- proj4string(map_aes)
# alaska in ESRI is split into two shapes. After moving the shapes further south, merge
# into one
alaska_combine <- gUnaryUnion(alaska)
alaska_name <- as.data.frame(list(OBJECTID = "3",
                                  ID_0 = "244",
                                  ISO = "USA",
                                  NAME_0 = "United States",
                                  ID_1 = "2", 
                                  NAME_1 = "Alaska", 
                                  HASC_1 = "US.AK", 
                                  CCN_1 = NA, 
                                  CCA_1 = "",
                                  TYPE_1 = "State",
                                  ENGTYPE_1 = "State",
                                  NL_NAME_1 = "",
                                  VARNAME_1 = "AK|Alaska"
                                  ))
alaska_combine <- SpatialPolygonsDataFrame(alaska_combine, alaska_name)
hawaii <- map_aes[map_aes$NAME_1=="Hawaii",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(3800000, -1000000))
proj4string(hawaii) <- proj4string(map_aes)
map_aes <- map_aes[!map_aes$NAME_1 %in% c("Alaska", "Hawaii"),] # Remove old AK and HI

# pull Puerto Rico from GADM to include in the US map
pr_map <- raster::getData(country = "Puerto Rico", level = 0,
                          download=TRUE, 
                          path=here::here("data", "temp"))
# add column to correspond to state names
pr_map$NAME_1 <- pr_map$NAME_ENGLISH

map_aes <- rbind(map_aes, alaska_combine, hawaii) # Add in shifted AK and HI
map_aes2 <- spTransform(map_aes, proj4string(map))
# add PR polygon
map_aes2 <- raster::bind(map_aes2, pr_map)

# create country (& region) level map by removing state borders from
# modified US map
map_aes2_noborder <- gUnaryUnion(map_aes2)
# convert spatial polygons object to spatial
# polygons dataframe object with ID that matches
# other region/country level maps
us_name <- as.data.frame(list(NAME_EN = "United States"))
map_aes2_noborder <- SpatialPolygonsDataFrame(map_aes2_noborder, us_name)



# append US to country-level data frame
sf_lvl_0 <- 
  rbind(
    sf_lvl_0,
      sf::st_as_sf(map_aes2_noborder) %>%
      dplyr::mutate(
        OBJECTID = NA,
        REGION   = "United States",
        ISO      = "USA",
        NAME_0   = "United States"
      ) %>%
      mutate_at(c("NAME_0"), accent_drop_string) %>%
      dplyr::select(OBJECTID, REGION, ISO, NAME_0, geometry) %>%
      rmapshaper::ms_simplify(., keep = 0.0025) %>%
      sf::st_transform(., 4326)
  )


# append US to sub-national data frame
sf_lvl_1 <-
  rbind(
    sf_lvl_1,
      sf::st_as_sf(map_aes2) %>%
      dplyr::mutate(
        OBJECTID = NA,
        REGION   = "United States",
        ISO      = "USA",
        NAME_0   = "United States"
      ) %>%
      mutate_at(c("NAME_0", "NAME_1"), accent_drop_string) %>%
      dplyr::select(OBJECTID, REGION, ISO, NAME_0, NAME_1, geometry) %>%
      rmapshaper::ms_simplify(., keep = 0.0025) %>%
      sf::st_transform(., 4326)
  )




# output
saveRDS(
  object = sf_lvl_0 %>% mutate(OBJECTID = dplyr::row_number()), 
  file   = here::here("data", "shapefiles", "lvl-0-country.Rds")
)

saveRDS(
  object = sf_lvl_0 %>% mutate(OBJECTID = dplyr::row_number()) %>% st_cast(., "MULTILINESTRING"), 
  file   = here::here("data", "shapefiles", "lvl-0-country-linestring.Rds")
)

saveRDS(
  object = sf_lvl_1 %>% mutate(OBJECTID = dplyr::row_number()), 
  file   = here::here("data", "shapefiles", "lvl-1-sub-national.Rds")
)

saveRDS(
  object = sf_lvl_1 %>% mutate(OBJECTID = dplyr::row_number()) %>% st_cast(., "MULTILINESTRING"), 
  file   = here::here("data", "shapefiles", "lvl-1-sub-national-linestring.Rds")
)

saveRDS(
  object = sf_lvl_2 %>% mutate(OBJECTID = dplyr::row_number()), 
  file   = here::here("data", "shapefiles", "lvl-2-district.Rds")
)

saveRDS(
  object = sf_lvl_2 %>% mutate(OBJECTID = dplyr::row_number()) %>% st_cast(., "MULTILINESTRING"), 
  file   = here::here("data", "shapefiles", "lvl-2-district-linestring.Rds")
)

