packages.used = c('tidytext', 'tidyverse', 'DT',
                  'htmlwidgets', 'plotly', 'RColorBrewer', 'stringr',
                  'geojsonio')

packages.needed = setdiff(packages.used,
                          intersect(installed.packages()[,1],
                                    packages.used))
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

# load packages
library(stringr)
library(tidytext)
library(tidyverse)
library(DT)
library(htmlwidgets)
library(plotly)
library(RColorBrewer)

library(geojsonio)
library(devtools)

# location data
# polygons
skate_parks <- geojsonio::geojson_read("../data/skate_parks.geojson", what = 'sp')
ath_facilities <- geojsonio::geojson_read("../data/ath_facility.geojson", what = 'sp')
dog_runs <- geojsonio::geojson_read("../data/dog_runs.geojson", what = 'sp')
comfort_st <- geojsonio::geojson_read("../data/comfort_stations.geojson", what = 'sp')
playgrounds <- read_csv("../data/playgrounds.csv")
adult_exer_equip <- read_csv("../data/adult_exer_equip.csv")

park_list <- list("adult_exer_equip" = adult_exer_equip,
     "playgrounds" = playgrounds,
     "skate_parks" = skate_parks@data,
     "dog_runs" = dog_runs@data,
     "comfort_st" = comfort_st@data,
     "ath_facilities" = ath_facilities@data
     )

for (i in 1:length(park_list)){
  park_list[[i]]["button"] <- names(park_list)[i]
  
  park_list[[i]] <- park_list[[i]] %>% 
    dplyr::rename_all(funs(str_replace_all(., "B", "b")))%>%
    mutate(borough = ifelse(borough == "B", "Brooklyn", 
                            ifelse(borough =="M", "Manhattan",
                                   ifelse(borough == "X", "Bronx",
                                          ifelse(borough == "Q", "Queens",
                                                 ifelse(borough == "R", "Staten Island", NA))))))
}


park_list$adult_exer_equip["content"] <- paste(sep = "<br/>",
                                              paste0(park_list$adult_exer_equip$PropName,", ",park_list$adult_exer_equip$borough,", NY"), 
                                              paste0("<b>Status: </b>",park_list$adult_exer_equip$Status),
                                              paste0("<b>Features: </b>", park_list$adult_exer_equip$FeatureType))

park_list$playgrounds["content"] <- paste(sep = "<br/>",
                                       paste0(park_list$playgrounds$location,", ",park_list$playgrounds$borough,", NY"), 
                                       paste0("<b>Status: </b>",park_list$playgrounds$Status),
                                       paste0("<b>Accessibility: </b>", park_list$playgrounds$accessibilityLevel))

park_list$ath_facilities["content"] <- paste(sep = "<br/>",
                                        paste0(park_list$ath_facilities$propertyname,", ",park_list$ath_facilities$borough,", NY"),
                                        paste0("<b>Status: </b>",park_list$ath_facilities$status),
                                        paste0("<b>Primary sport: </b>", park_list$ath_facilities$primarysport),
                                        paste0("<b>Surface type: </b>", park_list$ath_facilities$surfacetype))

park_list$dog_runs["content"] <- paste(sep = "<br/>",
                                   paste0(park_list$dog_runs$propertyname,", ",park_list$dog_runs$borough,", NY"),  
                                   paste0("<b>Status: </b>",park_list$dog_runs$status))

park_list$skate_parks["content"] <- paste(sep = "<br/>",
                                      paste0(park_list$skate_parks$propertyname,", ",park_list$skate_parks$borough,", NY"),
                                      paste0("<b>Status: </b>",park_list$skate_parks$status))


get_coords <- function(data) {
  data$point <- gsub("POINT |[()]", "", data$point)
  data<- data %>%
    tidyr::separate(point, c("longitude", "latitude"), " ") %>% 
    mutate(longitude = as.numeric(longitude),
           latitude = as.numeric(latitude))
  return(data)
}

get_poly <- function(geo){
  long_list <- list()
  lat_list <- list()
  for (i in 1:length(geo@polygons)){
    long_list[i] <- as.numeric(geo@polygons[[i]]@labpt[1])
    lat_list[i] <- as.numeric(geo@polygons[[i]]@labpt[2])
  }
  geo@data$longitude <- unlist(long_list)
  geo@data$latitude <- unlist(lat_list)
  return(geo)
}
  
  
park_list$playgrounds <- get_coords(park_list$playgrounds)
park_list$adult_exer_equip <- get_coords(park_list$adult_exer_equip)
skate_parks <- get_poly(skate_parks)
dog_runs <- get_poly(dog_runs)
comfort_st <- get_poly(comfort_st)
ath_facilities <- get_poly(ath_facilities)



### mapping



