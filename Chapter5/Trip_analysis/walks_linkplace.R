# Define the walkability for each walk based on the link-place framework

# Load libraries
library(tidyverse)
library(leaflet)
library(sf)

# Open the link-place classified road network and the layer with urban paths
linkplace <- read_sf(dsn = "Z:/Spatial/LinkPlace/FinalLinkPlaceClassification_BNG.shp") %>% st_transform(32618) #Set to a projected coordinate system for planar operations
urbpaths <- read_sf(dsn = "Z:/Spatial/LinkPlace/UrbanPaths.shp") %>% st_transform(32618) 

# Function to create a buffer around the track of a walk
create_buffer <- function(walk_track){
  # Convert to one line
  walk_line <- walk_track[,1:2] %>% as.matrix() %>% st_linestring() %>% st_sfc(crs = 4326) %>% st_transform(32618)
  # Create buffer around it
  walk_line_buffer <- walk_line %>% st_buffer(dist = 25)
  walk_line_buffer
}


# Load trip data and select all walk triplegs
trips <-  read.csv("Z:/Trackdata/trips/trips.csv")
tripleg_ids <- trips %>% filter(mode == "Walk") %>% select(tripleg_id) %>% unlist()

# Prepare matrix for link-place lengths
LinkPlace_mat <- matrix(nrow=0, ncol=27, data=NA)
LinkPlace_mat <- as.data.frame(LinkPlace_mat)

# Select GPS points for all walks
for(tripleg_id in tripleg_ids){
  user <- trips[trips$tripleg_id == tripleg_id, "user_id"]
  from_id <- trips[trips$tripleg_id == tripleg_id, "from_id"]
  to_id <- trips[trips$tripleg_id == tripleg_id, "to_id"]
  
  # Read data for tracks
  data_tracks <- 
    list.files(path = "//its-rds/2017/tightmr-transport-walking/Trackdata/Location_users/processed/",
               pattern = paste0("^tracks_user",user,"_(.*)csv$")) %>%
    map_df(~read.csv(paste0("//its-rds/2017/tightmr-transport-walking/Trackdata/Location_users/processed/",.x))[,c(-5,-14, -21, -27, -33)])
  
  walk_track <- data_tracks %>% filter(id >= from_id & id <= to_id)
  if(nrow(walk_track) != 0){
    walk_line_buffer <- walk_track %>% create_buffer()
    
    # Create intersection (define which street sections and paths are within the buffer around the walking route)
    intersection <- st_intersection(linkplace, walk_line_buffer)
    intersection_urbpaths <- st_intersection(urbpaths, walk_line_buffer)
    
    # Calculate the length of these street and path sections
    intersection$Shape_Leng <- st_length(intersection)
    intersection_urbpaths$Shape_Leng <- st_length(intersection_urbpaths)
    
    rowValues <- tripleg_id
    for(l in 1:5){#link
      for(p in 1:5){#place
        if(nrow(intersection[intersection$Link_Class==l & intersection$Final_Plac==LETTERS[p], "Shape_Leng"]) != 0){
          rowValues <- c(rowValues, sum(st_set_geometry(intersection[intersection$Link_Class==l & 
                                                                       intersection$Final_Plac==LETTERS[p], "Shape_Leng"], NULL)))
        }else{
          rowValues <- c(rowValues, 0)
        }
      }
    }
    rowValues <- c(rowValues, sum(intersection_urbpaths$Shape_Leng))
    LinkPlace_mat <- rbind(LinkPlace_mat, rowValues)
  }
}

names(LinkPlace_mat) <- c("tripleg_id", "1A", "1B", "1C", "1D", "1E",
                          "2A", "2B", "2C", "2D", "2E", "3A", "3B", "3C", "3D", "3E",
                          "4A", "4B", "4C", "4D", "4E", "5A", "5B", "5C", "5D", "5E", "Urbpath")

LinkPlace_mat$total_length <- rowSums(LinkPlace_mat[,c(2:27)])

# Multiply all lengths with weights for each link-place category
LinkPlace_mat$index_length <- LinkPlace_mat$`1A` + LinkPlace_mat$`1B` * 0.8 + LinkPlace_mat$`1C` * 0.6 + LinkPlace_mat$`1D` * 0.4 + LinkPlace_mat$`1E` * 0.2 +
  LinkPlace_mat$`2A` * 1.2 + LinkPlace_mat$`2B` + LinkPlace_mat$`2C` * 0.8 + LinkPlace_mat$`2D` * 0.6 + LinkPlace_mat$`2E` * 0.4 +
  LinkPlace_mat$`3A` * 1.4 + LinkPlace_mat$`3B` * 1.2 + LinkPlace_mat$`3C` + LinkPlace_mat$`3D` * 0.8 + LinkPlace_mat$`3E` * 0.6 +
  LinkPlace_mat$`4A` * 1.6 + LinkPlace_mat$`4B` * 1.4 + LinkPlace_mat$`4C` * 1.2 + LinkPlace_mat$`4D` + LinkPlace_mat$`4E` * 0.8 +
  LinkPlace_mat$`5A` * 1.8 + LinkPlace_mat$`5B` * 1.6 + LinkPlace_mat$`5C` * 1.4 + LinkPlace_mat$`5D` * 1.2 + LinkPlace_mat$`5E` + LinkPlace_mat$Urbpath * 2

# Calculate LinkPlace index based on weighted length divided by actual length
LinkPlace_mat$index <- LinkPlace_mat$index_length / LinkPlace_mat$total_length



# Save
write.csv(LinkPlace_mat, "Z:/Spatial/LinkPlace/linkplace_walks.csv", row.names = F)
