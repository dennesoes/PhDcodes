# create_map --------------------------------------------------------------
# Function to create an interactive map with tracked location points
# Input:  - simple features cleaned dataset with tracking locations
#         - day to analyse
# Output: leaflet map object

create_map <- function(data_sf, day){

#Create the trackline, connecting all location points
lines <- SpatialLines(list(Lines(list(Line(as_Spatial(data_sf[data_sf$date==day,]))), "id")))


#Prepare colours for speed
pal <- colorNumeric(palette = c("green", "red"), domain = c(0,75), na.color = "red") 

#prepare labels and layer ids
labels1 <- lapply(seq(nrow(data_sf[data_sf$date==day & data_sf$timediff <= 180,])), function(i){ 
  paste0(data_sf[data_sf$date==day & data_sf$timediff <= 180, "time_BST"][i,1][[1]], "</br>",
         data_sf[data_sf$date==day & data_sf$timediff <= 180, "speed_kmph"][i,1][[1]], " km/h </br>",
         "ID: ",data_sf[data_sf$date==day & data_sf$timediff <= 180, "id"][i,1][[1]])})
ids1 <- lapply(seq(nrow(data_sf[data_sf$date==day & data_sf$timediff <= 180,])), function(i){
  data_sf[data_sf$date==day & data_sf$timediff <= 180, "id"][i,1][[1]]})
labels2 <- lapply(seq(nrow(data_sf[data_sf$date==day & data_sf$timediff > 180,])), function(i){
  paste0(data_sf[data_sf$date==day & data_sf$timediff > 180, "time_BST"][i,1][[1]], "</br>",
         data_sf[data_sf$date==day & data_sf$timediff > 180, "speed_kmph"][i,1][[1]], " km/h </br>",
         "ID: ",data_sf[data_sf$date==day & data_sf$timediff > 180, "id"][i,1][[1]])})
ids2 <- lapply(seq(nrow(data_sf[data_sf$date==day & data_sf$timediff > 180,])), function(i){
  data_sf[data_sf$date==day & data_sf$timediff > 180, "id"][i,1][[1]]})

#Create map and add tracklines and GPS-points
map <- leaflet(data = data_sf) %>% addTiles() %>% 
  addPolylines(data = lines, color = "#000", weight = 2, opacity = 0.5) %>%
  addCircleMarkers(data = data_sf[data_sf$date==day & data_sf$timediff <= 180,], group = day, layerId = ids1,
                   label = lapply(labels1,HTML), popup = lapply(labels1,HTML), radius = 2, opacity = 0.8, color = ~pal(speed_kmph)) %>%
  addCircleMarkers(data = data_sf[data_sf$date==day & data_sf$timediff > 180,], group = day, layerId = ids2,
                   label = lapply(labels2,HTML), popup = lapply(labels2,HTML), radius = 10, opacity = 0.8, color = ~pal(speed_kmph))
  
map
}
