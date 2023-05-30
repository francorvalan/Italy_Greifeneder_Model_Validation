library(stars)
library(sf)
library(mapview)
library(data.table)
#methods(class="stars")
#install.packages("starsdata", repos = "http://pebesma.staff.ifgi.de", type = "source")
Stations<- sf::read_sf("./02_Data/03_Vector_data/Station_coordinates.shp")

Pred_20m_map <- list.files("./02_Data/05_Raster_predictions/",
                       pattern = ".tif$",full.names = T)

raster_stack = read_stars(unlist(Pred_20m_map),NA_value =0)

# Set names in time format = "%Y%m%d_%H%M%OS"
names(raster_stack)<- unlist(lapply(strsplit(names(raster_stack),"_"),function(x){
  paste(strsplit(x," ")[c(2,3)],collapse = "_")
}))


time_values <- as.POSIXct(names(raster_stack), tz = "UTC", format = "%Y%m%d_%H%M%OS")

extracted_values <- st_extract(raster_stack, Stations)

# Add Stations attributes
extracted_values<- st_intersection(Stations,extracted_values)

extracted_values_df<- data.table(extracted_values)
id_melt<- c("Station_na","Land_use","Aspect","Latitude","Longitude",
            "Altitude","Altitude_a","geometry")
extracted_values_df <- melt(extracted_values_df,id.vars = id_melt)
#t(extracted_values_df)
names(extracted_values_df)[names(extracted_values_df)=="variable"] <- "Date"
extracted_values_df[extracted_values_df$value==0,"value"] <- NA
names(extracted_values_df)[names(extracted_values_df)=="value"] <- "Pred_20m_map"
extracted_values_df$Date <- gsub("X","",extracted_values_df$Date)
extracted_values_df$Pred_20m_map <- extracted_values_df$Pred_20m_map /100
extracted_values_df$geometry <- NULL

output_dir <- "./02_Data/04_SMC_Estimations/get_map"
if(!dir.exists(output_dir)) dir.create(output_dir)

write.csv(extracted_values_df,file.path("./02_Data/04_SMC_Estimations/get_map/","SMC_20m_map.csv"))

mapviewOptions(fgb = FALSE)
mapview::mapView((extracted_values))



output <- ("./02_Data/04_SMC_Estimations/pysmm_get_map")
if(!dir.exists(output)) dir.create(output)

write.csv(extracted_values_df,file.path(output,"extracted_values_df_20m_footprint.csv"))
