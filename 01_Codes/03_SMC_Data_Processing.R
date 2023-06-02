#####                       Italy SMC Data Processing

# Process realized in this script:
#   - 01. Ground Truth Data (GTD) from local stations in Italy are processing:
#       - Removing duplicated data,
#       - Data joined
#       - The output file is a csv with all the , writed in ""./03_Results/""

#   - 02. SMC Estimations processing

#       - Reading data and filtering by stations


# francisco.corvalan6@gmail.com
# franciscomartin.corvalan@eurac.edu



library(dplyr)                                    
#library(plyr)
library(fuzzyjoin)
library(readr)  
library(purrr)
library(tmap)
library(tmaptools)

#### Data visualization
# Locations site loading 

Catchment_area <- sf::read_sf("./02_Data/03_Vector_data/LTSER_IT25_Matsch_Mazia_Catchment.shp")
Stations<- sf::read_sf("./02_Data/03_Vector_data/Station_coordinates.shp")

tmap_mode("view")
# https://leaflet-extras.github.io/leaflet-providers/preview/
tm_basemap(c(Esri_WorldImagery ='https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
             StreetMap = "OpenStreetMap",
             Esri_WorldTopoMap ='https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',
             TopoMap = "OpenTopoMap"))+
  tm_shape(Stations)+tm_dots(col="Land_use",title= "Land use",border.col = "black",
                             scale=1,border.lwd = 1,size=0.05)+
  tm_text(text = "Station_na", bg.color="black", size = 1.5,ymod = 3,id="Station_na",
          bg.alpha =1, root=4, size.lowerbound = 10, shadow = T,print.tiny=T,
          auto.placement = 1)+
  tm_shape(Catchment_area) +
  tm_polygons(col = "lightblue",alpha=0.4,
              border.col = "gray15", lwd = 2, lty = "dashed")


####  01 Ground Truth Data (GTD) Processing ####

# List and files filter
SMC_files <- list.files("./02_Data/01_Original_SMC_data/Second_part/",pattern = ".csv$",full.names = T)
SMC_files <- grep("Wide",SMC_files,invert = T,value = T)

# Set csv configutation to read
local<- locale(
  date_names = "en",
  date_format = "%AD",
  time_format = "%AT",
  decimal_mark = ".",
  grouping_mark = ",",
  tz ="Europe/Rome",
  encoding = "UTF-8",
  asciify = FALSE
)

# Reading and joining all csv files, removing the fist row (slice(-1))
all_GTD <- map_dfr(SMC_files, ~ read_csv(.x,locale = local,na = c("", "NA","NaN")) %>% slice(-1))
all_GTD<- all_GTD %>% select(c("time","station","landuse","elevation","latitude","longitude",
                     "swc_wc_a_02_avg",	"swc_wc_a_05_avg",	"swc_wc_b_02_avg",	
                     "swc_wc_b_05_avg",	"swc_wc_c_02_avg","swc_wc_c_05_avg","swc_wc_02_avg","swc_wc_05_avg"))

# Write an open in order to read without problem in character & number (double)
write.csv(all_GTD,"./02_Data/02_Processed_data/Ground_truth_data.csv",row.names = F) 
all_GTD <- read_csv("./02_Data/02_Processed_data/Ground_truth_data.csv",
                    locale = local,na = c("", "NA","NaN"))

all_GTD
summary(all_GTD)
as_tibble(all_GTD)

# Finding duplicated rows
(n_duplicated <- nrow(all_GTD[duplicated(all_GTD),])) # 424

# Finding duplicates inside each file
SMC_files %>% 
  lapply(read_csv) %>%  
  lapply(function(x) sum(duplicated(x))) # there are not duplicated data inside the files...

SMC_files %>% 
  lapply(read_csv) %>%  
  lapply(function(x) unique(x$station)) # the 4 file has 6 stations,...

# Searching if the duplicated data are between the 4 file and the all_GTD
# df_4 <- read_csv(SMC_files[[4]],locale = local,delim=",")%>% slice(-1) # reading file 4

# nrow(intersect(all_GTD, df_4)) # 576, there are also one duplicated data,
# now we can know that all data from SMC_files[[4]] are already in other csv files

# removing the duplicated data
all_GTD <- all_GTD[!duplicated(all_GTD),]
sum(duplicated(all_GTD))



# There are anomalies in SMC messurements, SMC range are between 0:0.5
columns_with_SMC <- c("swc_wc_a_02_avg","swc_wc_b_02_avg","swc_wc_c_02_avg",
                      "swc_wc_a_05_avg","swc_wc_b_05_avg","swc_wc_c_05_avg",
                      "swc_wc_02_avg","swc_wc_05_avg")

all_GTD[, columns_with_SMC] <- apply(all_GTD[, columns_with_SMC], 2, function(x) {
  x[x > 0.55] <- NA
  x[x <= 0] <- NA
  return(x)
})
summary(all_GTD)
# Rounding in 
# all_GTD <- data.frame(lapply(all_GTD,    # Using Base R functions
#                   function(x) if(is.numeric(x)) round(x, 5) else x))
names(all_GTD)[grep("station",names(all_GTD))] <- "Station"
names(all_GTD)[grep("time",names(all_GTD))] <- "Date"
all_GTD$Station <- toupper(all_GTD$Station)
all_GTD$wc_05_av <- rowMeans(all_GTD[,c("swc_wc_a_05_avg","swc_wc_b_05_avg","swc_wc_c_05_avg","swc_wc_05_avg")],na.rm = T)
all_GTD$wc_02_av <- rowMeans(all_GTD[,c("swc_wc_a_02_avg","swc_wc_b_02_avg","swc_wc_c_02_avg","swc_wc_02_avg")],na.rm = T)

write.csv(all_GTD,"./02_Data/02_Processed_data/Ground_truth_data.csv",row.names = F) 

all_GTD <- read.csv("./02_Data/02_Processed_data/Ground_truth_data.csv")

all_GTD$Date <- as.POSIXct(all_GTD$Date,tz="Europe/Rome")

#all_GTD$Date <- as.POSIXct(all_GTD$Date,tz="Europe/Berlin") # Setting Europe tz


####################     02 SMC Estimations processing    ######################
# There are two types of estimations in pysmm, get_ts (estimations in points)
# and get_map that makes maps over an area. This predictios are then sampled
# in the Stations and the results of the samples were stored in 
# "./02_Data/04_SMC_Estimations/get_map/" for more information about 
# this sample see "./01_Codes/02_Pred_maps_data_extraction.R"

## Estimations with get_ts pysmm
SMC_ts_estimations <- list.files("./02_Data/04_SMC_Estimations/get_ts/",pattern = ".csv$",
          full.names = T,recursive = T)
SMC_ts_estimations <- grep("Superado",SMC_ts_estimations,invert = T,value = T)
# Filtering Stations according with the avaibility of ground truth data0
Stations<- Stations[Stations$Station_na%in%c(unique(all_GTD$Station)),]

# Filtering SMC Estimations by the stations with ground truth data availability
Stations_names <- toupper(unique(all_GTD$Station))
SMC_ts_estimations <- grep(paste(paste0((unique(all_GTD$Station)),collapse ="|"),collapse = ""),
                           SMC_ts_estimations,value = T)

for(i in Stations_names){
  output_dir <- "./02_Data/02_Processed_data/SMC_Estimations_footprtint_merged"
  if(!dir.exists(output_dir)) dir.create(output_dir)
  # Filter files by the Station i
  files_i <- grep(paste0(i,"_"),SMC_ts_estimations,value = T)
  # Change the column name by the footprint
  data_frames <- lapply(files_i, function(path) {
    df <- read.csv(path,sep = ";")
    df$X <- NULL
    df
    footprint <- sapply(strsplit(path,"/"),"[",6)
    #paste0(names(df),footprint)
    df[,grep("SM",colnames(df))] <-df[,grep("SM",colnames(df))]/100
    colnames(df)[colnames(df) == "SM"] <- paste0("Pred_", footprint)
    colnames(df)[colnames(df) == "Track"] <- paste0("Track_", footprint)
    df
  })
  
  # Merge all dataframes in the list "data_frames" 
  # Reduce operates in a succesive way
  merged_df <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), data_frames)
  merged_df[,grep("Coordinates",names(merged_df))] <- NULL
  merged_df$Station <- i
  rownames(merged_df) <- NULL
  write.csv(merged_df,paste0(output_dir,"/SiteID_",i,"_pymm_out.csv"),row.names = F)
}

# List SMC estimation per station with all the footprints merged
SMC_ts_estimations <- list.files("./02_Data/02_Processed_data/SMC_Estimations_footprtint_merged",pattern = ".csv$",
                              full.names = T,recursive = T)
# Row bind all pred df
all_ts_estimations <- map_dfr(SMC_ts_estimations, ~ read.csv(.x,sep =",",dec="."))
all_ts_estimations$Date <- as.POSIXct(all_ts_estimations$Date,tz="GMT")
all_ts_estimations <- all_ts_estimations[order(all_ts_estimations$Date),]


## Estimations with get_map pysmm
all_maps_estimations<- list.files("./02_Data/04_SMC_Estimations/get_map/",
                                  pattern = ".csv$",full.names = T)
all_maps_estimations <- map_dfr(all_maps_estimations, ~ read.csv(.x,sep =",",dec="."))
all_maps_estimations$Date <- as.POSIXct(all_maps_estimations$Date,"%Y%m%d_%H%M%OS",tz="GMT")
names(all_maps_estimations)[grep("Station",names(all_maps_estimations))] <- "Station"
all_maps_estimations<- all_maps_estimations %>% 
  filter(Station%in%unique(all_GTD$Station))%>% 
  select(-c("X","Land_use","Aspect","Latitude","Longitude","Altitude","Altitude_a"))

all_estimations <- full_join(all_maps_estimations,all_ts_estimations,by=c("Station","Date"))

# Adding the Stations sites propierties (name, Land_use,Aspect,...)
# Attributes to add
attributes<-c("Station_na","Land_use","Aspect","Latitude","Longitude",
              "Altitude","Altitude_a") 

all_estimations <- merge(all_estimations,data.frame(Stations)[,names(Stations)%in%attributes],
                            by.x="Station",by.y="Station_na")

### Data frame to compare get_ts() vs get_map() stimations

# Set time tolerance
tolerance <- 3600 # in seconds

# join two data frames with time tolerance
get_ts_vs_map<- fuzzy_join(all_ts_estimations[,c("Station","Date","Pred_20")], all_maps_estimations[,c("Station","Date","Pred_20m_map")], 
                       by = c("Date"),
                       
                       match_fun = function(x, y) abs(difftime(x,y,units = "secs")) <= tolerance)

get_ts_vs_map<- 
  get_ts_vs_map %>% 
  filter(Station.x==Station.y) %>% 
  group_by(Date.x,Station.x) %>% 
  summarise(get_ts=mean(Pred_20,na.rm=T),
            get_map=mean(Pred_20m_map,na.rm=T),
            Station=Station.x)


sum(duplicated(get_ts_vs_map))
write.csv(get_ts_vs_map,"./02_Data/02_Processed_data/Get_ts_vs_map_20m.csv")
#################    SMC Ground Data and Estimations Merge    ##################


# Set time tolerance
tolerance <- 3600 # in seconds

# join two data frames with time tolerance
df_joined<- fuzzy_join(all_estimations, all_GTD, 
                       by = c("Date"),

                       match_fun = function(x, y) abs(difftime(x,y,units = "secs")) <= tolerance)


# Rename .y by "_obs", and ".x" by "_pred"
names(df_joined)[grep(".y",names(df_joined))]<-
  gsub(".y","_obs",grep(".y",names(df_joined),value = T))
names(df_joined)[grep(".x",names(df_joined))]<-
  gsub(".x","_pred",grep(".x",names(df_joined),value = T))


# The dataframe has been joined only with Date,
# but its necessary the same pred and obs station in the join 
df_joined <- df_joined %>%
  # filter by stations
  filter(Station_pred == Station_obs)%>%
  # As the join of the SMC pred and obs was maded with +- 3600 seconds of tolerance 
  # each pred. has multiples joins with obs. 
  # If there are more than one GTD to per each prediction, the mean and sd of the GTD is computed
  # per day in the obs values
  dplyr::group_by(as.Date(Date_obs),Station_pred) %>%
  dplyr::mutate(sd_obs_02=sd(wc_02_av,na.rm =T),obs_02 =mean(wc_02_av, na.rm =T),
          sd_obs_05=sd(wc_05_av, na.rm =T),obs_05 =mean(wc_05_av, na.rm =T),
          Date_obs=mean(Date_obs))  %>% 
  # As we will work with an average of the obs, it is necessary to eliminate 
  # multiple records for each day.  I eliminate duplicate rows obs data per day. 
  # To do this, the columns that correspond to hourly records are eliminated, then 
  # the non-repeated rows are kept.
  select(-c("swc_wc_a_02_avg","swc_wc_a_05_avg",
            "swc_wc_b_02_avg","swc_wc_b_05_avg",
            "swc_wc_c_02_avg","swc_wc_c_05_avg","wc_05_av","wc_02_av")) %>% 
  distinct() # remove duplicates (created in the day agrupation)

head(df_joined)
df_joined$`as.Date(Date_obs)` <- NULL
df_joined[, c("obs_02","obs_05")] <- apply(df_joined[, c("obs_02","obs_05")], 2, function(x) {
  x[x =="NaN"] <- NA
  return(x)
})
df_joined$SMC_map_20m <- NULL

write_csv(df_joined,"./02_Data/02_Processed_data/SMC_GTD_Pred.csv")



