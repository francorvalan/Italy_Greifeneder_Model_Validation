#####                       Italy SMC Data Processing

# Process realized in this script:
#   - 01. Ground Truth Data (GTD) from local stations in Italy are processing:
#       - Removing duplicated data,
#       - Data joined
#       - The output file is a csv with all the , writed in ""./03_Results/""

#   - 02. SMC Estimations processing
#       - 

# francisco.corvalan6@gmail.com
# franciscomartin.corvalan@eurac.edu


library(mapview)
library(dplyr)                                    
library(plyr)                                     
library(readr)  
library(purrr)

####  01 Ground Truth Data (GTD) Processing ####

# List and files filter
SMC_files<- list.files("./02_Data/01_Original_SMC_data/",pattern = ".csv$",full.names = T)
SMC_files <- grep("Wide",SMC_files,invert = T,value = T)

# Set csv configutation to read
local<- locale(
  date_names = "en",
  date_format = "%AD",
  time_format = "%AT",
  decimal_mark = ".",
  grouping_mark = ",",
  tz ="Europe/Berlin",
  encoding = "UTF-8",
  asciify = FALSE
)

# Reading and joining all csv files, removing the fist row (slice(-1))
all_GTD <- map_dfr(SMC_files, ~ read_csv(.x,locale = local) %>% slice(-1))

# Finding duplicated rows
(n_duplicated <- nrow(all_GTD[duplicated(all_GTD),])) # 576 data duplicated

# Finding duplicates inside each file
SMC_files %>% 
  lapply(read_csv) %>%  
  lapply(function(x) sum(duplicated(x))) # there are not duplicated data inside the files...

SMC_files %>% 
  lapply(read_csv) %>%  
  lapply(function(x) unique(x$station)) # the 4 file has 6 stations,...

# Searching if the duplicated data are between the 4 file and the all_GTD
df_4 <- read_csv(SMC_files[[4]],locale = local)%>% slice(-1) # reading file 4
nrow(intersect(all_GTD, df_4)) # 576, there are also one duplicated data,
# now we can know that all data from SMC_files[[4]] are already in other csv files

# removing the duplicated data
all_GTD <- all_GTD[!duplicated(all_GTD),]
sum(duplicated(all_GTD))

str(all_GTD)

write_csv(all_GTD,"./02_Data/02_Processed_data/Ground_truth_data.csv") 

all_GTD <- read.csv("./02_Data/02_Processed_data/Ground_truth_data.csv")
str(all_GTD)
all_GTD$time <- as.POSIXct(all_GTD$time,tz="Europe/Berlin") # Setting Europe tz
all_GTD$wc_05_av <- rowMeans(all_GTD[,c("swc_wc_a_05_avg","swc_wc_b_05_avg","swc_wc_c_05_avg")])


####  02 SMC Estimations processing  ####

SMC_estimations <- list.files("./02_Data/04_SMC_Estimations/",pattern = ".csv$",
          full.names = T)

# Locations site loading
Catchment_area <- sf::read_sf("./02_Data/03_Vector_data/LTSER_IT25_Matsch_Mazia_Catchment.shp")
Stations<- sf::read_sf("./02_Data/03_Vector_data/Station_coordinates.shp")

# Filtering Stations according with the avaibility of ground truth data
Stations<- Stations[Stations$Station_na%in%c(toupper(unique(all_GTD$station))),]

# Mapping
mapviewOptions(fgb = F)
mapview::mapView(Catchment_area,map.types="Esri.WorldImagery",alpha.regions = 0.5)+
  mapview(Stations,col.regions ="black")

# Filtering SMC Estimations by the stations with ground truth data availability
SMC_estimations <- grep(paste(paste0(toupper(unique(all_GTD$station)),collapse ="|"),collapse = ""),
     SMC_estimations,value = T)

# Reading and joining all SMC estimations data
all_estimations <- map_dfr(SMC_estimations, ~ read.csv(.x,locale = local,sep =";",dec=".")) #%>% slice(-1))

#all_estimations <- map_dfr(SMC_estimations, ~ read_delim(.x,locale = local,delim =";")) #%>% slice(-1))
# Adding the sites propierties (name, Land_use,Aspect,...)
options(digits = 9)

#head(Stations)
#head(all_estimations)

## Merging Stations with 
# Adding coordinates all_estimations data
Stations <- cbind(Stations,sf::st_coordinates(Stations))
Stations$Coordinates <- paste0("(",Stations$X,", ",Stations$Y,")")

attributes<-c("Station_na","Land_use","Aspect","Latitude","Longitude",
              "Altitude","Altitude_a","X","Y","Coordinates") 

all_GTD$station <- toupper((all_GTD$station))
all_estimations <- merge(all_estimations,data.frame(Stations)[,attributes],by="Coordinates")

#################    SMC Ground Data and Estimations Merge    ##################
library(fuzzyjoin)
# Set time tolerance
tolerance <- 3800 # in seconds

# join two data frames with time tolerance
all_GTD$Date <- as.POSIXct(all_GTD$time,tz="Europe/Berlin")
names(all_GTD)[2] <- "Station"
names(all_estimations)[5] <- "Station"

df_joined<- fuzzy_join(all_estimations, all_GTD, 
                       by = c("Date"),
                       match_fun = function(x, y) abs(as.numeric(x) - as.numeric(y)) <= tolerance)

# merge data frames using dplyr
df_joined <- df_joined %>%
  
  filter(Station.x ==Station.y)
head(df_joined)

