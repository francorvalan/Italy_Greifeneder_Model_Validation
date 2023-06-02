# In this scripts several Statics metrics were been 
library(readr)
library(rlang)
library(dplyr)
pred_obs_data <- read_csv("./02_Data/02_Processed_data/SMC_GTD_Pred.csv")
(col_melt <- grep("Pred",names(pred_obs_data),invert = T,value = T))
pred_obs_data_pred_melted <- melt(pred_obs_data,id=col_melt,value.name = "Pred")
names(pred_obs_data_pred_melted)[grep("variable",names(pred_obs_data_pred_melted))] <- "Pred_type"

Accuracy_Metrics <- function(dataframe, obs = "obs", pred = "pred", group = NULL,Station_name=NULL) {
  # This function computes several validation metrics:
  #     - AVE, RMSE, MSE, MAE, R2, Spearman, Pearson
  # It allows to group the data in order to compute this mettrics by group.
  require(caret)
  require(Metrics)
  Depth <- quo_name(enquo(obs))
  Station <- quo_name(enquo(Station_name))
  Foot <- quo_name(enquo(pred))
  if (!is.null(Station_name)) {
    dataframe <- filter(dataframe,Station_pred == {{Station_name}})
  }
  dataframe %>%
    filter(complete.cases(!!ensym(obs), !!ensym(pred))) %>%
    group_by({{group}})%>%
      summarise(
              Depth=Depth,
              if (!is.null(Station_name)) {
                Station = {{Station_name}}
              },
              Footprint=Foot,
              n=n(),
              RMSE = caret::RMSE(!!ensym(obs), !!ensym(pred)),
              MAE = Metrics::mae(!!ensym(obs), !!ensym(pred)),
              MSE = Metrics::mse(!!ensym(obs), !!ensym(pred)),
              AVE = (1 - sum((!!ensym(pred) - !!ensym(obs))^2, na.rm = TRUE) /
                       sum((!!ensym(obs) - mean(!!ensym(obs), na.rm = TRUE))^2, na.rm = TRUE)),
              R2 = caret::R2(!!ensym(obs), !!ensym(pred)),
              Spearman = cor(!!ensym(obs), !!ensym(pred), method = "spearman"),
              Pearson = cor(!!ensym(obs), !!ensym(pred), method = "pearson")
    ) %>%
    ungroup()
}

######### STATION AND LAND-COVER VALIDATION IN ALL POSIBLE COMBINATIONS VALIDATION 
# Create list to make all possible validation combination
# Posibles footprints predictions
pred_list <- as.list(sort(noquote(grep("Pred",names(pred_obs_data),value = T))))
pred_list <- pred_list[order(as.numeric(gsub("\\D", "", pred_list)))]
pred_list <- lapply(pred_list, function(x) noquote (x))

# Observations depth
obs_list <- list(quote(obs_02), quote(obs_05))

# Land-use groups
group_list <- list(quote(Station_obs), quote(Land_use))

Station_list
# ACCURACY METRICS FOR ALL POSSIBLE DEPTHS(2)-FOOTPRINTS(6)-LAND COVER(2) COMBINATIONS (24)
# output_list[[1]][[2]][[1]] would give you the output of
# first "obs" value (obs_02) and the second "pred_value" (Pred_100m) in the 3 "group" (Station_obs).
output_list <- lapply(obs_list, function(obs_value) {
  lapply(pred_list,function(pred_value){
    lapply(Station_list, function(Station_name) {
      Accuracy_Metrics(pred_obs_data, obs = !!obs_value, 
                       pred = !!pred_value, group =!!group_list)
    })
  })
})


# Write data frames to CSV files
output_dir <- "./03_Results/Accuracy/All_combinations"  # Specify the directory where the files will be saved
if(!dir.exists(output_dir)) dir.create(output_dir)

# Iterate over the output list and write each data frame to a CSV file
for(obs in seq_along(obs_list)){
  for (p in seq_along(pred_list)) {
    for (landc in seq_along(group_list)) {
      filename <- paste0("Accuracy","depth_", obs_list[[obs]],"pred_footprint_",pred_list[[p]], "_per_", group_list[[landc]], ".csv")
      filepath <- file.path(output_dir, filename)
      write.csv2(output_list[[obs]][[p]][[landc]], file = filepath, row.names = FALSE)
    }
  }
}



##################           Overall_Accuracy            #######################
# output_list[[1]][[2]][[3]] would give you the output of
# first "obs" value (obs_02) and the second "group" value (Land_use).

# Posibles footprints predictions
pred_list <- as.list(sort(noquote(grep("Pred",names(pred_obs_data),value = T))))
pred_list <- pred_list[order(as.numeric(gsub("\\D", "", pred_list)))]
pred_list <- lapply(pred_list, function(x) noquote (x))

# Observations depth
obs_list <- list(quote(obs_02), quote(obs_05))
group_list<- list(NULL)

Overall_Accuracy <- lapply(obs_list, function(obs_value) {
  lapply(pred_list,function(pred_value){
    lapply(group_list, function(group_value) {
      Accuracy_Metrics(pred_obs_data, obs = !!obs_value, pred = !!pred_value, group = !!group_value)
    })
  })
})
Overall_Accuracy <- lapply(Overall_Accuracy, unlist,recursive=F)
join_df <- function(x){
  do.call(rbind,x)
}

output_dir <- "./03_Results/Accuracy/Overall_accuracy/"  # Specify the directory where the files will be saved
if(!dir.exists(output_dir)) dir.create(output_dir)

Overall_Accuracy <-lapply(Overall_Accuracy, join_df)
write_files <- function(x){
  GTD_Depth <- c(unique(x[,"Depth"]))
  write.csv2(x,file = paste0(output_dir,"Overall_accuracy_",GTD_Depth,".csv"))
}
lapply(Overall_Accuracy,write_files)

################        LAND-COVER OVERALL ACCURACY        #####################

group_list<- list(quote(Land_use))

Overall_Accuracy_Land_cover <- lapply(obs_list, function(obs_value) {
  lapply(pred_list,function(pred_value){
    lapply(group_list, function(group_value) {
      Accuracy_Metrics(pred_obs_data, obs = !!obs_value, pred = !!pred_value, group = !!group_value)
    })
  })
})

bind_df <- function(x){
  do.call(rbind,x)
}


Overall_Accuracy_Land_cover<- lapply(lapply(Overall_Accuracy_Land_cover,unlist,recursive=F), bind_df)

Overall_Accuracy_Land_cover <- lapply(Overall_Accuracy_Land_cover, function(x){
  x %>% arrange(`(Land_use)`) %>% arrange(Depth) %>% mutate(across(where(is.numeric), round, 3))
})

# Write data frames to CSV files
output_dir <- "./03_Results/Accuracy/Overall_accuracy_Land_cover/"  # Specify the directory where the files will be saved
if(!dir.exists(output_dir)) dir.create(output_dir)
for(obs in seq_along(obs_list)){
      filename <- paste0("Overall_Land_cover_accuracy_","depth_", gsub("\\D","",obs_list[[obs]]),"_cm", ".csv")
      filepath <- file.path(output_dir, filename)
      write.csv2(Overall_Accuracy_Land_cover[[obs]], file = filepath, row.names = FALSE)
}

##### Accuraccy per Stations

######### STATION AND LAND-COVER VALIDATION IN ALL POSIBLE COMBINATIONS VALIDATION 
# Create list to make all possible validation combination
# Posibles footprints predictions
#pred_list <- as.list(sort(noquote(grep("Pred",names(pred_obs_data),value = T))))
#pred_list <- c("SMC_map_20m",pred_list)
#pred_list <- pred_list[order(as.numeric(gsub("\\D", "", pred_list)))]
#pred_list <- lapply(pred_list, function(x) noquote (x))
pred_list <- as.list(quote(Pred))
# Observations depth
obs_list <- list(quote(obs_02), quote(obs_05))

# Land-use groups
#group_list <- list(quote(Station_obs), quote(Land_use))
group_list <-list(quote(Pred_type)) 
# Stations list
Station_list = unlist(unique(pred_obs_data$Station_pred))


# ACCURACY METRICS FOR ALL POSSIBLE DEPTHS(2)-FOOTPRINTS(6)-LAND COVER(2) COMBINATIONS (24)
# output_list[[1]][[2]][[1]] would give you the output of
# first "obs" value (obs_02) and the second "pred_value" (Pred_100m) in the 3 "group" (Station_obs).
Accuracy_per_Station <- lapply(obs_list, function(obs_value) {
  lapply(pred_list,function(pred_value){
    lapply(Station_list, function(Station_name) {
      Accuracy_Metrics(pred_obs_data_pred_melted, obs = !!obs_value, 
                       pred = !!pred_value, group =!!group_list[[1]],Station_name=Station_name)
    })
  })
})

bind_df <- function(x){
  df <- do.call(rbind,x)
  df$Footprint <- as.numeric(gsub("\\D","",df$`(Pred_type)`))
  df<- df %>% arrange(Depth ,Footprint)
}

Accuracy_per_Station <- lapply(Accuracy_per_Station,unlist,recursive=F)
Accuracy_per_Station<- list.zip(Accuracy_per_Station[[1]],Accuracy_per_Station[[2]])
Accuracy_per_Station<- lapply(Accuracy_per_Station, bind_df)

output_dir <- "./03_Results/Accuracy/Accuracy_per_sation"
if(!dir.exists(output_dir))(dir.create(output_dir))

for (s in 1:length(Station_list)) {
    filename <- paste0("Station_",Station_list[[s]],".csv")
    write.csv2(Accuracy_per_Station[[s]],file.path(output_dir, filename))
}

