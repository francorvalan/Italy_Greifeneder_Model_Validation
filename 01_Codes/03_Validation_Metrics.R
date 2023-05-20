# In this scripts several Statics metrics were been 
library(readr)
library(rlang)
library(dplyr)
pred_obs_data <- read_csv("./02_Data/02_Processed_data/SMC_GTD_Pred.csv")

# asd
# # function definition to compute validation metrics
# Accuracy_Metrics<- function(dataframe,obs="obs",pred="obs"){
#   require(caret)
#   require(Metrics)
#   dataframe=pred_obs_data
#   obs="SM_pred"
#   pred= "obs_02"
#   results <- data.frame(Site=NA,Site_ID=NA)
#   try({
#     
#   results$Site <- unique(c(dataframe[,names(dataframe)%in%c("Station_pred","Station","station")]))
#   #results$Site_ID <-unique(c(dataframe[,names(dataframe)%in%c("Station_pred","Station","station")]))
#   results$Lat <- unique(c(dataframe[,names(dataframe)%in%c("lat","Lat","Latitud","latitud","Latitude")]))
#   results$Long <- unique(c(dataframe[,names(dataframe)%in%c("lon","long","Long","Lon","Longitud","longitud","Longitude")]))
#   results$Land_use <- unique(c(dataframe[,names(dataframe)%in%c("Land_use")]))})
#   results$n <- nrow(dataframe)
#   ###  Metrics computation 
#   # R2
#   results$R2 <- caret::R2(dataframe$obs,dataframe$pred)
#   # RMSE Root Mean Squared Error: average prediction error
#   results$RMSE <- Metrics::rmse(dataframe$obs,dataframe$pred)
#   # MAE
#   results$MAE <- Metrics::mae(dataframe$obs,dataframe$pred)
#   # MSE 
#   results$MSE <-  Metrics::mse(dataframe$obs,dataframe$pred)
#   # RSE Residual Standard Error: is an estimate of the standard deviation of  ϵ
#   #results$RSE <-  sqrt(sum((dataframe$obs - dataframe$pred)^2) / (n - k - 1))
#   # Spearman spatial correlation ρ
#   results$Spearman <- cor(dataframe$obs,dataframe$pred,method = "spearman")
#   # Pearson correlation R
#   results$Pearson<- cor(dataframe$obs,dataframe$pred,method = "pearson")
#   # Kling-Gupta Efficiency (KGE)
#   #results$KGE<- hydroGOF::KGE(dataframe$pred,dataframe$obs)
#   # Amount of Variance Explained
#   results$AVE <-  1 - sum((dataframe$pred -dataframe$obs)^2, na.rm=TRUE) /
#     sum((dataframe$obs - mean(dataframe$obs, na.rm = TRUE))^2,na.rm = TRUE)
#   return(results)
# }
# 
# Accuracy_Metrics<- function(dataframe,obs="obs",pred="pred",group=NULL){
#   require(caret)
#   require(Metrics)
#   dataframe=pred_obs_data
#   obs="obs_02"
#   pred="SM_pred"
#   group="Station_obs"
#   
#   dataframe %>% group_by({{group}}) %>% 
#     summarise(RMSE=caret::R2(obs[],pred[]),
#               MAE=Metrics::mae(obs[],pred[]),
#               MSE=Metrics::mse(obs[],pred[]),
#               AVE= (1 - sum((pred[] -obs[])^2, na.rm=TRUE) /
#                 sum((obs[] - mean(obs[], na.rm = TRUE))^2,na.rm = TRUE)),
#               R2=caret::R2(obs[],pred[]),
#               Spearman=cor(obs[],pred[],method = "spearman"),
#               Pearson=cor(obs[],pred[],method = "pearson")
#     )
#   return(dataframe)
# }
# 
# Accuracy_Metrics <- function(dataframe, obs = "obs", pred = "pred", group = NULL) {
#   dataframe %>%
#     filter(complete.cases(!!!obs, {{pred}})) %>%
#     group_by(!!!group) %>%
#     summarise(RMSE = caret::R2(!!!obs, {{pred}}),
#               MAE = Metrics::mae(!!!obs, {{pred}}),
#               MSE = Metrics::mse(!!!obs, {{pred}}),
#               AVE = (1 - sum(({{pred}} - !!!obs)^2, na.rm = TRUE) /
#                        sum((!!!obs - mean(!!!obs, na.rm = TRUE))^2, na.rm = TRUE)),
#               R2 = caret::R2(!!!obs, {{pred}}),
#               Spearman = cor(!!!obs, {{pred}}, method = "spearman"),
#               Pearson = cor(!!!obs, {{pred}}, method = "pearson")
#     ) %>%
#     ungroup()
# }
# 
# Accuracy_Metrics <- function(dataframe, obs = "obs", pred = "pred", group = NULL) {
#     require(rlang)
#   dataframe %>%
#     filter(complete.cases(!!ensym(obs), !!ensym(pred))) %>%
#     group_by(!!!group) %>%
#     summarise(RMSE = caret::R2(!!(obs), !!ensym(pred)),
#               MAE = Metrics::mae(!!ensym(obs), !!ensym(pred)),
#               MSE = Metrics::mse(!!ensym(obs), !!ensym(pred)),
#               AVE = (1 - sum((!!ensym(pred) - !!ensym(obs))^2, na.rm = TRUE) /
#                        sum((!!ensym(obs) - mean(!!ensym(obs), na.rm = TRUE))^2, na.rm = TRUE)),
#               R2 = caret::R2(!!ensym(obs), !!ensym(pred)),
#               Spearman = cor(!!ensym(obs), !!ensym(pred), method = "spearman"),
#               Pearson = cor(!!ensym(obs), !!ensym(pred), method = "pearson")
#     ) %>%
#     ungroup()
# }
# 
# group_list <- list(quote(Station_obs), quote(Land_use))
# obs_list <- list(quote(obs_02), quote(obs_05))
# 
# # output_list <- lapply(group_list, function(b_value) {
# #   Accuracy_Metrics(pred_obs_data,obs=obs_02,pred=SM_pred,group=b_value)
# # })
# 
# output_list <- lapply(obs_list, function(obs_value) {
#   lapply(group_list, function(group_value) {
#     Accuracy_Metrics(pred_obs_data, obs = obs_value, pred = SM_pred, group = group_value)
#   })
# })
# 
# 
# Accuracy_Metrics(pred_obs_data,obs=obs_02,pred=SM_pred,group=Station_obs)




Accuracy_Metrics <- function(dataframe, obs = "obs", pred = "pred", group = NULL) {
  # This function computes several validation metrics:
  #     - AVE, RMSE, MSE, MAE, R2, Spearman, Pearson
  # It allows to group the data in order to compute this mettrics by group.
  require(caret)
  require(Metrics)
  dataframe %>%
    filter(complete.cases(!!ensym(obs), !!ensym(pred))) %>%
    group_by(({{group}}))%>%
      summarise(
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

# Create list to make all possible validation combination
# Posibles footprints predictions
pred_list <- as.list(sort(noquote(grep("Pred",names(pred_obs_data),value = T))))[order(as.numeric(gsub("\\D", "", pred_list)))]
pred_list <- lapply(pred_list, function(x) noquote (x))
# Observations depth
obs_list <- list(quote(obs_02), quote(obs_05))

# Land-use groups
group_list <- list(quote(Station_obs), quote(Land_use))
group_list<- list(NULL)
# output_list[[1]][[2]] would give you the output of
# first "obs" value (obs_02) and the second "group" value (Land_use).

# output_list <- lapply(obs_list, function(obs_value) {
#   lapply(group_list, function(group_value) {
#     Accuracy_Metrics(pred_obs_data, obs = !!obs_value, pred = SM_pred, group = group_value)
#   })
# })

# output_list[[1]][[2]][[3]] would give you the output of
# first "obs" value (obs_02) and the second "group" value (Land_use).
output_list <- lapply(obs_list, function(obs_value) {
  lapply(pred_list,function(pred_value){
    lapply(group_list, function(group_value) {
      Accuracy_Metrics(pred_obs_data, obs = !!obs_value, pred = !!pred_value, group = !!group_value)
    })
  })
})

output_list

# pred_obs_data
# output_list <- lapply(obs_list, function(obs_value) {
#   lapply(pred_list,function(pred_value){
#     
#       Accuracy_Metrics(pred_obs_data, obs = !!obs_value, pred = !!pred_value, group = none)
#     
#   })
# })



# Write data frames to CSV files
output_dir <- "./03_Results/Accuracy/Sin_agrupar/"  # Specify the directory where the files will be saved
if(!dir.exists(output_dir)) dir.create(output_dir)

# Iterate over the output list and write each data frame to a CSV file

# for (i in seq_along(obs_list)) {
#   for (j in seq_along(group_list)) {
#     filename <- paste0("Accuracy_", obs_list[[i]], "_per_", group_list[[j]], ".csv")
#     filepath <- file.path(output_dir, filename)
#     write.csv2(output_list[[i]][[j]], file = filepath, row.names = FALSE)
#   }
# }

for(obs in seq_along(obs_list)){
  for (p in seq_along(pred_list)) {
    for (landc in seq_along(group_list)) {
      filename <- paste0("Accuracy","depth_", obs_list[[obs]],"pred_footprint_",pred_list[[p]], "_per_", group_list[[landc]], ".csv")
      filepath <- file.path(output_dir, filename)
      write.csv2(output_list[[obs]][[p]][[landc]], file = filepath, row.names = FALSE)
    }
  }
}


dataframe %>%
  filter(complete.cases(!!ensym(obs), !!ensym(pred))) %>%
  group_by(!!!ensym(group)) %>%
  summarise(RMSE = caret::R2(!!ensym(obs), !!ensym(pred)),
            MAE = Metrics::mae(!!ensym(obs), !!ensym(pred)),
            MSE = Metrics::mse(!!ensym(obs), !!ensym(pred)),
            AVE = (1 - sum((!!ensym(pred) - !!ensym(obs))^2, na.rm = TRUE) /
                     sum((!!ensym(obs) - mean(!!ensym(obs), na.rm = TRUE))^2, na.rm = TRUE)),
            R2 = caret::R2(!!ensym(obs), !!ensym(pred)),
            Spearman = cor(!!ensym(obs), !!ensym(pred), method = "spearman"),
            Pearson = cor(!!ensym(obs), !!ensym(pred), method = "pearson")
  ) %>%
  ungroup()


#################################################################################

Accuracy_Metrics <- function(dataframe, obs = "obs", pred = "pred",group=NULL) {
  require(caret)
  require(Metrics)
  
  dataframe %>%
    filter(complete.cases({{ obs }}, {{ pred }})) %>%
    #if(!is.null({{group}})) group_by({{group}}) 
    group_by({{group}}) %>% 
    summarise(
      RMSE = caret::R2({{ obs }}, {{ pred }}),
      MAE = Metrics::mae({{ obs }}, {{ pred }}),
      MSE = Metrics::mse({{ obs }}, {{ pred }}),
      AVE = {
        obs_mean <- mean({{ obs }}, na.rm = TRUE)
        1 - sum(({{ pred }} - {{ obs }})^2, na.rm = TRUE) / sum(({{ obs }} - obs_mean)^2, na.rm = TRUE)
      },
      R2 = caret::R2({{ obs }}, {{ pred }}),
      Spearman = cor({{ obs }}, {{ pred }}, method = "spearman"),
      Pearson = cor({{ obs }}, {{ pred }}, method = "pearson")
    ) %>%
    ungroup()
}


Accuracy_Metrics(pred_obs_data, obs = obs_05, pred = Pred_50m,group = asd)


