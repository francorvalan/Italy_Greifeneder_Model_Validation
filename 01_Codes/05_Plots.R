### In this Scripts we build this plots :
## 01 - Data distribution: Half-eye (density + boxplot)
## 02 - SMC GTD and Predictions over Time
## 03 - SSCATTERPLOTS
## 04 -
library(dplyr)
library(ggdist)
library(ggplot2)
library(readr)
library(tidyquant)
library(reshape2)
library(ggpubr)

local<- locale(
  date_names = "en",
  date_format = "%AD",
  time_format = "%AT",
  decimal_mark = ".",
  grouping_mark = ";",
  tz ="Europe/Rome",
  encoding = "UTF-8",
  asciify = FALSE
)

all_GTD <- read_delim("./02_Data/02_Processed_data/Ground_truth_data.csv",
                      delim=",",locale = local)
str(all_GTD)
all_GTD$Date <- as.POSIXct(all_GTD$Date,tz="Europe/Rome")
#all_GTD$wc_05_av <- rowMeans(all_GTD[,c("swc_wc_a_05_avg","swc_wc_b_05_avg","swc_wc_c_05_avg")])
#all_GTD$wc_02_av <- rowMeans(all_GTD[,c("swc_wc_a_02_avg","swc_wc_b_02_avg","swc_wc_c_02_avg")])
df_joined <- read_csv("./02_Data/02_Processed_data/SMC_GTD_Pred.csv")



#############               01 - DATA DISTRIBUTION               ###############
all_GTD$Station <- as.factor(all_GTD$Station)
## SMC data distribution at 2 cm depth
all_GTD %>% group_by(Station) %>% summarise(median=median(wc_02_av,na.rm = T))
all_GTD %>% 
  #filter(wc_02_av<1) %>% 
  ggplot(aes(y=wc_02_av,x=Station,fill=Station))+
  ggdist::stat_halfeye(adjust = 0.5,
                       justification=-0.2,
                       .width=0,
                       point_color=NA,show.legend=F)+
  geom_boxplot(width = 0.2,
               alpha=0.2,
               show.legend = FALSE)+ theme(legend.position = "none")   +
  labs(title="Soil moisture content data at 2 cm",x="SMC m³/m³",y="Stations")+
  scale_fill_tq()+theme_tq()+
  coord_flip()

## SMC data distribution at 5 cm depth

all_GTD %>% group_by(Station) %>% summarise(median=median(wc_05_av,na.rm = T))
all_GTD %>% 
  #filter(wc_02_av<1) %>% 
  ggplot(aes(y=wc_05_av,x=Station,fill=Station))+
  ggdist::stat_halfeye(adjust = 0.5,
                       justification=-0.2,
                       .width=0,
                       point_color=NA,show.legend=F)+
  geom_boxplot(width = 0.2,
               alpha=0.2)+#+ theme(legend.position = "none")   +
  labs(title="Soil moisture content data at 5 cm",x="SMC m³/m³",y="Stations")+
  scale_fill_tq()+theme_tq()+
  coord_flip()

############      02 - SMC GTD and Predictions over Time          ##############

Temporal_plot_function <- function(Gound_thouth = data.frame(),
                                   Estimations = data.frame(),
                                   depths = character(),
                                   stations_names = character()) {
  # This function create a plot adding the SMC Ground Data at one 
  # specific depth and adding all the possible predictions with diff 
  # footprints
  GTD <- Gound_thouth[Gound_thouth$Station == stations_names, ]
  pred <- Estimations[Estimations$Station_pred == stations_names, ]
  depth_cm<- gsub("\\D","",depths)
  ggplot(data = GTD, aes(Date, !!as.name(depths), colour = "depths")) +
    geom_point(size = 0.3) +
    geom_point(data = pred, aes(Date_obs, Pred_20m_map, colour = "Pred_20m_map"), size = 0.8) +
    geom_point(data = pred, aes(Date_obs, Pred_20, colour = "Pred_20"), size = 0.8) +
    geom_point(data = pred, aes(Date_obs, Pred_50, colour = "Pred_50"), size = 0.8) +
    geom_point(data = pred, aes(Date_obs, Pred_100, colour = "Pred_100"), size = 0.8) +
    geom_point(data = pred, aes(Date_obs, Pred_200, colour = "Pred_200"), size = 0.8) +
    geom_point(data = pred, aes(Date_obs, Pred_500, colour = "Pred_500"), size = 0.8) +
    scale_color_manual(name = "Data",
                       values = c("depths" = 'royalblue4',
                                  "Pred_20m_map"="forestgreen",
                                  "Pred_50" ="deeppink",
                                  "Pred_50" = 'orangered',
                                  "Pred_100" = 'seagreen2',
                                  "Pred_200" = 'purple1',
                                  "Pred_500" = 'gold1'),
                       labels = c(paste0("SMC at ",depth_cm," cm"),
                                  "Predicted 20m map",
                                  "Predicted 20m ts",
                                  "Predicted 50m ts",
                                  "Predicted 100m ts",
                                  "Predicted 200m ts",
                                  "Predicted 500m ts")) +
    ylab("SMC m3.m-3") +
    xlab("Date") +
    ggtitle(paste0("Soil moisture at ",depth_cm," cm, Station ",stations_names))
}

# Depths in the GTD to plot
depths <- c("wc_05_av", "wc_02_av")
# Stations
stations_names <- unique(pred_obs_data_melt_footprint$Station_pred)
# All possible combinations between Stations and depths
combinations <- expand.grid(depths = depths, stations_names = stations_names)

temporal_dir_output_plot <- "./04_Plots/Temporal_plots/"
if(!dir.exists(temporal_dir_output_plot))(dir.create(temporal_dir_output_plot))

for (i in 1:nrow(combinations)) {
  depth <- as.character(combinations$depths[i][1])
  station <- as.character(combinations$stations_names[i])
  
  jpeg(paste0(temporal_dir_output_plot,"Station_",station,"_",depth,"_cm.jpg"), 
       width = 1300, height = 500,quality=100,res=150)
  print(Temporal_plot_function(Gound_thouth = all_GTD,
                               Estimations = df_joined,
                               depths = depth,
                               stations_names = station))
  dev.off()
}


 
############                  03 - SSCATTERPLOTS                  ##############


# Overall by stations
cols_melt_footprint<- names(df_joined)[!names(df_joined)%in%c("Pred_20m_map","Pred_20","Pred_50","Pred_100","Pred_200","Pred_500")]
pred_obs_data_melt_footprint <- reshape2::melt(df_joined,id = cols_melt_footprint) 
pred_obs_data_melt_footprint$Footprint <- as.numeric(gsub("\\D","",pred_obs_data_melt_footprint$variable))
pred_obs_data_melt_footprint <- pred_obs_data_melt_footprint %>% arrange(Footprint)
pred_obs_data_melt_footprint_P3 <- pred_obs_data_melt_footprint %>% filter(Station_pred=="P3")


scatter_plots<- function(dataframe=NULL,obs=NULL,pred=NULL,group=NULL,station=NULL){
  if(!is.null(station))( dataframe<- dataframe %>% filter(Station_pred==station))
  x_lab <- paste0("SMC Observed at ",as.numeric(gsub("\\D","",obs))," cm.") 
  y_lab <- c("SMC Predicted") 
  Title <- if(!is.null(station))(paste0("Station ",station,", SCM at ",as.numeric(gsub("\\D","",obs))," cm."))else{
    paste0("Overall Accuracy, SCM at ",as.numeric(gsub("\\D","",obs))," cm.")
  }
  dataframe <- dataframe %>% filter(complete.cases(value))
  print(
    ggscatter(dataframe, x = obs, 
            y = pred, 
            col='royalblue4',size=0.2,# points
            add = "reg.line",panel.labs.background = list(fill = "skyblue3", color = "black"),
            add.params = list(color = "black",size=0.6,linetype = "solid"),
            #panel.labs = list("LC_Class"=c("Herbaceous vegetation","Crops")),
            #cor.coef = TRUE, 
            #cor.coeff.args = list(method = "pearson", size=3,label.x = 0.35,label.y = 0.15, label.sep = "\n"),
            facet.by = group)+
    stat_cor(aes(label = paste(..rr.label..,..r.label..,  sep = "~`,`~")),
             label.x = 0.15,label.y = 0.1,size=3,label.sep='\n',digits = 2) +
    labs(y=y_lab, x=x_lab,size=3,title = Title)+
    geom_abline(intercept=0, slope=1,col='red',lty=2,size=.5)
  )
}
##-----03.1 SMC at 02 cm vs Footprints
scatter_plots(dataframe = pred_obs_data_melt_footprint,
              obs ="obs_02",
              pred = "value",
              group = 'variable')



##-----03.3 SMC by Stations
scatter_plots(dataframe = pred_obs_data_melt_footprint,
              obs ="obs_05",
              pred = "value",
              group = 'variable')

stations <- unique(pred_obs_data_melt_footprint$Station_pred)
depths <- c("obs_02","obs_05")
scatter_plots_combinations <- expand.grid(stations,depths)
output_plots_dir <- "./04_Plots/Scatter_plots_by_Stations"
if(!dir.exists(output_plots_dir))(dir.create(output_plots_dir))

for(i in 1:nrow(scatter_plots_combinations)){
  combination<- paste0(unlist(scatter_plots_combinations[i,]),collapse = "_")
  jpeg(paste0(output_plots_dir,"/",combination,".jpg"), 
       width =1600, height = 1000,quality=100,res=250)
  scatter_plots(dataframe = pred_obs_data_melt_footprint,
                obs =as.character(scatter_plots_combinations[i,2]),
                pred = "value",
                group = 'variable',station = as.character(scatter_plots_combinations[i,1]))
  dev.off()
}




##-----03.4 SMC per each Pred gruped by Land-cover
scatterplot_Landcover_group<- function(dataframe,obs=NULL,pred=NULL){
  Depth <- as.numeric(gsub("\\D","",quo_name(enquo(obs))))
  foot <- paste0(as.numeric(gsub("\\D","",pred))," cm")
  if (grepl("map", quo_name(enquo(pred)))) {
    foot <- paste0(foot, " map")
  }
  dataframe <- dataframe %>% filter(variable==as.character(!!{{pred}}),
                                    complete.cases(Land_use))

  print(ggscatter(dataframe, x = quo_name(enquo(obs)), 
            y = "value", 
            col='royalblue4', size=0.2,# points
            add = "reg.line",panel.labs.background = list(fill = "skyblue3", color = "black"),
            add.params = list(color = "black",size=0.4,linetype = "solid"),
            facet.by = 'Land_use')+
    stat_cor(aes(label = paste(..rr.label..,..r.label..,  sep = "~`,`~")),
             label.x = 0.10,label.y = 0.05,size=3,label.sep='\n',digits = 2) +
    labs(y=paste0('SMC Predicted ',foot), x=paste0('SMC Observed ',Depth,' cm'),size=3)+
    geom_abline(intercept=0, slope=1,col='red',lty=2,size=.5)
  )
}

pred_list <- unlist(unique(pred_obs_data_melt_footprint$variable))
(obs_list <-as.factor(c("obs_02","obs_05")))

output_plots <- "./04_Plots/Scatter_plots_LC_grouped/"
if(!dir.exists(output_plots))(dir.create(output_plots))
for(depth in 1:length(obs_list)){
  for(pred in 1:length(pred_list)){
    jpeg(paste0(output_plots,"SMC_depth_",obs_list[[depth]],"_cm,prediction_",pred_list[[pred]],".jpg"), 
         width = 800, height = 500,quality=100,res=150)
    print(scatterplot_Landcover_group(pred_obs_data_melt_footprint,obs =!!obs_list[[depth]],pred = pred_list[[pred]]))
    dev.off()
    }
}

lapply(pred_list,function(pred){
  lapply(obs_list, function(obs){
    scatterplot_Landcover_group(pred_obs_data_melt_footprint,obs ="obs_02",pred = !!pred)
  })
})


#################### PYSMM  get_map vs get_ts()
get_ts_vs_map <- read_csv("./02_Data/02_Processed_data/Get_ts_vs_map_20m.csv")

ggscatter(df_joined, x = "Pred_20", size = 0.3,
          y = "Pred_20m_map",# points
          add = "reg.line",panel.labs.background = list(fill = "skyblue3", color = "black"),
          add.params = list(color = "royalblue4",size=0.3))+
  stat_cor(aes(label = paste(..rr.label..,..r.label..,sep = "~`,`~")),
           label.x = 0.1,label.y = 0.05,size=3,label.sep='\n')+
  geom_abline(intercept=0, slope=1,col='red',lty=3)+
  labs(y='get_map 20m', x='get_ts 20m',size=2)+ ggtitle("Pysmm prediction functions")

ggscatter(df_joined, x = "Pred_20", size = 0.3,
          y = "Pred_20m_map",# points
          add = "reg.line",panel.labs.background = list(fill = "skyblue3", color = "black"),
          add.params = list(color = "royalblue4",size=0.3))+
  stat_cor(aes(label = paste(..rr.label..,..r.label..,sep = "~`,`~")),
           label.x = 0.1,label.y = 0.05,size=3,label.sep='\n')+
  geom_abline(intercept=0, slope=1,col='red',lty=3)+
  labs(y='get_map 20m', x='get_ts 20m',size=2)+ ggtitle("Pysmm prediction functions")



# ###############################################
# ggscatter(df_joined, x = "obs_02", size = 0.3,
#           y = "Pred_500m",# points
#           add = "reg.line",panel.labs.background = list(fill = "skyblue3", color = "black"),
#           add.params = list(color = "royalblue4",size=0.3))+
#   stat_cor(aes(label = paste(..rr.label..,..r.label..,sep = "~`,`~")),
#            label.x = 0.35,label.y = 0.1,size=3,label.sep='\n')+
#   geom_abline(intercept=0, slope=1,col='red',lty=3)+
#   labs(y='Predicted Values', x='Observed Values',size=2)+ ggtitle("Overall Accuracy")
# 
# # Scatter Gruping by Land-Cover Class
# cols_melt_footprint<- names(df_joined)[!names(df_joined)%in%c("Pred_20m_map","Pred_20","Pred_50","Pred_100","Pred_200","Pred_500")]
# pred_obs_data_melt_footprint <- reshape2::melt(df_joined,id = cols_melt_footprint) 
# pred_obs_data_melt_footprint$Footprint <- as.numeric(gsub("\\D","",pred_obs_data_melt_footprint$variable))
# pred_obs_data_melt_footprint <- pred_obs_data_melt_footprint %>% arrange(Footprint)
# 
# 
# ggscatter(pred_obs_data_melt_footprint, x = "obs_05", 
#           y = "value", 
#           col='royalblue4',size=0.2,# points
#           add = "reg.line",panel.labs.background = list(fill = "skyblue3", color = "black"),
#           add.params = list(color = "royalblue4",size=0.1),
#           #panel.labs = list("LC_Class"=c("Herbaceous vegetation","Crops")),
#           #cor.coef = TRUE, 
#           #cor.coeff.args = list(method = "pearson", size=3,label.x = 0.35,label.y = 0.15, label.sep = "\n"),
#           facet.by = 'Land_use')+#panel.labs=list("Footprint"=c("20 m map","20 m",50 m","100 m","200 m","500 m"))
#   stat_cor(aes(label = paste(..rr.label..,..r.label..,  sep = "~`,`~")),
#            label.x = 0.15,label.y = 0.1,size=3,label.sep='\n',digits = 2) +
#   labs(y='SMC Predicted', x='SMC Observed 5 cm',size=3)+
#   geom_abline(intercept=0, slope=1,col='red',lty=2,size=.5)
# 
# 
# df <- reshape2::melt(Land_use[,c("value","obs_05","obs_02","Land_use","Station_obs")], id = c("Station_obs"))
# 
# ggplot(data=df[df$Station_obs=="P1",], aes(x=value, fill=variable)) +
#   geom_density(alpha=0.5) +
#   theme_minimal()
# 
# 
# p <- ggplot(df[df$Station_obs=="P1",], aes(x = value, fill = variable)) + 
#   geom_histogram(position = "identity", alpha = 0.5, bins = 10)
# # add facets for each class
# p <- p + facet_wrap(~ Station_pred)
# 
# # customize plot labels and theme
# p <- p + labs(x = "Predictions", y = "Count", fill = "Observations")
# p <- p + theme_bw()
# 
# # print plot
# print(p)
# 
# 
# ggscatter(df_joined, x = "obs_02", 
#           y = "Pred_50m", 
#           col='royalblue4',size=0.2,# points
#           add = "reg.line",panel.labs.background = list(fill = "skyblue3", color = "black"),
#           add.params = list(color = "royalblue4",size=0.1),
#           #panel.labs = list("LC_Class"=c("Herbaceous vegetation","Crops")),
#           #cor.coef = TRUE, 
#           #cor.coeff.args = list(method = "pearson", size=3,label.x = 0.35,label.y = 0.15, label.sep = "\n"),
#           facet.by = 'Station_pred')+
#   stat_cor(aes(label = paste(..rr.label..,..r.label..,  sep = "~`,`~")),
#            label.x = 0.15,label.y = 0.1,size=3,label.sep='\n') +
#   labs(y='SMC Predicted', x='SMC Observed',size=3)+
#   geom_abline(intercept=0, slope=1,col='red',lty=3)
# 
# 
# ggscatter(df_joined %>% filter(complete.cases(Pred_200m)), x = "obs_05", 
#           y = "Pred_200m", 
#           col='royalblue4',size=0.2,# points
#           add = "reg.line",panel.labs.background = list(fill = "skyblue3", color = "black"),
#           add.params = list(color = "royalblue4",size=0.1),
#           #panel.labs = list("LC_Class"=c("Herbaceous vegetation","Crops")),
#           #cor.coef = TRUE, 
#           #cor.coeff.args = list(method = "pearson", size=3,label.x = 0.35,label.y = 0.15, label.sep = "\n"),
#           facet.by = 'Land_use')+
#   stat_cor(aes(label = paste(..rr.label..,..r.label..,sep = "~`,`~")),
#            label.x = 0.15,label.y = 0.1,size=3,label.sep='\n')+
#   labs(y='SMC Predicted', x='SMC Observed',size=3)+
#   geom_abline(intercept=0, slope=1,col='red',lty=3) 
# df_joined<- df_joined[df_joined$obs_02<0.5,]
# 
# 
# ggscatter(df_joined, x = "obs_05", 
#           y = "SM_pred", 
#           col='royalblue4',size=0.2,# points
#           add = "reg.line",panel.labs.background = list(fill = "skyblue3", color = "black"),
#           add.params = list(color = "royalblue4",size=0.1),
#           #panel.labs = list("LC_Class"=c("Herbaceous vegetation","Crops")),
#           #cor.coef = TRUE, 
#           #cor.coeff.args = list(method = "pearson", size=3,label.x = 0.35,label.y = 0.15, label.sep = "\n"),
#           facet.by = 'Aspect_pred')+
#   stat_cor(aes(label = paste(..rr.label..,..r.label..,sep = "~`,`~")),
#            label.x = 0.15,label.y = 0.1,size=3,label.sep='\n')+
#   labs(y='SMC Predicted', x='SMC Observed',size=3)+
#   geom_abline(intercept=0, slope=1,col='red',lty=3)



########### Monthly Errors #### 

# Funcion to add the number of samples in each boxplot
stat_box_data <- function(y, upper_limit = 0.3 * 1.15) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste0('n',"\n", length(y))
    )
  )
}

#pred = "Pred_100" # Pred_20m_map
pred_list <- (unique(pred_obs_data_melt_footprint$variable))
obs = c("obs_02","obs_05")
combinations<- expand.grid(pred_list,obs)
# df_joined %>% mutate(dif = (!!sym(pred)-!!sym(obs)),month=format(df_joined$Date_pred,"%m")) %>% 
#   ggplot(aes(x=month,y=dif,fill=month))+geom_boxplot()+
#   ggtitle(paste0("Errors in SMC estimations at 5 cm with ",(pred)))+
#   ylab(expression(epsilon ~ m^3/m^3))+xlab("Months")+
#   stat_summary(
#     fun.data = stat_box_data, 
#     geom = "text", size =3,
#     hjust = 0.5,
#     vjust = 1
#   )

# Output directories creation
lapply(list("./04_Plots/Monthly_Errors/SMC_at_2cm","./04_Plots/Monthly_Errors/SMC_at_5cm"),function(x){
  if(!dir.exists(x))(dir.create(x,recursive = T))
})

# Monthly box plots in all the combinations
for(i in 1:nrow(combinations)){
  if(combinations[i,2]=="obs_02")({
    outpout_dir <- "./04_Plots/Monthly_Errors/SMC_at_2cm/"
  }) else(outpout_dir <- "./04_Plots/Monthly_Errors/SMC_at_5cm/")
  
  depth <- as.numeric(gsub("\\D","",combinations[i,2]))
  jpeg(paste0(outpout_dir,"Errors_",paste0(unlist(combinations[i,]),collapse = "_"),".jpg"), 
       width = 800, height = 500,quality=100,res=150)
  print(df_joined %>% mutate(dif = (!!sym(noquote(as.character(combinations[i,1]))) - 
                              !!sym(noquote(as.character(combinations[i,2])))),
                     month=format(df_joined$Date_pred,"%m")) %>% 
  ggplot(aes(x=month,y=dif,fill=month))+geom_boxplot()+
  ggtitle(paste0("Errors in SMC estimations at ",depth,"cm with ",as.character(combinations[i,1])))+
  ylab(expression(epsilon ~ m^3/m^3))+xlab("Months")+
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", size =3,
    hjust = 0.5,
    vjust = 1
  ))
  dev.off()
}


######## MAPS
# Locations site loading
Catchment_area <- sf::read_sf("./02_Data/03_Vector_data/LTSER_IT25_Matsch_Mazia_Catchment.shp")
Stations<- sf::read_sf("./02_Data/03_Vector_data/Station_coordinates.shp")

# Filtering Stations according with the avaibility of ground truth data
Stations<- Stations[Stations$Station_na%in%c(toupper(unique(all_GTD$station))),]

#
# Basin View
tmap_mode("plot")
bbox_stations<- st_buffer(st_as_sfc(st_bbox(Stations)),2)
basemap <- 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}'
osm_NLD <- read_osm(bb(Catchment_area, ext=1.1), type = basemap)

tm_shape(read_osm(Catchment_area, ext=1.5), type = basemap) + tm_rgb(alpha =0.6)+
  tm_shape(Catchment_area) +
  tm_polygons(col = "lightblue",alpha=0.4,
              border.col = "gray15", lwd = 2, lty = "dashed")+
  tm_shape(bbox_stations) +
  tm_polygons(col = "red",alpha=0.6)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position=c("center", "bottom"))+
  tm_credits("F.Corvalan, 2023",size = 0.5,position=c("RIGHT","BOTTOM"))+
  tm_layout(legend.position = c("LEFT","TOP"),legend.bg.color = "white", 
            legend.bg.alpha = 1,legend.frame = "gray50", 
            main.title = "Soil Moisture Stations",main.title.size=1,
            main.title.position=c("center"))

## Local View

tm_shape(read_osm(bbox_stations, ext=1.5,type = basemap)) + tm_rgb(alpha =0.8)+
  #tm_shape(Catchment_area) +
  #tm_polygons(col = "lightblue",alpha=0.4,
  #            border.col = "gray15", lwd = 2, lty = "dashed")+
  tm_shape(Stations)+tm_dots(col="Land_use",shape=21,title= "Land use",border.col = "black",
                             scale=1,border.lwd = 0.5,size=0.3,border.alpha=1)+
  tm_text(text = "Station_na", size = 0.7,id="Station_na",
          bg.alpha =1, root=4, size.lowerbound = 10, shadow = T,print.tiny=T,
          auto.placement = 1)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position=c("center", "bottom"))+
  tm_credits("F.Corvalan, 2023",size = 0.5,position=c("RIGHT","BOTTOM"), bg.color = "white",
             bg.alpha = 0.6)+
  tm_layout(legend.position = c("LEFT","BOTTOM"),legend.bg.color = "white", 
            legend.bg.alpha = 1,legend.frame = "gray50", 
            main.title = "Soil Moisture Stations",main.title.size=1,
            main.title.position=c("center"))

