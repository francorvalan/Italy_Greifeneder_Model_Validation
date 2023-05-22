### Preprocessing Soil Moisture Data
# Filter by date
# Plots creation


library(ggplot2)
library(plotly)
library(htmlwidgets)


#setwd("D:/Documents/Fran/MAIE/14_Tutoria_de_investigacion")

#Soil_data<- list.files("04_Data/01_Soil_Moisture_Stations/02_Data/Original_data",pattern = ".csv$",full.names = T,recursive = T)
#SM_Stations <- read.csv2("./B_Greifeneder_Model_Validation/02_Data/01_Soil_Stations/Argentina/Estaciones.csv")
all_GTD <- read_csv("./02_Data/02_Processed_data/Ground_truth_data.csv")
all_GTD$wc_05_av <- rowMeans(all_GTD[,c("swc_wc_a_05_avg","swc_wc_b_05_avg","swc_wc_c_05_avg")])
all_GTD$wc_02_av <- rowMeans(all_GTD[,c("swc_wc_a_02_avg","swc_wc_b_02_avg","swc_wc_c_02_avg")])


output_jpeg= ("./04_Plots/GTD_Historical_data/")
if(!dir.exists(output_jpeg)) dir.create(output_jpeg,recursive = T)
output = ("./04_Plots/Dynamic_plots/GTD/")
if(!dir.exists(output)) dir.create(output,recursive = T)

#dir.create("04_Data/01_Soil_Moisture_Stations/02_Data/Processed/02_Plots",recursive = T)
#output_folder <- "04_Data/01_Soil_Moisture_Stations/02_Data/Processed/"
for(i in unique(all_GTD$Station)){
  data <- all_GTD[all_GTD$Station==i,]
  #data$Fecha<- as.POSIXct(data$Fecha,tz="America/Argentina/Buenos_Aires")
  #data$Fecha <- strptime(data$Fecha,format=c("%Y/%m/%d%H:%M:%S"),tz="America/Argentina/Buenos_Aires")
  Site <- i
  #ID <- SM_Stations[SM_Stations$NOMBRE==Site,]$ID_SITIO
  # if(length(Site)>1) print(Site)
  # #data<- data[data$Fecha>as.POSIXct("2018/01/01",format="%Y/%m/%d"),]
  # #data <- na_seadec(data)
  # #data$Year<- format(data$Fecha,"%Y")
  # #data$Month<- format(data$Fecha,"%m")
  # #data$Day<- format(data$Fecha,"%d")
  # #data$Hour<- format(data$Fecha,"%H")
  # #data$Minutes<- format(data$Fecha,"%M")
  # if(nrow(data)>0){
  #   write.csv2(data,paste0(output_folder,"/01_Data/","ID_",ID,"_",basename(Soil_data[i])))
  # }
  jpeg(paste0(output_jpeg,"Station",i,".jpg"),res=200, 
      width = 1200, height = 600,quality=100)
  p <- ggplot(data = data, aes(x = Date, y = wc_05_av)) +
    geom_area(fill = "lightskyblue",alpha=0.7)+
    geom_point(size=0.4,col="lightseagreen")+
    ggtitle(paste0("Nodo ",Site,", SMC : ",5,"cm"))+ylab("SMC m3/m3")+
    theme(plot.title = element_text(size = 10, face = "bold",hjust = 0.5))
  print(p)
  dev.off()
  # Si no hay datos a partir del 2018 la funcion de graficar falla
  tryCatch(
  ({ggplotly(p)

  htmlwidgets::saveWidget(
    widget = ggplotly(p),
    file = paste0(output,"Station",i,".html"), #the path & file name
    selfcontained = TRUE #creates a single html file
  )}),error = function(e){
    cat("Ocurrio un error en el Sitio", Site,conditionMessage(e), "\n")}
  )

}


### ANOMALIES DETECTION

# Convert df into a tibble
df <- as_tibble(data)

df_anomalized <- df %>%
  time_decompose(overall, merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()
df_anomalized %>% glimpse()
df_anomalized %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)

p1 <- df_anomalized %>%
  plot_anomaly_decomposition() +
  ggtitle("Freq/Trend = 'auto'")
p1

################
p2 <- df %>%
  time_decompose(overall,
                 frequency = "auto",
                 trend     = "2 weeks") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Trend = 2 Weeks (Local)")
# Show plots
p1
p2
#####################
p3 <- df %>%
  time_decompose(overall) %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Trend = 2 Weeks (Global)")
p3






