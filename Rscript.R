
#Establecer directorio de trabajo
setwd("C:/Users/51952/Desktop/UNALM/R_TECNICASEST/COSTA_SUR/")

#Importacion de archivo de estacion (aqui se especifica la estacion a trabajar)
data <- read.table("ho00000830.txt", na.strings = -99.9) 
class(data)

#Establecemos nombres de columnas
colnames(data) <- c("Año", "Mes", "Dia", "PP", "Tmax", "Tmin")


#Creamos vector de fecha
data$Fecha <- as.Date(paste0(data$Año, "-", data$Mes, "-", data$Dia))

class(paste0(data$Año, "-", data$Mes, "-", data$Dia))  

#Tipo de objeto
class(data$Fecha)
head(data)

#Estadistica basica

#cantidad de NAs de cada columna
sum(data$PP)
sum(data$PP, na.rm=T)

sum(is.na(data$PP))
sum(is.na(data$Tmax))
sum(is.na(data$Tmin))

#MINIMO
min(data$PP, na.rm=T)
min(data$Tmax, na.rm=T)
min(data$Tmin, na.rm=T)
#Maximo
max(data$PP, na.rm=T)
max(data$Tmax, na.rm=T)
max(data$Tmin, na.rm=T)
#RANGO
range(data$PP, na.rm=T)

max(data$PP, na.rm=T) - min(data$PP, na.rm=T)

range(data$Tmax, na.rm=T)
max(data$Tmax, na.rm=T) - min(data$Tmin, na.rm=T)


#media
mean(data$PP, na.rm=T)
mean(data$Tmax, na.rm=T)
mean(data$Tmin, na.rm=T)
#mediana
median(data$PP, na.rm=T)

#cuartiles
quantile(data$PP, na.rm=T, c(0.25, 0.5, 0.75))

#rango intercuatilico Q3-Q1
IQR(data$PP, na.rm=T)
IQR(data$Tmax, na.rm=T)
IQR(data$Tmin, na.rm=T)

#desviacion estandar
sd(data$PP, na.rm=T)
sd(data$Tmax, na.rm=T)
sd(data$Tmin, na.rm=T)
#varianza
var(data$PP, na.rm=T)
sd(data$PP, na.rm=T)^2

#resumen estadistico
summary(data)

dev.off()
dev.new()

####GRAFICAR SERIES TEMPORALES
library(ggplot2)
ggplot(data, aes(x=Fecha, y=PP))+
  geom_point(col="blue")+ggtitle("Precipitación Estación ILO")

g1 <- ggplot(data, aes(x=Fecha, y=PP))+
  geom_point(col="green")+
  theme(plot.title=element_text(hjust=0.5, face="bold"))+
  xlab("Fecha")+ ylab("Precipitacion (mm)")+theme_bw()+ggtitle("Precipitación Estación ILO")

g2 <- ggplot(data, aes(x=Fecha, y=Tmax))+
  geom_line(col="red")+
  theme(plot.title = element_text(hjust=0.5, face="bold"))+
  xlab("Fecha")+ ylab("Temperatura mÃ¡x (Â°C)")+theme_bw()+ggtitle("Temperatura Máxima Estación ILO")

g3 <- ggplot(data, aes(x=Fecha, y=Tmin))+
  geom_line(col="blue")+
  theme(plot.title = element_text(hjust=0.5, face="bold"))+
  xlab("Fecha")+ ylab("Temperatura mÃ­n (Â°C)")+theme_bw()+ggtitle("Temperatura Mínima Estaciónn ILO")


#Graficos interactivos
install.packages("plotly")
library(plotly)
ggplotly(g1)
ggplotly(g2)
ggplotly(g3)

##Para unir graficos en un solo panel
install.packages("reshape2")
library(reshape2)

datos_melted <- melt(data, id.vars=c("Fecha"), measure.vars= c("Tmax","Tmin","PP"))
                  
grafico<-ggplot(data=datos_melted,aes(x=Fecha, y=value, color=variable))+
  geom_line(data=subset(datos_melted,variable %in% c("Tmax","Tmin")),size=0.8)+
  geom_point(data=subset(datos_melted,variable=="PP"),size=2)+
  labs(x="Fecha",y="")+
  scale_x_date(date_labels="%Y",date_breaks="5 years")+
  facet_wrap(~variable, scales="free_y", nrow=5)+
  theme_bw()+
  theme(legend.position="none")+
  theme(plot.title=element_text(hjust=0.5, face="plain",color="black"), strip.text=element_text(color="black",face="plain"))+
  theme(axis.title=element_text(face="plain"))+ggtitle("Serie temporal diaria Estación Punta Atico")+
  scale_color_manual(values=c("paleturquoise3", "thistle2", "palegreen2"))

grafico

#Guardar grafico de forma directa
ggsave(plot=grafico, file="serie_estacionpuntaatico.png", width=, height=8)



#GRAFICAR DIAGRAMA DE CAJAS
sum_na <- function(x) {if (all(is.na(x))==TRUE|sum(is.na(x))>5) {NA} else {sum(x,na.rm=TRUE)}}
mean_na <- function(x) {if (all(is.na(x))==TRUE|sum(is.na(x))>5) {NA} else {mean(x,na.rm=TRUE)}}

#Primera forma
install.packages("xts")
library(xts)

data$Fecha <- as.Date(data$Fecha)

datos_xts <- xts(data[,4:6], order.by =data$Fecha)

pp_month <- apply.monthly(datos_xts$PP, sum_na)
tmin_month <- apply.monthly(datos_xts$Tmin, mean_na)
tmax_month <- apply.monthly(datos_xts$Tmax, mean_na)

data_month <- data.frame(Fecha=as.Date(index(pp_month)), PP=coredata(pp_month), Tmax=coredata(tmax_month), Tmin=coredata(tmin_month))

#Segunda forma (mas efectiva)
install.packages("tidyverse")
library(tidyverse)
prom_m <- data %>% group_by(Año, Mes) %>%
  summarise(Tmax=mean_na(Tmax), Tmin= mean_na(Tmin), PP=sum_na(PP))

Fecha <- seq.Date(as.Date("1965-01-01"), as.Date("2019-12-01"), by="month")
prom_m <- data.frame(prom_m, Fecha)
head(prom_m, 15)


library(RColorBrewer)
prom_m$Mes <- rep(c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago","Sep","Oct","Nov","Dec"), 2019-1965+1)
prom_m$Mes <- factor(prom_m$Mes, levels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago","Sep","Oct","Nov","Dec"))

data_box <- melt(prom_m, id.vars=c("Mes"), measure.vars = c("Tmax", "Tmin", "PP"))

boxplot  <-  ggplot(data=data_box, aes(x = Mes, y = value ,fill=Mes))+
  geom_boxplot()+
  labs(x="Fecha",y="")+
  facet_wrap(~variable, scales="free_y", nrow=3)+
  theme_bw()+
  theme(legend.position="none")+
  theme(plot.title=element_text(hjust=0.5, face="plain",color="black"), strip.text=element_text(color="black",face="plain"))+
  theme(axis.title=element_text(face="plain"))+ggtitle("Diagrama de cajas mensual Estacion de Punta Atico")+scale_fill_manual(values=brewer.pal(n=12, name="Set3"))+
  stat_summary(fun=mean, geom="point", shape=18, color="red", size=2)

boxplot


#####CLIMOGRAMA
install.packages(hydroTSM)
library(hydroTSM)
library(zoo)

head(data)
data_serie <- zoo(data[,4:6],data$Fecha)
head(data_serie,10)
 
png("climograma3.png",width=12, height=7, res=700, units="in")  #guarda la imagen 
climograph(pcp=data_serie$PP, tmx=data_serie$Tmax, tmn=data_serie$Tmin, na.rm=TRUE,
           main="Estación Punta Atico",
           pcp.label="Precipitación (mm)",
           tmean.label="Temperatura (°C)")
dev.off()



