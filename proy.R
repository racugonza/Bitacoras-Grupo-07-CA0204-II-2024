library(tidyverse)
library(knitr)
library(readr)
library(readxl)
library(lubridate)
library(dplyr)
library(TTR)
library(openxlsx)
library(nortest)
library(readxl)
library(xtable)
library(fitdistrplus)

reclamos <- read_excel("C:/Users/racug/Downloads/Reporte_Siniestralidad_SOA.xlsx", 
                                         sheet = "Accidentes")

reclamos$Fechas <- seq(as.Date("2017-01-01"), as.Date("2024-08-01"), by = "month")
anos <- c(rep(2017, 12), rep(2018, 12), rep(2019, 12), rep(2020, 12),
          rep(2021, 12), rep(2022, 12), rep(2023, 12), rep(2024, 8))
Mes2 <- c(rep(1:12, 8))

Mes <- Mes2[1:(length(Mes2) - 4)]

reclamos$Year <- anos
reclamos$Mes <- Mes
#Los valores mensuales permiten ver estacionalidad y los anuales tendencia.
reclamoss <- head(reclamos)
reclamos2 <- reclamoss[,1:7]
reclamos3 <- reclamoss[,7:15]

x.rescale <- xtable(reclamos2)
print(x.rescale, scalebox = 0.7,include.rownames = FALSE)

x.rescale <- xtable(reclamos3)
print(x.rescale, scalebox = 0.7,include.rownames = FALSE)
ggplot(reclamos)+geom_point(aes(x=Fecha, y=Total))+geom_line(aes(x=Fecha, y=Total), color="blue")+geom_line(aes(x=Fecha, y=decompose(ts(Total, start = c(2015, 1), frequency = 12), type="multiplicative")$trend), colour = "red")+labs(title="Número de reclamos totales por mes.",
                                                                                                                                                                                                                                        subtitle = "Reclamos Seguro Obligatorio de Vehículos Automotores (SOA)",
                                                                                                                                                                                                                                        caption = "Fuente: Elaboración propia con datos de SUGESE")+
  xlab("Fecha de reclamo")+ 
  ylab("Total de reclamos")

ggplot(mutate(reclamos, Year = as.character(Year))) + 
  geom_line(aes(
  x = as.factor(Mes),
  y = (Total - lag(Total)) / lag(Total),
  color = Year, 
  group=Year
), size = 3) + 
  facet_wrap(~Year, ncol = 4)+
  labs(title = "Tasa de cambio mensual de reclamos al SOA",
       caption = "Fuente: Elaboración propia con datos de SUGESE.") +
  xlab("Mes de reclamo") +
  # scale_x_discrete(
  # labels = c(
  #   "Enero",
  #   "Febrero",
  #   "Marzo",
  #   "Abril",
  #   "Mayo",
  #   "Junio",
  #   "Julio",
  #   "Agosto",
  #   "Setiembre",
  #   "Octubre",
  #   "Noviembre",
  #   "\ \ Diciembre"
  # )
# ) +
  ylab("Tasa de cambio") +
  cowplot::theme_minimal_hgrid()+
theme(legend.position = "none")
  
  
  theme(legend.background = element_rect(fill = "lightblue", # Fondo
                                                                                                        colour = 1))+        # Borde
  theme(legend.title = element_text(family = "Roboto",
                                    color = "blue", 
                                    size = 10,
                                    face = 2))

ggplot(pivot_longer(select(reclamos, Motos, Particular, Fecha), cols = c("Particular", "Motos"), names_to = "Tipo", values_to = "Reclamos"), aes(x=Fecha, y=Reclamos, colour = Tipo))+geom_line()+geom_point()+  labs(title="Reclamos mensuales al SOA, vehículos particulares y motos.",
                                                                                                                                                                                                                      caption = "Fuente: Elaboración propia con datos de SUGESE")+
  xlab("Fecha de reclamo") + 
  ylab("Total de reclamos")+
  theme(legend.position="bottom")+scale_color_hue(labels = c( "Motos","Auto Particular"))+theme(legend.background = element_rect(fill = "lightblue", # Fondo
                                                                                                                                 colour = 1))+        # Borde
  theme(legend.title = element_text(family = "Roboto",
                                    color = "blue", 
                                    size = 10,
                                    face = 2))

ggplot(mutate(reclamos, Year = as.character(Year), Mes = as.integer(Mes) ))+geom_point(aes(x=Mes, y=Total, color=Year))+
  geom_line(aes(x=Mes, y=Total, color=Year))+labs(title="Número de reclamos totales por año",
                                                  subtitle = "Reclamos Seguro Obligatorio de Vehículos Automotores (SOA)",
                                                  caption = "Fuente: Elaboración propia con datos de SUGESE)")+
  xlab("Mes de reclamo") + 
  ylab("Total de reclamos")+theme(legend.position="bottom")



ggplot(pivot_longer(select(reclamos, -No_identificado, -Equipo_Especial, - Particular, -Motos), cols = c("Taxis",  "Carga_Liviana", "Carga_Pesada", "Bus_remunerado"), names_to = "Tipo", values_to = "Reclamos"), aes(x=Fecha, y=Reclamos, colour = Tipo))+geom_line()+
  labs(title="Número de reclamos totales por mes de Buses, Taxis, Carga Pesada y Liviana.",
       subtitle = "Reclamos Seguro Obligatorio de Vehículos Automotores (SOA)",
       caption = "Fuente: Elaboración propia con datos de SUGESE")+
  xlab("Fecha de reclamo") + 
  ylab("Total de reclamos")+theme(legend.position="bottom")+scale_color_hue(labels = c( "Bus Remunerado","Carga Liviana","Carga Pesada","Taxis"))+theme(legend.background = element_rect(fill = "lightblue", # Fondo
                                                                                                                                                                                         colour = 1))+        # Borde
  theme(legend.title = element_text(family = "Roboto",
                                    color = "blue", 
                                    size = 10,
                                    face = 2))
a<-ggplot(reclamos, aes(x = Total)) +
  geom_density(color="#8B0A50")+
  labs(title="Costa Rica: Densidad del número total de reclamos por mes entre \n Enero 2017 y Agosto 2024",
       subtitle = "Reclamos Seguro Obligatorio de Vehículos Automotores (SOA)",
       caption = "Fuente: Superintendencia General de Seguros(SUGESE)")+
  xlab("Número de reclamos mensual") + 
  ylab("Densidad")+theme(legend.position="bottom")+ theme_minimal()

plot(ecdf(reclamos$Total),main = "Costa Rica, función de acumulación empírica del número total \n de reclamos por mes entre Enero 2017 y Agosto 2024", sub="Fuente: Superintendencia General de Seguros(SUGESE)",xlab="Número de Reclamos Mensual", ylab="Distribución Empírica",cex.main = 0.9,cex.lab=0.8,cex.sub =0.7,col="#8B0A50")



#codigo para hacer las particiones para pruebas X_2
muestra <- sort(reclamos$Total)  
n <- length(muestra) 
c <- qnorm(0.05, lower.tail=FALSE) 
m <- ceiling(4*(((2*n^2)/c^2)^0.2)) 
longitud <- max(muestra)/m 
lim_sup <- append(seq(136.6087, max(muestra), by=136.6087), max(muestra)) 

tabla <- table(cut(muestra, append(seq(0, max(muestra), by=136.6087), max(muestra)))) 
(seq(0, max(muestra), by=136.6087)) 
data_tabla<-as.data.frame.table(tabla) 
data_tabla <- data_tabla[9:23,]
cortes<-lim_sup[8:23]
val_obs_esp <- data_tabla

#*


lambda <- mean(reclamos$Total) #EMV de una poisson
#probabilidad esperada de una poisson
ppoi <- lapply(X=2:length(cortes),function(X){
  ppois(cortes[X],lambda)-ppois(cortes[X-1],lambda)
} )
ppoi<- unlist(ppoi)


#valores esperados de la poisson
val_obs_esp$EPOI <- n*ppoi

pnbin   <- lapply(X=2:length(cortes), 
                  function(X){pnbinom(cortes[X], size=29.00602, mu=2397.30604) - pnbinom(cortes[X-1],size=29.00602, mu=2397.30604)})
pnbin   <- unlist(pnbin)
#valores esperabados de la binomial negativa
val_obs_esp$ENBIN <- n*pnbin

#prueba chi cuadrado
#para negativa binomial
X_neg_binomial<-chisq.test(val_obs_esp$Freq,p=pnbin, rescale.p = TRUE, correct = TRUE)
X_poisson<-chisq.test(val_obs_esp$Freq,p=ppoi, rescale.p = TRUE, correct = TRUE)
media <- mean(muestra)
desv <- sd(muestra)


ks.test(muestra, "pnorm",mean=media,sd=desv)
ks.test(muestra, "pexp",1/media)
fitdist(muestra, "gamma")
ks.test(muestra, "pgamma",shape=28.6,scale=0.012)
fitdist(muestra, "nbinom")

ks.test(muestra, "pnbinom",size=29.00602,mu=2397.30604)

resultado_prueba <- ad.test(muestra)


resultado_prueba