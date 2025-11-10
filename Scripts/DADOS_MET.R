# PROJETO: PUB- SÉRIES TEMPORAIS
# ARQUIVO: DADOS_MET.R
# AUTOR: José Fábio Viana de Brito
# DATA: 07/10/2025
# DESCRIÇÃO: ANÁLISE DE DADOS METEOROLÓGICOS DE SÃO CARLOS
#            01/01/2025 - 07/10/2025
library(ggplot2)
library(forecast)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)
library(tsibble)

DADOS_2022<-read.csv2("Datasets/DADOS_MET_SC_2022.csv", encoding = "UTF-8")
DADOS_2023<-read.csv2("Datasets/DADOS_MET_SC_2023.csv", encoding = "UTF-8")
DADOS_2024<-read.csv2("Datasets/DADOS_MET_SC_2024.csv", encoding = "UTF-8")
DADOS_2025<-read.csv2("Datasets/DADOS_MET_SC_2025.csv", encoding = "UTF-8")

DADOS_2022$Data<- as.Date(DADOS_2022$Data, format = "%d/%m/%Y")
DADOS_2023$Data<- as.Date(DADOS_2023$Data, format = "%d/%m/%Y")
DADOS_2024$Data<- as.Date(DADOS_2024$Data, format = "%d/%m/%Y")
DADOS_2025$Data<- as.Date(DADOS_2025$Data, format = "%d/%m/%Y")

DADOS_GERAL<-rbind(DADOS_2022,DADOS_2023,DADOS_2024,DADOS_2025)

DADOS_RESUM<- DADOS_GERAL|>
  group_by(Data)|>
  summarise(
    Temperatura_Media = mean(Temp..Ins...C., na.rm = TRUE),
    Amplitude_Media = mean(Temp..Max...C., na.rm = TRUE)-mean(Temp..Min...C.,na.rm = TRUE),
    Umidade_Media = mean(Umi..Ins....., na.rm = TRUE),
    Vol_Chuva = sum(Chuva..mm., na.rm = TRUE)
  )


plot_temperatura <- ggplot(DADOS_RESUM, 
                           aes(x= Data, y=Temperatura_Media))+
  labs(y = "Temperatura (°C)", 
       title = "Temperatura média diária em São Carlos 2022-2025", 
       subtitle = "Fonte: INMET")+
  geom_line(size = 0.3, colour = 'blue')+
  theme_minimal()
show(plot_temperatura)

plot_amplitude <- ggplot(DADOS_RESUM, 
                         aes(x= Data, y=Amplitude_Media))+
  labs(y = "Amplitude térmica",
       title = "Amplitude térmica média diária em São Carlos 2022-2025", 
       subtitle = "Fonte: INMET")+
  geom_line(size = 0.3, colour = 'blue')+
  theme_minimal()
show(plot_amplitude)

plot_umidade <- ggplot(DADOS_RESUM, 
                         aes(x= Data, y=Umidade_Media))+
  labs(y = "Umidade (%)",
       title = "Umidade média diária em São Carlos 2022-2025", 
       subtitle = "Fonte: INMET")+
  geom_line(size = 0.3, colour = 'blue')+
  theme_minimal()
show(plot_umidade)

plot_chuva <- ggplot(DADOS_RESUM, 
                         aes(x= Data, y=Vol_Chuva))+
  labs(y = "Volume de chuva (mm)",
       title = "Volume diário de chuva em São Carlos 2022-2025", 
       subtitle = "Fonte: INMET")+
  geom_line(size = 0.3, colour = 'blue')+
  theme_minimal()
show(plot_chuva)



dia_inicio = day(DADOS_RESUM$Data[1])
mes_inicio = month(DADOS_RESUM$Data[1])
ano_inicio = year(DADOS_RESUM$Data[1])

ts_temperatura<- msts(DADOS_RESUM$Temperatura_Media,
                      start = c(ano_inicio,mes_inicio,dia_inicio),
                      seasonal.periods = c(7,365.25))
ts_amplitude<- msts(DADOS_RESUM$Amplitude_Media,
                      start = c(ano_inicio,mes_inicio,dia_inicio),
                      seasonal.periods = c(7,365.25))
ts_umidade<- msts(DADOS_RESUM$Umidade_Media,
                      start = c(ano_inicio,mes_inicio,dia_inicio),
                      seasonal.periods = c(7,365.25))
ts_chuva<- msts(DADOS_RESUM$Vol_Chuva,
                      start = c(ano_inicio,mes_inicio,dia_inicio),
                      seasonal.periods = c(7,365.25))


