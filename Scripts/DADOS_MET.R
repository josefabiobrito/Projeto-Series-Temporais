# PROJETO: PUB- SÉRIES TEMPORAIS
# ARQUIVO: DADOS_MET.R
# AUTOR: José Fábio Viana de Brito
# DATA: 07/10/2025
# DESCRIÇÃO: ANÁLISE DE DADOS METEOROLÓGICOS DE SÃO CARLOS
#            2022 - 07/10/2025
library(ggplot2)
library(forecast)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)
library(tsibble)

DADOS_2025<-read.csv2("C:/Users/josef/Downloads/dados_met_2025.csv", encoding = 'latin1')
DADOS_2024<-read.csv2("C:/Users/josef/Downloads/dados_met_2024.csv", encoding = 'latin1')
DADOS_2023<-read.csv2("C:/Users/josef/Downloads/dados_met_2023.csv", encoding = 'latin1')
DADOS_2022<-read.csv2("C:/Users/josef/Downloads/dados_met_2022.csv", encoding = 'latin1')
DADOS_2021<-read.csv2("C:/Users/josef/Downloads/dados_met_2021.csv", encoding = 'latin1')
DADOS_2020<-read.csv2("C:/Users/josef/Downloads/dados_met_2020.csv", encoding = 'latin1')
DADOS_2019<-read.csv2("C:/Users/josef/Downloads/dados_met_2019.csv", encoding = 'latin1')
DADOS_2018<-read.csv2("C:/Users/josef/Downloads/dados_met_2018.csv", encoding = 'latin1')
DADOS_2017<-read.csv2("C:/Users/josef/Downloads/dados_met_2017.csv", encoding = 'latin1')
DADOS_2016<-read.csv2("C:/Users/josef/Downloads/dados_met_2016.csv", encoding = 'latin1')
DADOS_2015<-read.csv2("C:/Users/josef/Downloads/dados_met_2015.csv", encoding = 'latin1')

DADOS_2O25<-as.Date(DADOS_2025$Data)
DADOS_2O24<-as.Date(DADOS_2024$Data)
DADOS_2O23<-as.Date(DADOS_2023$Data)
DADOS_2O22<-as.Date(DADOS_2022$Data)
DADOS_2O21<-as.Date(DADOS_2021$Data)
DADOS_2O20<-as.Date(DADOS_2020$Data)
DADOS_2O19<-as.Date(DADOS_2019$Data)
DADOS_2O18<-as.Date(DADOS_2018$Data)
DADOS_2O17<-as.Date(DADOS_2017$Data)
DADOS_2O16<-as.Date(DADOS_2016$Data)
DADOS_2O15<-as.Date(DADOS_2015$Data)


DADOS_GERAL<-rbind(DADOS_2015,DADOS_2016,DADOS_2017, DADOS_2018, DADOS_2019,DADOS_2020,DADOS_2021,DADOS_2022,DADOS_2023,DADOS_2024,DADOS_2025)
DADOS_GERAL <- DADOS_GERAL %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

DADOS_GERAL<-DADOS_GERAL%>%
  mutate(Data = dmy(Data))%>%
  group_by(Data)%>%
  summarise(
    Temperatura_Media = mean(Temp..Ins...C., na.rm = TRUE),
    Amplitude_Media = mean((Temp..Max...C.-Temp..Min...C.),na.rm = TRUE),
    Umidade_Media = mean(Umi..Ins....., na.rm = TRUE),
    Vol_Chuva = sum(Chuva..mm., na.rm = TRUE)
  )
head(DADOS_GERAL, n=10)
write.csv2(DADOS_GERAL, "C:/Users/josef/OneDrive/Documentos/PUB/Projeto-Series-Temporais/Datasets/Dados_Met_SC.csv", row.names = FALSE)



DADOS_RESUM<-DADOS_GERAL%>%
  mutate(ano_mes = floor_date(Data, "month"))%>%
  group_by(ano_mes)%>%
  summarise(
    Temperatura_Media = mean(Temperatura_Media, na.rm = TRUE),
    Amplitude_Media = mean(Amplitude_Media,na.rm = TRUE),
    Umidade_Media = mean(Umidade_Media, na.rm = TRUE),
    Vol_Chuva = sum(Vol_Chuva, na.rm = TRUE))

mes_inicio<- month(DADOS_RESUM$ano_mes[1])
ano_inicio<-year(DADOS_RESUM$ano_mes[1])

ts_temperatura<- ts(DADOS_RESUM$Temperatura_Media,
                      start = c(ano_inicio,mes_inicio),
                      frequency = 12)
ts_amplitude<- ts(DADOS_RESUM$Amplitude_Media,
                      start = c(ano_inicio,mes_inicio),
                      frequency = 12)
ts_umidade<- ts(DADOS_RESUM$Umidade_Media,
                      start = c(ano_inicio,mes_inicio),
                      frequency = 12)
ts_chuva<- ts(DADOS_RESUM$Vol_Chuva,
                      start = c(ano_inicio,mes_inicio),
                      frequency = 12)

plot_temp<-autoplot(ts_temperatura)+
  labs(y = "Temperatura (°C)", 
       title = "Temperatura média mensal em São Carlos 2022-2025", 
       subtitle = "Fonte: INMET")+
  geom_line(size = 0.3, colour = 'blue')+
  theme_minimal()
show(plot_temp)
ggsave(filename = "Temperatura_SC.png",
         plot = plot_temp,
         path = "C:/Users/josef/OneDrive/Documentos/PUB/Projeto-Series-Temporais/Gráficos",
         width = 8,
         height = 6,
         units = "in",
         dpi = 300)


plot_amp<-autoplot(ts_amplitude)+
  labs(y = "Diferença (°C)", 
       title = "Amplitude média mensal em São Carlos 2022-2025", 
       subtitle = "Fonte: INMET")+
  geom_line(size = 0.3, colour = 'blue')+
  theme_minimal()
show(plot_amp)
ggsave(filename = "Amplitude_SC.png",
         plot = plot_amp,
         path = "C:/Users/josef/OneDrive/Documentos/PUB/Projeto-Series-Temporais/Gráficos",
         width = 8,
         height = 6,
         units = "in",
         dpi = 300)

plot_umid<-autoplot(ts_umidade)+
  labs(y = "Umidade (%)", 
       title = "Umidade média mensal em São Carlos 2022-2025", 
       subtitle = "Fonte: INMET")+
  geom_line(size = 0.3, colour = 'blue')+
  theme_minimal()
show(plot_umid)
ggsave(filename = "Umidade_SC.png",
         plot = plot_umid,
         path = "C:/Users/josef/OneDrive/Documentos/PUB/Projeto-Series-Temporais/Gráficos",
         width = 8,
         height = 6,
         units = "in",
         dpi = 300)

plot_chuva<-autoplot(ts_chuva)+
  labs(y = "Chuva (mm)", 
       title = "Volume mensal de chuva em São Carlos 2022-2025", 
       subtitle = "Fonte: INMET")+
  geom_line(size = 0.3, colour = 'blue')+
  theme_minimal()
show(plot_chuva)
ggsave(filename = "Chuva_SC.png",
         plot = plot_chuva,
         path = "C:/Users/josef/OneDrive/Documentos/PUB/Projeto-Series-Temporais/Gráficos",
         width = 8,
         height = 6,
         units = "in",
         dpi = 300)






