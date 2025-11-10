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

DADOS_GERAL<-read.csv2("C:/Users/josef/OneDrive/Documentos/PUB/Projeto-Series-Temporais/Datasets/Dados_met_SC.csv")
head(DADOS_GERAL,n=10)

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






