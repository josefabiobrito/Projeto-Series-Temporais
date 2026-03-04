# PROJETO: PUB- SÉRIES TEMPORAIS
# ARQUIVO: DADOS_MET.R
# AUTOR: José Fábio Viana de Brito
# DATA: 07/10/2025
# DESCRIÇÃO: ANÁLISE DE DADOS METEOROLÓGICOS DE SÃO CARLOS
#            2022 - 07/10/2025

#Bibliotecas
library(ggplot2)
library(forecast)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)
library(tsibble)
library(patchwork)

#Carregando dados
DADOS_GERAL<-read.csv2("C:/Users/josef/OneDrive/Documentos/PUB/Projeto-Series-Temporais/Datasets/Dados_met_SC.csv")
head(DADOS_GERAL,n=10)

#Transformação em dados mensais
DADOS_RESUM<-DADOS_GERAL%>%
  mutate(Data = ymd(Data)) %>%
  mutate(ano_mes = floor_date(Data, "month"))%>%
  group_by(ano_mes)%>%
  summarise(
    Temperatura_Media = mean(Temperatura_Media, na.rm = TRUE),
    Amplitude_Media = mean(Amplitude_Media,na.rm = TRUE),
    Umidade_Media = mean(Umidade_Media, na.rm = TRUE),
    Vol_Chuva = sum(Vol_Chuva, na.rm = TRUE))

mes_inicio<- month(DADOS_RESUM$ano_mes[1])
ano_inicio<-year(DADOS_RESUM$ano_mes[1])

TSs<-list()
#Séries temporais
ts_temperatura<- ts(DADOS_RESUM$Temperatura_Media,
                      start = c(ano_inicio,mes_inicio),
                      frequency = 12)
TSs[['Temperatura']]<-ts_temperatura
ts_amplitude<- ts(DADOS_RESUM$Amplitude_Media,
                      start = c(ano_inicio,mes_inicio),
                      frequency = 12)
TSs[['Amplitude']]<-ts_amplitude
ts_umidade<- ts(DADOS_RESUM$Umidade_Media,
                      start = c(ano_inicio,mes_inicio),
                      frequency = 12)
TSs[['Umidade']]<-ts_umidade
ts_chuva<- ts(DADOS_RESUM$Vol_Chuva,
                      start = c(ano_inicio,mes_inicio),
                      frequency = 12)
TSs[['Chuva']]<-ts_chuva

#Gráficos inciais
plot_temp<-autoplot(ts_temperatura)+
  labs(y = "Temperatura (°C)", 
       title = "Temperatura média mensal em São Carlos 2015-2025", 
       subtitle = "Fonte: INMET")+
  geom_line(size = 0.3, colour = 'blue')+
  theme_minimal()
show(plot_temp)
#ggsave(filename = "Temperatura_SC.png",
#         plot = plot_temp,
#         path = "C:/Users/josef/OneDrive/Documentos/PUB/Projeto-Series-Temporais/Gráficos",
#         width = 8,
#         height = 6,
#         units = "in",
#         dpi = 300)


plot_amp<-autoplot(ts_amplitude)+
  labs(y = "Diferença (°C)", 
       title = "Amplitude média mensal em São Carlos 2015-2025", 
       subtitle = "Fonte: INMET")+
  geom_line(size = 0.3, colour = 'blue')+
  theme_minimal()
show(plot_amp)
#ggsave(filename = "Amplitude_SC.png",
#         plot = plot_amp,
#         path = "C:/Users/josef/OneDrive/Documentos/PUB/Projeto-Series-Temporais/Gráficos",
#         width = 8,
#         height = 6,
#         units = "in",
#         dpi = 300)
#

plot_umid<-autoplot(ts_umidade)+
  labs(y = "Umidade (%)", 
       title = "Umidade média mensal em São Carlos 2015-2025", 
       subtitle = "Fonte: INMET")+
  geom_line(size = 0.3, colour = 'blue')+
  theme_minimal()
show(plot_umid)
#ggsave(filename = "Umidade_SC.png",
#         plot = plot_umid,
#         path = "C:/Users/josef/OneDrive/Documentos/PUB/Projeto-Series-Temporais/Gráficos",
#         width = 8,
#         height = 6,
#         units = "in",
#         dpi = 300)

plot_chuva<-autoplot(ts_chuva)+
  labs(y = "Chuva (mm)", 
       title = "Volume mensal de chuva em São Carlos 2015-2025", 
       subtitle = "Fonte: INMET")+
  geom_line(size = 0.3, colour = 'blue')+
  theme_minimal()
show(plot_chuva)
#ggsave(filename = "Chuva_SC.png",
#         plot = plot_chuva,
#         path = "C:/Users/josef/OneDrive/Documentos/PUB/Projeto-Series-Temporais/Gráficos",
#         width = 8,
#         height = 6,
#         units = "in",
#         dpi = 300)


#Correlogramas

for (nome in names(TSs)) {
  
  ts_atual <- TSs[[nome]]
  d <- ndiffs(ts_atual)
  
  if (d > 0) {
    ts_plot <- diff(ts_atual, differences = d)
  } else {
    ts_plot <- ts_atual
  }
  
  p1 <- ggAcf(ts_plot, lag.max = 20, type = 'correlation') +
    ggtitle(NULL)
  
  p2 <- ggAcf(ts_plot, lag.max = 20, type = 'partial') +
    ggtitle(NULL)
  
  plot_final <- (p1 / p2) +
    plot_annotation(
      title = str_glue("Autocorrelação e Autocorrelação Parcial-{nome}"),
      subtitle = str_glue("Número de diferenciações: {d}"),
      theme = theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11)
      )
    )
  
  print(plot_final)
}

#Ajuste de modelos
for (nome in names(TSs)) {
  
  serie_bruta <- TSs[[nome]]+1
  serie_treino <- head(serie_bruta ,-12)
  serie_teste <-tail(serie_bruta,12)
  
  mod_auto <- auto.arima(serie_treino, lambda = 0)
  
  
  mod_manual1 <- Arima(serie_treino, order = c(1, 1, 0),seasonal = c(0,1,1), lambda = 0)
  
  
  mod_manual2 <- Arima(serie_treino, order = c(0, 0, 0),seasonal = c(1,1,0), lambda = 0)
  
  
  mod_manual3 <- Arima(serie_treino, order = c(1, 1, 1),seasonal = c(1,1,1), lambda = 0)
  
  modelos<-list(mod_auto, mod_manual1, mod_manual2, mod_manual3)
  extrair_metricas <- function(modelo, dados_teste) {
    aic_val <- modelo$aic
    prev <- forecast(modelo, h = length(dados_teste))
    acc  <- accuracy(prev, dados_teste)
    rmse_val <- acc[2, "RMSE"]
    mase_val <- acc[2, "MASE"]
    nome <- forecast:::arima.string(modelo, padding = FALSE)
    
    return(data.frame(Modelo = nome, 
                      AIC = round(aic_val, 2), 
                      RMSE_Teste = round(rmse_val, 2), 
                      MASE_Teste = round(mase_val, 3)))
  }
  
  lista_resultados <- list(
    extrair_metricas(mod_auto, serie_teste),
    extrair_metricas(mod_manual1, serie_teste),
    extrair_metricas(mod_manual2, serie_teste),
    extrair_metricas(mod_manual3, serie_teste)
    
  )
  
  tabela_resultados <- bind_rows(lista_resultados[!sapply(lista_resultados, is.null)])
  
  cat("\n========================================\n")
  cat(str_glue(" {nome} "))
  cat("\n========================================\n")
  if (nrow(tabela_resultados) > 0) {
    print(tabela_resultados)
  } else {
    cat("Não foi possível ajustar modelos (possivelmente dados insuficientes).\n")
  }
  cat("\n")
  melhor_modelo <- modelos[[which.min(tabela_resultados$AIC)]]
  plot<-autoplot(forecast(melhor_modelo, h=length(serie_teste))) +
    autolayer(serie_teste, series="Dados Reais") +
    labs(title = str_glue("Previsão-{nome} vs Realidade"),
         subtitle = str_glue("Modelo:{forecast:::arima.string(melhor_modelo)}"))
  show(plot)
}

