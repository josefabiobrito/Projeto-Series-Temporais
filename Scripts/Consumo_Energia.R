# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${Consumo_Energia.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 14/09/2025
# DESCRIÇÃO: ANÁLISE DO CONSUMO RESIDÊNCIAL DE ENERGIA NO BRASIL E ESTADOS 

#Bibliotecas
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(forecast)
library(ggplot2)
library(openxlsx)

#Carregando dados
df_raw <- read.csv("C:/Users/josef/OneDrive/Documentos/PUB/Projeto-Series-Temporais/Datasets/CONSUMO RESIDENCIAL DE ENERGIA POR UF.csv")

#Criação das séries temporais por estado
TSs_ufs<-list()
for (uf in df_raw$UF){
  linha_uf<-df_raw%>%
    filter(UF ==uf)%>%
    select(-UF)%>%
    as.numeric()
  ts_uf<-ts(linha_uf,start=c(2004,1), frequency = 12)
  TSs_ufs[[uf]]<-ts_uf
}

#Gráfico inicial por estado
for (nome_uf in names(TSs_ufs)){
  ts_uf<-TSs_ufs[[nome_uf]]
  plot<-autoplot(ts_uf, ylab = 'Consumo(MWh')+
    labs(title = str_glue("Consumo residêncial de Energia em {nome_uf}"),
         subtitle ="Fonte: EPE " )+
    geom_line(size = 0.9, colour = "blue")+
    theme_minimal()
  show(plot)
  #ggsave(filename = str_glue("Energia_{uf$UF[1]}.png"),
  #       plot = plot,
  #       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
  #       width = 8,
  #       height = 6,
  #       units = "in",
  #       dpi = 300)
}


#Correlogramas
for (nome_uf in names(TSs_ufs)){
  plot_acf<- ggAcf(TSs_ufs[[nome_uf]], lag.max = 24, type = 'correlation')+
    labs(title = str_glue("Autocorrelação da série de consumo de energia - {nome_uf}"))
  show(plot_acf)
}

for (nome_uf in names(TSs_ufs)){
  plot_pacf<- ggAcf(TSs_ufs[[nome_uf]], lag.max = 24, type = 'partial')+
    labs(title = str_glue("Autocorrelação parcial da série de consumo de energia - {nome_uf}"))
  show(plot_pacf)
}


#Ajuste de modelos
for (nome_uf in names(TSs_ufs)) {
  
  serie_bruta <- TSs_ufs[[nome_uf]]
  serie_treino <- head(serie_bruta ,-12)
  serie_teste <-tail(serie_bruta,12)
  
  mod_auto <- auto.arima(serie_treino, lambda = 0)
  
  
  mod_manual1 <- Arima(serie_treino, order = c(2, 1, 0),seasonal = c(0,1,1), lambda = 0)
  
  
  mod_manual2 <- Arima(serie_treino, order = c(1, 1, 1),seasonal = c(1,1,0), lambda = 0)
  
  
  mod_manual3 <- Arima(serie_treino, order = c(0, 2, 1),seasonal = c(1,1,1), lambda = 0)
  
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
  cat(str_glue(" ESTADO: {nome_uf} "))
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
    labs(title = str_glue("Previsão Consumo Energia-{nome_uf} vs Realidade"),
         subtitle = str_glue("Modelo:{forecast:::arima.string(melhor_modelo)}"))
  show(plot)
}
