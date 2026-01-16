# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${AIDS_HTR}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 19/09/2025
# DESCRIÇÃO: ANÁLISE DOS CASOS DE AIDS NA POPULAÇÃO HÉTEROSEXUAL BRASILEIRA


#Bibliotecas
library(openxlsx)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

#Carregando dados
AIDS<- read.xlsx("Datasets/AIDS_HTR.xlsx")

#Tratamento dos dados
AIDS<- rename(AIDS, UF = UF.Notificação)
AIDS$Total<-NULL
ufs<- AIDS$UF
nomes<-list()
for (uf in ufs){
  df_uf<-filter(AIDS,UF == uf)
  df_uf <- df_uf %>%
    mutate(across(-UF, as.integer))
  df_uf<-pivot_longer(df_uf,
                      cols = -UF,
                      names_to = "Data",
                      values_to = 'Notificação')
  nomes[[uf]]<-df_uf
}

#Transformação em Série temporal e gráficos inciais
TSs_ufs<-list()

for (n in nomes){
  ts_uf<- ts(n$Notificação, start = 1980,frequency = 1)
  TSs_ufs[[n$UF[1]]]<-ts_uf
}
for (nome_uf in names(TSs_ufs)){
  ts_uf<-TSs_uf[[nome_uf]]
  plot<-autoplot(ts_uf, ylab = 'Notificação')+
    labs(title = str_glue("Notificação de AIDS em Heterossexuais {n$UF[1]}"),
         subtitle ="Fonte: DataSUS" )+
    geom_line(size = 0.9, colour = "red")+
    theme_minimal()
  show(plot)
}

#Autocorrelação das séries

for (nome_uf in names(TSs_ufs)) {
  plot <- ggAcf(TSs_ufs[[nome_uf]], lag.max = 20, type = 'correlation') +
    labs(title = str_glue("Autocorrelação da série de notificações de AIDS - {nome_uf}"))
  print(plot)
}

#Autocorrelação parcial das séries

for (nome_uf in names(TSs_ufs)) {
  plot <- ggAcf(TSs_ufs[[nome_uf]], lag.max = 20, type = 'partial') +
    labs(title = str_glue("Autocorrelação parcial da série de notificações de AIDS - {nome_uf}"))
  print(plot)
}

#Ajuste de modelos e seleção
for (nome_uf in names(TSs_ufs)) {
  
  serie_bruta <- TSs_ufs[[nome_uf]]
  serie_limpa <- head(serie_bruta,-1)
  serie_treino <- head(serie_limpa + 1,-5)
  serie_teste <-tail(serie_limpa+1,5)
  
  mod_auto <- auto.arima(serie_treino, lambda = 0, max.d = 2)
  
  # Modelo 2: Conservador (0,1,1)
  mod_manual1 <- Arima(serie_treino, order = c(0, 1, 1), include.drift = FALSE, lambda = 0)
  
  # Modelo 3: Flexível (0,1,2)
  mod_manual2 <- Arima(serie_treino, order = c(0, 1, 2), include.drift = FALSE, lambda = 0)
  
  # Modelo 4: (0,2,1)
  mod_manual3 <- Arima(serie_treino, order = c(0, 2, 1), include.drift = FALSE, lambda = 0)
  
  # Modelo 5: (2,1,0)
  mod_manual4 <- Arima(serie_treino, order = c(2, 1, 0), include.drift = FALSE, lambda = 0)
  
  modelos<-list(mod_auto, mod_manual1, mod_manual2, mod_manual3, mod_manual4)
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
    extrair_metricas(mod_manual3, serie_teste),
    extrair_metricas(mod_manual4, serie_teste)
    
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
  plot<-autoplot(forecast(melhor_modelo, h=5)) +
    autolayer(serie_teste, series="Dados Reais") +
    labs(title = str_glue("Previsão Notificação AIDS-{nome_uf} vs Realidade"),
         subtitle = str_glue("Modelo:{forecast:::arima.string(melhor_modelo)}"))
  show(plot)
}


