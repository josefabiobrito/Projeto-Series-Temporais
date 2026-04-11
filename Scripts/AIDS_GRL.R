# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${AIDS_GRL}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 19/09/2025
# DESCRIÇÃO: ANÁLISE DOS CASOS DE AIDS NA POPULAÇÃO GERAL BRASILEIRA

#Bibliotecas
library(openxlsx)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(tidyverse)
library(patchwork)

#Carregar dados
AIDS<- read.xlsx("Datasets/AIDS_GRL.xlsx")
AIDS<- rename(AIDS, UF = UF.Notificação)

#Tratar dados
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

#Transformação em série temporal e plotar gráficos iniciais
TSs_ufs<-list()

for (n in nomes){
  ts_uf<- ts(n$Notificação, start = 1980,frequency = 1)
  TSs_ufs[[n$UF[1]]]<-ts_uf
}

for(nome_uf in names(TSs_ufs)){
  ts_uf<-TSs_ufs[[nome_uf]]
  plot<-autoplot(ts_uf, ylab = 'Notificação')+
    labs(title = str_glue("Notificação de AIDS {n$UF[1]}"),
         subtitle ="Fonte: DataSUS" )+
    geom_line(size = 0.9, colour = "red")+
    theme_minimal()
  show(plot)
  #ggsave(filename = str_glue("AIDS_GRL_{n$UF[1]}.png"),
  #       plot = plot,
  #       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
  #       width = 8,
  #       height = 6,
  #       units = "in",
  #       dpi = 300)
}

#Autocorrelação das séries

for (nome_uf in names(TSs_ufs)) {
  
  ts_atual <- TSs_ufs[[nome_uf]]
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
      title = str_glue("Autocorrelação e Autocorrelação Parcial (AIDS) - {nome_uf}"),
      subtitle = str_glue("Número de diferenciações: {d}"),
      theme = theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11)
      )
    )
  
  print(plot_final)
}

#Ajuste de modelos e seleção

melhores_modelos<-list()

for (nome_uf in names(TSs_ufs)) {
  serie_bruta <- TSs_ufs[[nome_uf]]
  serie_limpa <- head(serie_bruta,-1)
  p<-as.integer(length(serie_limpa)*0.1)
  serie_treino <- head(serie_limpa + 1,-p)
  serie_teste <-tail(serie_limpa+1,p)
  
  mod_auto <- auto.arima(serie_treino, lambda = 0, max.d = 2)
  
  mod_manual1 <- Arima(serie_treino, order = c(0, 1, 1), include.drift = FALSE, lambda = 0)
  
  mod_manual2 <- Arima(serie_treino, order = c(0, 1, 2), include.drift = FALSE, lambda = 0)
  
  mod_manual3 <- Arima(serie_treino, order = c(0, 2, 1), include.drift = FALSE, lambda = 0)
  
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
  melhores_modelos[[nome_uf]] <- melhor_modelo
  plot<-autoplot(forecast(melhor_modelo, h=length(serie_teste))) +
    autolayer(serie_teste, series="Dados Reais") +
    labs(title = str_glue("Previsão Notificação AIDS-{nome_uf} vs Realidade"),
         subtitle = str_glue("Modelo:{forecast:::arima.string(melhor_modelo)}"))
  show(plot)
}

for (nome in names(melhores_modelos)){
  modelo <- melhores_modelos[[nome]]
  
  if (is.null(modelo)) {
    next
  }
  
  cat("\n============================================================\n")
  cat(str_glue(" ESTADO: {nome} | MODELO: {forecast:::arima.string(modelo)} "))
  cat("\n============================================================\n")
  
  teste <- checkresiduals(modelo, plot = FALSE)
  print(teste)
  
  titulo_personalizado <- str_glue("Resíduos de {forecast:::arima.string(modelo)} - {nome}")
  
  grafico_residuos <- ggtsdisplay(residuals(modelo), 
                                  plot.type = "histogram", 
                                  main = titulo_personalizado)
  
  print(grafico_residuos)
  
  Sys.sleep(2)
}
