# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${Desemprego_Brasil.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 18/11/2025
# DESCRIÇÃO: ANÁLISE COTAÇÃO DO DOLAR 2011-2025

#Bibliotecas
library(rbcb)
library(forecast)
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
library(xts)
library(patchwork)

#Carregando dados
dados_dolar<-get_currency("USD","2011-08-01","2025-10-18", "data.frame")
#Criação das séries temporais
TSs<-list()
ts_compra<-ts(dados_dolar$bid, 
              start = c(year(dados_dolar$date[1]),yday(dados_dolar$date[1])), 
              frequency = 252)
TSs[['Compra']]<-ts_compra
ts_venda<-ts(dados_dolar$ask, 
              start = c(year(dados_dolar$date[1]),yday(dados_dolar$date[1])), 
              frequency = 252)
TSs[['Venda']]<-ts_venda

#Gráficos iniciais
plot_bid<-autoplot(ts_compra)+
  geom_line(size = 0.5, color = 'blue')+
  labs(title = "Cotação de compra do Dólar",
       subtitle ="Fonte: Banco Central do Brasil" )+
  ylab("R$")+
  theme_minimal()
#ggsave(filename = "Compra_dolar.png",
#       plot = plot_bid,
#       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
#       width = 8,
#       height = 6,
#       units = "in",
#       dpi = 300)
show(plot_bid)

plot_ask<-autoplot(ts_venda)+
  geom_line(size = 0.5, color = 'blue')+
  labs(title = "Cotação de venda do Dólar",
       subtitle ="Fonte: Banco Central do Brasil" )+
  ylab("R$")+
  theme_minimal()
#ggsave(filename = "Venda_dolar.png",
#       plot = plot_ask,
#       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
#       width = 8,
#       height = 6,
#       units = "in",
#       dpi = 300)
show(plot_ask)

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

melhores_modelos<-list()
#Ajuste de modelos
for (nome in names(TSs)) {
  
  serie_bruta <- TSs[[nome]]+1
  serie_treino <- head(serie_bruta ,-20)
  serie_teste <-tail(serie_bruta,20)
  
  mod_auto <- auto.arima(serie_treino, lambda = 0)
  
  
  mod_manual1 <- Arima(serie_treino, order = c(0, 1, 2), lambda = 0)
  
  
  mod_manual2 <- Arima(serie_treino, order = c(0, 1, 1), lambda = 0)
  
  
  mod_manual3 <- Arima(serie_treino, order = c(1, 1, 1), lambda = 0)
  
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
  melhores_modelos[[nome]]<-melhor_modelo
  plot<-autoplot(forecast(melhor_modelo, h=length(serie_teste))) +
    autolayer(serie_teste, series="Dados Reais") +
    labs(title = str_glue("Previsão-{nome} vs Realidade"),
         subtitle = str_glue("Modelo:{forecast:::arima.string(melhor_modelo)}"))
  show(plot)
}

for (nome in names(melhores_modelos)){
  modelo <- melhores_modelos[[nome]]
  
  if (is.null(modelo)) {
    next
  }
  
  cat("\n============================================================\n")
  cat(str_glue(" {nome} | MODELO: {forecast:::arima.string(modelo)} "))
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

