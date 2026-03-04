# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${Desemprego_Brasil.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 12/09/2025
# DESCRIÇÃO: ANÁLISE DO CONSUMO E PRODUÇÃO DE PETRÓLEO NO BRASIL

#Bibliotecas
library(rbcb)
library(forecast)
library(ggplot2)
library(readr)
library(dplyr)
library(patchwork)

#Carregando dados
TSs<-list()
ts_producao<-get_series(
  code = c("Producao_Total" = 1391),
  start_date = "1979-01-31",
  end_date = "2025-10-30",
  as = "ts"
)
TSs[['Produção']]<-ts_producao
ts_consumo<-get_series(
  code = c("Consumo_Total" = 1398),
  start_date = "1979-01-31",
  end_date = "2025-10-30",
  as = "ts"
)
TSs[['Consumo']]<- ts_consumo


#Gráficos inciais
plot_prod<-autoplot(ts_producao)+
  geom_line(size = 0.9, color = 'blue')+
  labs(title = "Produção de derivados de petróleo total",
       subtitle= "Fonte: Agência Nacional do Petróleo, Gás Natural e Biocombustíveis do Brasil")+
  ylab("Barris/dia (mil)")+
  theme_minimal()
show(plot_prod)
#ggsave(filename = "Producao_petroleo.png",
#       plot = plot_prod,
#       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
#       width = 8,
#       height = 6,
#       units = "in",
#       dpi = 300)
  

plot_cons<-autoplot(ts_consumo)+
  geom_line(size = 0.5, color = 'blue')+
  labs(title = "Consumo de derivados de petróleo total",
       subtitle= "Fonte: Agência Nacional do Petróleo, Gás Natural e Biocombustíveis do Brasil")+
  ylab("Barris/dia (mil)")+
  theme_minimal()
show(plot_cons)
#ggsave(filename = "Consumo_petroleo.png",
#       plot = plot_cons,
#       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
#       width = 8,
#       height = 6,
#       units = "in",
#       dpi = 300)

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
  serie_treino <- head(TSs[[nome]] ,-6)
  serie_teste <-tail(TSs[[nome]],6)
  
  mod_auto <- auto.arima(serie_treino)
  
  mod_manual1 <- Arima(serie_treino, order = c(2, 1, 2))
  
  mod_manual2 <- Arima(serie_treino, order = c(2, 1, 2),seasonal = c(1,1,1))
  
  mod_manual3 <- Arima(serie_treino, order = c(1, 1, 1))
  
  mod_manual4 <- Arima(serie_treino, order = c(1, 1, 1),seasonal = c(1,0,0))
  
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




