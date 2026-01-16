# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${Desemprego_Brasil.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 31/10/2025
# DESCRIÇÃO: ANÁLISE DA DÍVIDA LIQUIDA NACIONAL

#Bibliotecas
library(rbcb)
library(forecast)
library(ggplot2)
library(readr)
library(dplyr)

#Carregando dados
dados_divida <- get_series(
  code = c("Divida_Liquida_PIB" = 4504),
  start_date = "2001-12-31",
  end_date = "2025-10-30",
  as = "ts"
)

#Gráficos inciais
plot<-autoplot(dados_divida)+
  geom_line(size = 0.9, color = 'blue')+
  labs(title = "Dívida Liquida do setor público federal",
       subtitle= "Fonte: Banco Central do Brasil")+
  ylab("% PIB")+
  theme_minimal()
#ggsave(filename = "Divida_liquida_BR.png",
#       plot = plot,
#       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
#       width = 8,
#       height = 6,
#       units = "in",
#       dpi = 300)
show(plot)

#Correlogramas
plotAcf<- ggAcf(dados_divida, lag.max = 24, type = "correlation")+
  labs(title = 'Autocorrelação série da dívida pública')
show(plotAcf)

plotPacf<- ggAcf(dados_divida, lag.max = 24, type = 'partial')+
  labs(title = 'Autocorrelação parcial série da dívida pública')
show(plotPacf)

#Ajuste de Modelo

serie_treino <- head(dados_divida ,-12)
serie_teste <-tail(dados_divida,12)

mod_auto <- auto.arima(serie_treino, lambda = 0)


mod_manual1 <- Arima(serie_treino, order = c(2, 2, 0), lambda = 0)


mod_manual2 <- Arima(serie_treino, order = c(0, 2,  0), lambda = 0)


mod_manual3 <- Arima(serie_treino, order = c(1, 2, 2), lambda = 0)

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
cat("Ajuste de Modelos- Dívida pública")
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
  labs(title = str_glue("Previsão Dívida pública vs Realidade"),
       subtitle = str_glue("Modelo:{forecast:::arima.string(melhor_modelo)}"))
show(plot)
