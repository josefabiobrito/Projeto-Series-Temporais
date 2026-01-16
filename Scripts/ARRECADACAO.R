# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${ARRECADACAO.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 22/09/2025
# DESCRIÇÃO: ANÁLISE DA  RECEITA DE ARRECADAÇÃO TRIBUTÁRIA 
#            BRUTA DA RECEITA FEDERAL BRASILEIRA

#Bibliotecas
library(ggplot2)
library(forecast)
library(tidyr)
library(dplyr)
library(stringr)
library(openxlsx)

#Carregando dados
ARRECADACAO<- read.xlsx("Datasets/arrecadacao-cnae.xlsx", sheet= "arrecadacao_total")

#Transformação em série temporal
ts_ARRECADACAO<-ts(ARRECADACAO$Receita,start = c(2016,01), frequency = 12)

#Gráfico inicial
plot<-autoplot(ts_ARRECADACAO,ylab = "Receita Bruta(1BR$)")+
  labs(title = "Arrecadação Bruta do Brasil",
       subtitle = "Fonte: Ministerio da Fazenda")+
  geom_line(size = 0.9, colour = "blue")+
  theme_minimal()
show(plot)
#ggsave(filename = "Arrecadação_Bruta.png",
#       plot = plot,
#       path = "Gráficos",
#       width = 8,
#       height = 6,
#       units = "in",
#       dpi = 300)

#Decomposição
ARRECADACAO_stl<-stl(ts_ARRECADACAO, s.window = 'periodic')
plot(ARRECADACAO_stl)

#Correlogramas
plot<-ggAcf(ts_ARRECADACAO, lag.max = 36, type = "correlation")
show(plot)

ts_diff <- diff(diff(ts_ARRECADACAO, lag=12), differences=1)
ggtsdisplay(ts_diff)

#Ajuste e seleção de modelos
treino <- head(ts_ARRECADACAO, -12)
teste  <- tail(ts_ARRECADACAO, 12)

fit_auto <- auto.arima(treino, lambda = 0)

fit_manual1 <- Arima(treino, 
                     order = c(0, 1, 1), 
                     seasonal = c(0, 1, 1),
                     lambda = 0)

fit_manual2 <- Arima(treino, 
                     order = c(1, 0, 1), 
                     seasonal = c(1, 0, 1),
                     include.drift = TRUE,
                     lambda = 0)
modelos<-list(fit_auto,fit_manual1,fit_manual2)

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

tabela_comparativa <- bind_rows(
  extrair_metricas(fit_auto, teste),
  extrair_metricas(fit_manual1, teste),
  extrair_metricas(fit_manual2, teste)
)

print(tabela_comparativa)

melhor_modelo <- modelos[[which.min(tabela_comparativa$AIC)]]
autoplot(forecast(melhor_modelo, h=12)) +
  autolayer(teste, series="Dados Reais") +
  labs(title = "Previsão Arrecadação bruta vs Realidade",
       subtitle = str_glue("Modelo:{forecast:::arima.string(melhor_modelo)}"))


