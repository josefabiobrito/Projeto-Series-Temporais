# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${Hanseniase.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 19/09/2025
# DESCRIÇÃO: ANÁLISE DO NÚMERO DE NOTIFICAÇÕES DE HANSENIASE NO SUS

#Bibliotecas
library(openxlsx)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(patchwork)

#Carregando dados
HBR<-read.xlsx("Datasets/Hanseniase_Brasil.xlsx")

#Tratamento de dados
cols<- colnames(HBR)[2:length(colnames(HBR))]
for (col_name in cols) {
  HBR[[col_name]][HBR[[col_name]] == "-"] <- 0
}
HBR <- HBR %>%
  mutate(across(-UF, as.integer))

TT_BR<- filter(HBR, UF=="Total")
TT_BR<- pivot_longer(TT_BR,
                     cols = -UF,
                     names_to = "Data",
                     values_to = "Notificações")
#Criação de série temporal
TT_BR_TS<-ts(TT_BR$Notificações,start = 1970,frequency = 1)

#Gráfico inicial
autoplot(TT_BR_TS, ylab = "Notificações")+
  labs(title = "Número de Notificações de Hanseníase no Brasil",
       subtitle = "Fonte: SUS")+
  geom_line(size = 1.1, colour = 'blue')+
  theme_minimal()

#Correlogramas
plotAcf<- ggAcf(TT_BR_TS, lag.max = 24, type = "correlation")+
  labs(title = '')

plotPacf<- ggAcf(TT_BR_TS, lag.max = 24, type = 'partial')+
  labs(title = '')

plot_final<-(plotAcf/plotPacf)+
  plot_annotation(
    title = str_glue("Autocorrelação e Autocorrelação Parcial (AIDS HTR) - {nome_uf}"),
    subtitle = str_glue("Número de diferenciações: {d}"),
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  )

print(plot_final)


#Ajuste de Modelo
serie_treino <- head(TT_BR_TS ,-5)
serie_teste <-tail(TT_BR_TS,5)

mod_auto <- auto.arima(serie_treino, lambda = 0)


mod_manual1 <- Arima(serie_treino, order = c(1, 1, 0), lambda = 0)


mod_manual2 <- Arima(serie_treino, order = c(1, 2,  0), lambda = 0)


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
cat("Ajuste de Modelos- Notificações de Hanseníase")
cat("\n========================================\n")
if (nrow(tabela_resultados) > 0) {
  print(tabela_resultados)
} else {
  cat("Não foi possível ajustar modelos (possivelmente dados insuficientes).\n")
}
cat("\n")
melhor_modelo <- modelos[[which.min(tabela_resultados$AIC)]]
plot<-autoplot(forecast(mod_manual1, h=length(serie_teste))) +
  autolayer(serie_teste, series="Dados Reais") +
  labs(title = str_glue("Previsão Notificações vs Realidade"),
       subtitle = str_glue("Modelo:{forecast:::arima.string(mod_manual1)}"))
show(plot)

checkresiduals(melhor_modelo)