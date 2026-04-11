# PROJETO: PUB- SÉRIES TEMPORAIS
# ARQUIVO: SSP.R
# AUTOR: José Fábio Viana de Brito
# DATA: 07/10/2025
# DESCRIÇÃO: ANÁLISE DE DADOS CRIMINAIS DO ESTADO DE SÃO PAULO

#Bibliotecas
library(ggplot2)
library(forecast)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)
library(tsibble)
library(readr)
library(patchwork)

meses<- c('janeiro' = 1,'fevereiro' = 2,'março' = 3,'abril' = 4,'maio' = 5,
          'junho' = 6 , 'julho'= 7, 'agosto'=8,'setembro'=9,'outubro'=10,
          'novembro'=11,'dezembro'=12)

#Carregando dados
Delitos_SP<- readxl::read_xlsx("Datasets/TaxaDelito-EstadoSP_20251020_211705.xlsx")

#Tratamento de dados por ano
Delitos_SP<- Delitos_SP|>
  arrange(ano)
Delitos_SP <- Delitos_SP %>%
  mutate(
    across(everything(),
           ~parse_number(., locale = locale(decimal_mark = ",", grouping_mark = "."))
    ),
  )

TSs<-list()
names<- colnames(Delitos_SP)

#Criação de séries temporais
for (n in names){
  TSs[[n]]<-ts(Delitos_SP[[n]], start = (Delitos_SP$ano[1]), frequency = 1)
}

TSs[1]<-NULL

#Gráficos inciais
for (s in names(TSs)){
  plot_s<-ggplot(NULL, mapping = aes(x = Delitos_SP$ano, y = TSs[[s]]))+
    geom_line(size = 0.9, color = 'blue')+
    labs(x = "Tempo",
         y = "Número",
         title = str_glue("Número de ", s),
         subtitle = "Fonte: SSP-SP")+
    theme_minimal()
  show(plot_s)
}


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
  serie_treino <- head(serie_bruta ,-12)
  serie_teste <-tail(serie_bruta,12)
  
  mod_auto <- auto.arima(serie_treino, lambda = 0)
  
  
  mod_manual1 <- Arima(serie_treino, order = c(1, 1, 0), lambda = 0)
  
  
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
