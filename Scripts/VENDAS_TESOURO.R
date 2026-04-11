# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${VENDAS_TESOURO.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 26/09/2025
# DESCRIÇÃO: ANÁLISE DO VOLUME E RECEITA DE VENDAS DE TÍTULOS DO TESOURO DIRETO
#            POR CATEGORIA DE TÍTULO

#Bibliotecas
library(forecast)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(patchwork)

#Carregando dados
VENDAS<-read.csv2("Datasets/vendastesourodireto.csv")

#Agrupamento por tipo de título
tipos<- unique(VENDAS$Tipo.Titulo)
df_tipos<-list()
for (t in tipos){  
  VENDAS_tipo<-filter(VENDAS,Tipo.Titulo == t)
  VENDAS_tipo$Vencimento.do.Titulo<-NULL
  VENDAS_tipo$Data.Venda<-dmy(VENDAS_tipo$Data.Venda)
  VENDAS_tipo$Valor <- as.numeric(gsub("\\.", "", gsub(",", ".", VENDAS_tipo$Valor)))
  VENDAS_tipo$Valor<- VENDAS_tipo$Valor/1e+08
  VENDAS_tipo <- VENDAS_tipo %>%
    mutate(AnoMes = floor_date(Data.Venda, "month"))
  VENDAS_tipo_mensal <- VENDAS_tipo %>%
    group_by(AnoMes) %>%
    summarise(Total = sum(Valor, na.rm = TRUE))
  df_tipos[[t]]<-VENDAS_tipo_mensal
}

VENDAS$Data.Venda <- dmy(VENDAS$Data.Venda)
VENDAS$Valor <- as.numeric(gsub("\\.", "", gsub(",", ".", VENDAS$Valor)))
VENDAS$Valor <- VENDAS$Valor / 1e+08  

VENDAS_total_mensal <- VENDAS %>%
  mutate(AnoMes = floor_date(Data.Venda, "month")) %>%
  group_by(AnoMes) %>%
  summarise(Total = sum(Valor, na.rm = TRUE))

#Séries temporais e gráficos iniciais

TSs<-list()
for (n in names(df_tipos)){
  t<-df_tipos[[n]]
  ts_tipo<- ts(t$Total, start = c(year(t$AnoMes[1]),month(t$AnoMes[1])), frequency = 12)
  TSs[[n]]<-ts_tipo
}
ts_total <- ts(
  VENDAS_total_mensal$Total,
  start = c(year(VENDAS_total_mensal$AnoMes[1]),
            month(VENDAS_total_mensal$AnoMes[1])),
  frequency = 12
)
TSs[['Vendas Acumuladas']]<-ts_total

for (nome in names(TSs)){
  plot<-autoplot(TSs[[nome]], ylab = "Valor (x 100 milhões R$)")+
      labs(title = str_glue("Vendas de títulos: {nome} "),
           subtitle = "Ministerio da Fazenda")+
      geom_line(size = 0.9, colour = "blue")+
      theme_minimal()
  show(plot)
    #ggsave(filename = str_glue("Vendas_{n}.png"),
    #       plot = plot,
    #       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
    #       width = 8,
    #       height = 6,
    #       units = "in",
    #       dpi = 300)
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


#Ajuste de modelos
safe_Arima <- function(y, order, seasonal = c(0,0,0), ...) {
  tryCatch({
    Arima(y, order = order, seasonal = seasonal, method="ML", ...)
  }, error = function(e) return(NULL))
}

melhores_modelos<-list()
for (nome in names(TSs)) {
  
  dados_brutos <- TSs[[nome]] + 1
  
  if(!is.ts(dados_brutos)) {
    dados_brutos <- ts(as.numeric(dados_brutos), frequency = 1)
  }
  
  n <- length(dados_brutos)
  serie_treino <- subset(dados_brutos, end = n - 12)
  serie_teste  <- subset(dados_brutos, start = n - 11)
  
  mod_auto    <- tryCatch(auto.arima(serie_treino), error=function(e) NULL)
  mod_manual1 <- safe_Arima(serie_treino, order = c(3, 1, 2))
  mod_manual2 <- safe_Arima(serie_treino, order = c(2, 1, 1))
  mod_manual3 <- safe_Arima(serie_treino, order = c(1, 1, 2))
  mod_manual4 <- safe_Arima(serie_treino, order = c(1, 1, 1))
  
  modelos <- list(mod_auto, mod_manual1, mod_manual2, mod_manual3, mod_manual4)
  
  extrair_metricas <- function(modelo, dados_teste) {
    if (is.null(modelo)) return(NULL)
    
    prev <- forecast(modelo, h = length(dados_teste))
    
    tryCatch({
      acc  <- accuracy(prev, dados_teste)
      return(data.frame(Modelo = forecast:::arima.string(modelo, padding = FALSE), 
                        AIC = round(modelo$aic, 2), 
                        RMSE_Teste = round(acc[2, "RMSE"], 2), 
                        MASE_Teste = round(acc[2, "MASE"], 3)))
    }, error = function(e) return(NULL))
  }
  
  lista_resultados <- lapply(modelos, extrair_metricas, dados_teste = serie_teste)
  lista_limpa <- lista_resultados[!sapply(lista_resultados, is.null)]
  tabela_resultados <- bind_rows(lista_limpa)
  
  cat("\n========================================\n")
  cat(str_glue(" {nome} "))
  cat("\n========================================\n")
  
  if (nrow(tabela_resultados) > 0) {
    print(tabela_resultados)
    
    indices_validos <- which(!sapply(modelos, is.null))
    modelos_validos <- modelos[indices_validos]
    aics <- sapply(modelos_validos, function(x) x$aic)
    melhor_modelo <- modelos_validos[[which.min(aics)]]
    melhores_modelos[[nome]]<-melhor_modelo
    
    tryCatch({
      plot <- autoplot(forecast(melhor_modelo, h=length(serie_teste))) +
        autolayer(serie_teste, series="Dados Reais") +
        labs(title = str_glue("Previsão-{nome} vs Realidade"),
             subtitle = str_glue("Modelo: {forecast:::arima.string(melhor_modelo)}")) +
        theme_bw()
      print(plot)
    }, error = function(e) {
      plot(forecast(melhor_modelo, h=length(serie_teste)), main=nome)
      lines(serie_teste, col="red")
    })
    
  } else {
    cat("Não foi possível ajustar modelos (dados insuficientes ou erro de convergência).\n")
  }
  cat("\n")
}

for (nome in names(melhores_modelos)){
  modelo <- melhores_modelos[[nome]]
  
  if (is.null(modelo)) {
    next
  }
  
  cat("\n============================================================\n")
  cat(str_glue("{nome} | MODELO: {forecast:::arima.string(modelo)} "))
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