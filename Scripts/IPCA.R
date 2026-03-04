# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${IPCA.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 22/09/2025
# DESCRIÇÃO: ANÁLISE DO IPCA NOS MAIORES GRUPOS CATEGÓRICOS

#Bibliotecas
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(patchwork)

#Carregando dados
IPCA<- read_excel('Datasets/IPCA.xlsx')

#Tratamento de dados
IPCA$Territorio<-NULL
col_ig<-c("Grupo")
datas<- setdiff(colnames(IPCA),col_ig)
novas_datas<-as.Date(as.numeric(datas), origin = "1899-12-30")
nomes<-c(col_ig,as.character(novas_datas))
colnames(IPCA)<-nomes

#Separação por tipo de IPCA
grupos<-unique(IPCA$Grupo)
grupos
objetos<-list()
for(grupo in grupos){
  df_grp<-filter(IPCA,Grupo == grupo)
  df_grp<-pivot_longer(df_grp,
                          cols = -Grupo,
                           names_to = "Data",
                           values_to = "indice"
       )
  objetos[[grupo]]<-df_grp
}

#Criação de séries temporais e gráficos iniciais
TSs<-list()
for (nome in names(objetos)){
  ts_grp<-ts(objetos[[nome]]$indice, start = c(2020,1), frequency = 12)
  TSs[[nome]]<-ts_grp
}
for(nome in names(TSs)){
  ts_grp<-TSs[[nome]]
  plot<-autoplot(ts_grp, ylab = 'Valor índice')+
    labs(title = str_glue("IPCA referente a categoria {obj$Grupo[1]}"),
         subtitle ="Fonte: IBGE" )+
    geom_line(size = 0.9, colour = "blue")+
    theme_minimal()
  show(plot)
  #ggsave(filename = str_glue("IPCA_{obj$Grupo[1]}.png"),
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
for (nome in names(TSs)) {
  serie_treino <- head(TSs[[nome]] ,-6)
  serie_teste <-tail(TSs[[nome]],6)
  
  mod_auto <- auto.arima(serie_treino)
  
  mod_manual1 <- Arima(serie_treino, order = c(1, 0, 1))
  
  mod_manual2 <- Arima(serie_treino, order = c(1, 0, 1),seasonal = c(0,1,0))
  
  mod_manual3 <- Arima(serie_treino, order = c(0, 1, 1))
  
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

