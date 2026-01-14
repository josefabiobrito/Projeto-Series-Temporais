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
  serie_treino <- serie_limpa + 1
  
  mod_auto <- tryCatch({
    auto.arima(serie_treino, lambda = 0)
  }, error = function(e) return(NULL))
  
  # Modelo 2: Conservador (0,1,1)
  mod_manual1 <- tryCatch({
    Arima(serie_treino, order = c(0, 1, 1), include.drift = TRUE, lambda = 0)
  }, error = function(e) return(NULL))
  
  # Modelo 3: Flexível (0,1,2)
  mod_manual2 <- tryCatch({
    Arima(serie_treino, order = c(0, 1, 2), include.drift = TRUE, lambda = 0)
  }, error = function(e) return(NULL))
  
  # Modelo 4: (0,2,1)
  mod_manual3 <- tryCatch({
    Arima(serie_treino, order = c(0, 2, 1), include.drift = TRUE, lambda = 0)
  }, error = function(e) return(NULL))
  
  # Modelo 5: (2,1,0)
  mod_manual4 <- tryCatch({
    Arima(serie_treino, order = c(2, 1, 0), include.drift = TRUE, lambda = 0)
  }, error = function(e) return(NULL))
  
  get_metrics <- function(modelo, nome) {
    if (is.null(modelo)) return(NULL) 
    
    if (grepl("Auto", nome)) {
      desc <- forecast:::arima.string(modelo, padding = FALSE)
      nome <- paste("Auto:", desc)
    }
    
    acc <- accuracy(modelo)
    data.frame(
      Modelo = nome,
      AIC = round(modelo$aic, 2),
      RMSE = round(acc[1, "RMSE"], 2),
      MASE = round(acc[1, "MASE"], 3)
    )
  }
  
  lista_resultados <- list(
    get_metrics(mod_auto, "Auto"),
    get_metrics(mod_manual1, "Manual: ARIMA(0,1,1) c/ drift"),
    get_metrics(mod_manual2, "Manual: ARIMA(0,1,2) c/ drift"),
    get_metrics(mod_manual3, "Manual: ARIMA(0,2,1) c/ drift"),
    get_metrics(mod_manual4, "Manual: ARIMA(2,1,0) c/ drift")
    
  )
  
  tabela_resultados <- bind_rows(lista_resultados[!sapply(lista_resultados, is.null)]) %>% 
    arrange(AIC)
  
  cat("\n========================================\n")
  cat(str_glue(" ESTADO: {nome_uf} "))
  cat("\n========================================\n")
  if (nrow(tabela_resultados) > 0) {
    print(tabela_resultados)
  } else {
    cat("Não foi possível ajustar modelos (possivelmente dados insuficientes).\n")
  }
  cat("\n")
}

melhores_modelos_df <- tibble(
  UF = c("Ignorado/Em Branco", "Rondônia", "Acre", "Amazonas", "Roraima", 
         "Pará", "Amapá", "Tocantins", "Maranhão", "Piauí", 
         "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", 
         "Sergipe", "Bahia", "Minas Gerais", "Espírito Santo", "Rio de Janeiro", 
         "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul", 
         "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal", "Brasil"),
  
  Modelo = c("Auto: ARIMA(0,1,0)", "Manual: ARIMA(0,1,2) c/ drift", "Auto: ARIMA(0,1,1) with drift", 
             "Manual: ARIMA(0,1,2) c/ drift", "Auto: ARIMA(1,1,0) with drift", "Auto: ARIMA(0,2,1)", 
             "Manual: ARIMA(2,1,0) c/ drift", "Auto: ARIMA(3,1,0)", "Auto: ARIMA(1,2,1)", 
             "Manual: ARIMA(0,1,1) c/ drift", "Auto: ARIMA(3,2,0)", "Auto: ARIMA(0,2,2)", 
             "Auto: ARIMA(0,2,2)", "Auto: ARIMA(1,2,0)", "Auto: ARIMA(0,1,0) with drift", 
             "Auto: ARIMA(0,1,0) with drift", "Manual: ARIMA(2,1,0) c/ drift", "Auto: ARIMA(0,2,1)", 
             "Auto: ARIMA(0,2,1)", "Auto: ARIMA(1,2,0)", "Auto: ARIMA(3,2,0)", 
             "Auto: ARIMA(2,2,2)", "Auto: ARIMA(1,2,0)", "Auto: ARIMA(0,2,1)", 
             "Auto: ARIMA(0,2,2)", "Auto: ARIMA(0,2,1)", "Auto: ARIMA(0,2,1)", 
             "Auto: ARIMA(1,2,2)", "Auto: ARIMA(3,2,1)"),
  
  AIC = c(149.49, 41.28, 66.87, 16.08, 42.13, 16.39, 50.75, 30.69, 42.28, 31.03, 
          38.00, 47.89, 41.61, 4.97, 71.05, 41.14, 13.26, 13.84, 15.48, 5.37, 
          -44.37, 1.28, 18.23, -0.37, 23.31, 13.13, 26.18, 35.17, -11.24),
  
  RMSE = c(1661.06, 39.04, 14.88, 170.34, 25.17, 128.36, 22.63, 21.85, 84.29, 54.58, 
           99.17, 43.76, 54.37, 146.78, 47.13, 36.73, 108.46, 170.03, 58.39, 286.33, 
           648.15, 157.63, 153.04, 269.30, 68.46, 56.01, 75.28, 86.68, 3345.81),
  
  MASE = c(0.977, 1.228, 0.929, 1.084, 0.946, 1.112, 1.136, 0.972, 0.999, 1.189, 
           1.376, 1.019, 1.203, 1.254, 1.297, 1.172, 1.058, 1.159, 1.134, 1.064, 
           0.838, 1.157, 0.986, 0.866, 1.328, 1.144, 1.199, 1.288, 1.217)
)

print(melhores_modelos_df)
