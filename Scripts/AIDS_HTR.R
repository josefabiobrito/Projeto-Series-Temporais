# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${AIDS_HTR}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 19/09/2025
# DESCRIÇÃO: ANÁLISE DOS CASOS DE AIDS NA POPULAÇÃO HÉTEROSEXUAL BRASILEIRA


#Bibliotecas
library(openxlsx)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

#Carregando dados
AIDS<- read.xlsx("Datasets/AIDS_HTR.xlsx")

#Tratamento dos dados
AIDS<- rename(AIDS, UF = UF.Notificação)
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

#Transformação em Série temporal e gráficos inciais
TSs_ufs<-list()

for (n in nomes){
  ts_uf<- ts(n$Notificação, start = 1980,frequency = 1)
  TSs_ufs[[n$UF[1]]]<-ts_uf
  plot<-autoplot(ts_uf, ylab = 'Notificação')+
    labs(title = str_glue("Notificação de AIDS em Heterossexuais {n$UF[1]}"),
         subtitle ="Fonte: DataSUS" )+
    geom_line(size = 0.9, colour = "red")+
    theme_minimal()
  show(plot)
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
    Arima(serie_treino, order = c(0, 1, 1), lambda = 0)
  }, error = function(e) return(NULL))
  
  # Modelo 3: Flexível (0,1,2)
  mod_manual2 <- tryCatch({
    Arima(serie_treino, order = c(0, 1, 2),  lambda = 0)
  }, error = function(e) return(NULL))
  
  # Modelo 4: (0,2,1)
  mod_manual3 <- tryCatch({
    Arima(serie_treino, order = c(0, 2, 1),  lambda = 0)
  }, error = function(e) return(NULL))
  
  # Modelo 5: (2,1,0)
  mod_manual4 <- tryCatch({
    Arima(serie_treino, order = c(2, 1, 0),  lambda = 0)
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
    get_metrics(mod_manual1, "Manual: ARIMA(0,1,1) "),
    get_metrics(mod_manual2, "Manual: ARIMA(0,1,2) "),
    get_metrics(mod_manual3, "Manual: ARIMA(0,2,1) "),
    get_metrics(mod_manual4, "Manual: ARIMA(2,1,0) ")
    
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
  UF = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", 
         "Tocantins", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", 
         "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", 
         "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo", 
         "Paraná", "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", 
         "Mato Grosso", "Goiás", "Distrito Federal", "Brasil"),
  
  Modelo = c("Auto: ARIMA(1,2,1)", "Auto: ARIMA(0,1,1) with drift", "Auto: ARIMA(1,2,1)", 
             "Auto: ARIMA(0,1,1) with drift", "Auto: ARIMA(0,2,1)", "Auto: ARIMA(0,1,1) with drift", 
             "Auto: ARIMA(0,2,1)", "Auto: ARIMA(0,2,1)", "Auto: ARIMA(0,2,2)", 
             "Auto: ARIMA(2,2,0)", "Auto: ARIMA(1,2,1)", "Auto: ARIMA(0,2,1)", 
             "Auto: ARIMA(0,2,1)", "Auto: ARIMA(0,2,1)", "Manual: ARIMA(2,1,0)", 
             "Manual: ARIMA(2,1,0)", "Auto: ARIMA(1,2,1)", "Auto: ARIMA(0,2,1)", 
             "Manual: ARIMA(2,1,0)", "Auto: ARIMA(1,2,1)", "Manual: ARIMA(2,1,0)", 
             "Auto: ARIMA(0,2,1)", "Auto: ARIMA(0,2,1)", "Auto: ARIMA(0,2,1)", 
             "Auto: ARIMA(1,2,1)", "Auto: ARIMA(1,2,1)", "Manual: ARIMA(2,1,0)", 
             "Auto: ARIMA(0,2,0)"),
  
  AIC = c(17.57, 65.74, 46.40, 50.42, 40.43, 39.37, 13.74, 19.99, 30.63, 
          31.57, 40.30, 44.85, 13.91, 21.04, 31.99, 15.23, -10.34, 40.10, 
          3.03, -16.56, 27.71, 6.29, 18.83, 9.20, 25.28, 43.29, 38.11, -39.60),
  
  RMSE = c(24.22, 11.03, 106.48, 17.70, 89.18, 16.25, 15.66, 61.69, 35.82, 
           63.49, 27.14, 41.12, 78.96, 34.75, 20.20, 52.80, 83.86, 49.57, 
           112.29, 367.78, 90.24, 114.00, 174.48, 40.78, 47.94, 47.83, 35.94, 978.45),
  
  MASE = c(1.067, 0.959, 1.148, 0.976, 1.180, 1.064, 1.008, 1.051, 1.276, 
           1.331, 1.202, 1.118, 1.183, 1.115, 1.013, 0.932, 0.924, 1.246, 
           0.817, 0.806, 0.922, 1.015, 0.974, 1.233, 1.120, 1.195, 1.019, 0.794)
)
