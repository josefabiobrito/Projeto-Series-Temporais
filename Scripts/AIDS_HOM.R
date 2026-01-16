# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${AIDS_HTR}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 20/10/2025
# DESCRIÇÃO: ANÁLISE DOS CASOS DE AIDS NA POPULAÇÃO HOMOSSEXUAL BRASILEIRA


#Bibliotecas
library(openxlsx)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

#Carregar dados
AIDS<- read.xlsx("Datasets/AIDS_HOM.xlsx")

#Tratamento dos dados
AIDS<- rename(AIDS, UF = UF.Notificação)
AIDS$Total<- NULL
ufs<- AIDS$UF
ufs
dfs_uf<-list()
for (uf in ufs){
  df_uf<-filter(AIDS,UF == uf)
  df_uf <- df_uf %>%
    mutate(across(-UF, as.integer))
  df_uf<-pivot_longer(df_uf,
                      cols = -UF,
                      names_to = "Data",
                      values_to = 'Notificação')
  dfs_uf[[uf]]<-df_uf
}

#Criação de Séries temporais e graficos iniciais
TSs_ufs<-list()

for (n in dfs_uf){
  ts_uf<- ts(n$Notificação, start = c(1980,01),frequency = 1)
  TSs_ufs[[n$UF[1]]]<-ts_uf
  plot<-autoplot(ts_uf, ylab = 'Notificação')+
    labs(title = str_glue("Notificação de AIDS em Homossexuais- {n$UF[1]}"),
         subtitle ="Fonte: DataSUS" )+
    geom_line(size = 0.9, colour = "red")+
    theme_minimal()
  show(plot)
  #Salvar os gráficos
  #ggsave(filename = str_glue("AIDS_HOM_{n$UF[1]}.png"),
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
  
  Modelo = c("Auto: ARIMA(0,1,1) with drift", "Auto: ARIMA(3,1,0) with drift", 
             "Auto: ARIMA(0,1,1) with drift", "Auto: ARIMA(0,1,1) with drift", 
             "Auto: ARIMA(0,1,0) with drift", "Auto: ARIMA(0,1,0)", 
             "Auto: ARIMA(2,1,0) with drift", "Auto: ARIMA(1,1,0) with drift", 
             "Auto: ARIMA(1,1,0) with drift", "Auto: ARIMA(1,2,1)", 
             "Auto: ARIMA(0,1,1) with drift", "Auto: ARIMA(2,1,0) with drift", 
             "Auto: ARIMA(0,2,3)", "Auto: ARIMA(0,1,0)", 
             "Auto: ARIMA(0,1,1) with drift", "Manual: ARIMA(0,1,1) c/ drift", 
             "Auto: ARIMA(2,2,0)", "Auto: ARIMA(0,1,1) with drift", 
             "Manual: ARIMA(0,1,1) c/ drift", "Auto: ARIMA(1,2,2)", 
             "Auto: ARIMA(0,2,1)", "Auto: ARIMA(1,2,1)", 
             "Auto: ARIMA(0,2,2)", "Auto: ARIMA(0,1,0) with drift", 
             "Auto: ARIMA(0,1,0)", "Auto: ARIMA(0,1,1) with drift", 
             "Manual: ARIMA(2,1,0) c/ drift", "Auto: ARIMA(2,2,1)"),
  
  AIC = c(43.03, 71.07, 77.73, 46.75, 27.87, 55.44, 65.09, 48.72, 60.50, 
          47.63, 61.24, 61.47, 23.47, 56.17, 66.76, 32.65, 41.16, 54.61, 
          61.59, 9.24, 20.04, 38.83, 42.71, 46.28, 63.28, 58.97, 27.46, -7.72),
  
  RMSE = c(12.87, 3.60, 48.80, 6.90, 37.47, 7.07, 6.53, 21.46, 13.96, 
           43.05, 17.97, 18.08, 36.84, 13.70, 12.21, 27.98, 75.05, 27.73, 
           164.02, 283.92, 34.98, 54.79, 57.09, 15.86, 11.82, 28.54, 27.31, 710.93),
  
  MASE = c(1.165, 0.673, 1.124, 0.797, 1.110, 0.976, 0.939, 1.119, 1.006, 
           0.993, 0.838, 0.775, 1.039, 0.976, 0.823, 0.994, 1.336, 1.063, 
           1.080, 1.183, 1.089, 1.074, 1.227, 1.065, 0.976, 0.967, 1.064, 1.194)
)
