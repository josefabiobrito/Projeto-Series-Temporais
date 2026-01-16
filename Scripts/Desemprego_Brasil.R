# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${Desemprego_Brasil.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 12/09/2025
# DESCRIÇÃO: ANÁLISE DA TAXA DE DESOCUPAÇÃO E RENDIMENTO MÉDIO NO BRASIL

#Bibliotecas
library('tsibble')
library('dplyr')
library('forecast')
library('ggplot2')
library('stringr')
library(openxlsx)

#Carregando dados de desocupação
df<-read.csv("Datasets/20250912071448.csv.csv")|>
  rename(Taxa = Taxa.de.desocupação)

#Tratamento de dados
meses<- c('jan' = 1,'fev' = 2,'mar' = 3,'abr' = 4,'mai' = 5,'jun' = 6 , 'jul'= 7,
          'ago'=8,'set'=9,'out'=10,'nov'=11,'dez'=12)
df_final <- df |>
  mutate(
    Mês = recode(
      str_extract(Tempo, "\\w{3}(?=\\s\\d{4}$)"), 
      !!!meses
    ),
    Ano = as.integer(str_extract(Tempo, "\\d{4}"))
    )|>
  select(Mês, Ano, Taxa)
#Salvar dados limpos
write.xlsx(df_final,'C:/Users/josef/OneDrive/Documentos/PUB/df_desemprego_BR.xlsx')

TSs<-list()
#Criação série temporal
ts_desemprego<-ts(df_final$Taxa, start = c(df_final$Ano[1],df_final$Mês[1]), frequency = 12)
TSs[['Desocupação']]<-ts_desemprego

#Gráficos iniciais
autoplot(ts_desemprego, 
         xlab = "Tempo",
         ylab = "Taxa de Desocupação",
         colour = "blue") +
  labs(title = "Série Histórica do Desemprego no Brasil",
       subtitle = "Fonte: IBGE",
       colour = "Legenda da Série") +
  scale_x_continuous(breaks = seq(2012, 2025, by = 2)) +
  theme_minimal() +                      
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"          
  )


#Carregando dados de ocupação
df2<- read.csv("C:/Users/josef/OneDrive/Documentos/PUB/Projeto-Series-Temporais/Datasets/Tabela_Ocupacao.csv")
df2<-pivot_longer(df2, cols = -'Região')

#Criação série temporal
ts_Rendimento<- ts(df2$value,start =c(2012,03),frequency = 12)
TSs[['Rendimento']]<-ts_Rendimento

#Gráficos iniciais
autoplot(ts_Rendimento, 
         xlab = "Tempo",
         ylab = "Rendimento Médio Mensal",
         colour = "blue") +
  geom_line(size = 1, colour = "blue")+
  labs(title = "Série Histórica do Rendimento Médio no Brasil",
       subtitle = "Fonte: IBGE",
       colour = "Legenda da Série") +
  scale_x_continuous(breaks = seq(2012, 2025, by = 2)) +
  theme_minimal() +                      
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"          
  )




#Correlogramas
for(nome in names(TSs)){
  plotAcf<-ggAcf(TSs[[nome]],lag.max = 40, type = 'correlation')+
    labs(title = str_glue("Autocorrelação para série de {nome}"))
  show(plotAcf)
}

for(nome in names(TSs)){
  plotPacf<-ggAcf(TSs[[nome]],lag.max = 40, type = 'partial')+
    labs(title = str_glue("Autocorrelação parcial para série de {nome}"))
  show(plotPacf)
}

#Ajuste de modelos
for (nome in names(TSs)) {
  
  serie_bruta <- TSs[[nome]]+1
  serie_treino <- head(serie_bruta ,-12)
  serie_teste <-tail(serie_bruta,12)
  
  mod_auto <- auto.arima(serie_treino, lambda = 0)
  
  
  mod_manual1 <- Arima(serie_treino, order = c(2, 1, 1),seasonal = c(1,1,1), lambda = 0)
  
  
  mod_manual2 <- Arima(serie_treino, order = c(1, 1, 2),seasonal = c(1,1,0), lambda = 0)
  
  
  mod_manual3 <- Arima(serie_treino, order = c(1, 1, 3),seasonal = c(1,1,1), lambda = 0)
  
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
  plot<-autoplot(forecast(melhor_modelo, h=length(serie_teste))) +
    autolayer(serie_teste, series="Dados Reais") +
    labs(title = str_glue("Previsão-{nome} vs Realidade"),
         subtitle = str_glue("Modelo:{forecast:::arima.string(melhor_modelo)}"))
  show(plot)
}






