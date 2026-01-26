# PROJETO: PUB- SÉRIES TEMPORAIS
# ARQUIVO: MJSP.R
# AUTOR: José Fábio Viana de Brito
# DATA: 07/10/2025
# DESCRIÇÃO: ANÁLISE DE DADOS CRIMINAIS DO BRASIL

#Bibliotecas
library(ggplot2)
library(forecast)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)
library(tsibble)
library(readr)

meses<- c('janeiro' = 1,'fevereiro' = 2,'março' = 3,'abril' = 4,'maio' = 5,
          'junho' = 6 , 'julho'= 7, 'agosto'=8,'setembro'=9,'outubro'=10,
          'novembro'=11,'dezembro'=12)

#Carregando dados de Segurança pública nacional
CRIMES<-readxl::read_xlsx("Datasets/indicadoressegurancapublicauf.xlsx")
#Tratamento de dados
CRIMES<-CRIMES|>
  rename(Mes = 'Mês')

#Separação por estado
ufs<-unique(CRIMES$UF)
dfs_uf<-list()
for(nome in ufs){
  dfs_uf[[nome]]<-CRIMES[CRIMES$UF == nome,]
}

#Organização por tipo de crime
for (i in seq_along(dfs_uf)) {
  dfs_uf[[i]] <- dfs_uf[[i]] |>
    mutate(
      Mes_Limpo = str_trim(Mes),
      Mes_num = meses[Mes_Limpo],
      Data = yearmonth(make_date(year = Ano, month = Mes_num)),
      Categoria = case_when(
        `Tipo Crime` == "Estupro" ~ "Estupro",
        `Tipo Crime` %in% c("Homicídio doloso", "Roubo seguido de morte (latrocínio)") ~ "Mortes",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(Categoria)) |>
    group_by(UF, Data, Categoria) |>
    summarise(Ocorrências = sum(Ocorrências, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(
      names_from = Categoria,
      values_from = Ocorrências,
      values_fill = 0
    ) |>
    as_tsibble(
      index = Data,
      key = UF
    )
}

TSs_ufs<-list()

for(nome in names(dfs_uf)){
  TSs_ufs[[nome]]<-list()
  for(i in 3:4){
    ts_cat<-ts(dfs_uf[[nome]][,i],start = c(2015,1), frequency = 12)
    tipo<-names(dfs_uf[[nome]])[i]
    TSs_ufs[[nome]][[tipo]]<-ts_cat
  }
}

#Gráficos Iniciais
for(nome in names(TSs_ufs)){
  for(cat in names(TSs_ufs[[nome]])){
    plot<-autoplot(TSs_ufs[[nome]][[cat]], ylab = 'Número de ocorrências')+
      labs(title = str_glue("Série temporal {cat}-{nome}"),
           subtitle ="Fonte: MSP-BR" )+
      geom_line(size = 0.9, colour = "blue")+
      theme_minimal()
    show(plot)
  }
}

#Correlogramas

for(nome in names(TSs_ufs)){
  for (cat in names(TSs_ufs[[nome]])){
    plotAcf<-ggAcf(TSs_ufs[[nome]][[cat]],lag.max = 36, type = 'correlation')+
      labs(title = str_glue("Autocorrelação para série de {cat}-{nome}"))
    show(plotAcf)
  }
}

for(nome in names(TSs_ufs)){
  for (cat in names(TSs_ufs[[nome]])){
    plotPacf<-ggAcf(TSs_ufs[[nome]][[cat]],lag.max = 36, type = 'partial')+
      labs(title = str_glue("Autocorrelação parcial para série de {cat}-{nome}"))
    show(plotPacf)
  }
}

#Ajuste de modelos
for (nome in names(TSs)) {
  for (cat in names(TSs_ufs[[nome]])){
    serie_treino <- head(TSs[[nome]][[cat]] ,-12)
    serie_teste <-tail(TSs[[nome]][[cat]],12)
    
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
    cat(str_glue(" {cat}-{nome} "))
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
      labs(title = str_glue("Previsão {cat}-{nome} vs Realidade"),
           subtitle = str_glue("Modelo:{forecast:::arima.string(melhor_modelo)}"))
    show(plot)
  }
}

