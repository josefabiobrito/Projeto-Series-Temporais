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
    mutate(Mes_Limpo = str_trim(Mes),
           Mes_num = meses[Mes_Limpo],
           Data = yearmonth(make_date(year = Ano, month = Mes_num)))|>
    select(-Ano, -Mes, -Mes_Limpo,-Mes_num) |>
    as_tsibble(index = Data,
               key = c('Tipo Crime'))
}

dados_completos <- bind_rows(dfs_uf, .id = "UF")