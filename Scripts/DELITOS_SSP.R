# PROJETO: PUB- SÉRIES TEMPORAIS
# ARQUIVO: SSP.R
# AUTOR: José Fábio Viana de Brito
# DATA: 07/10/2025
# DESCRIÇÃO: ANÁLISE DE DADOS CRIMINAIS DO ESTADO DE SÃO PAULO
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

Delitos_SP<- readxl::read_xlsx("Datasets/TaxaDelito-EstadoSP_20251020_211705.xlsx")
Delitos_SP<- Delitos_SP|>
  arrange(ano)
Delitos_SP <- Delitos_SP %>%
  mutate(
    across(everything(),
           ~parse_number(., locale = locale(decimal_mark = ",", grouping_mark = "."))
    ),
  )

series<-list()
names<- colnames(Delitos_SP)

for (n in names){
  series[[n]]<-ts(Delitos_SP[[n]], start = (Delitos_SP$ano[1]), frequency = 1)
}

series[1]<-NULL

for (s in names(series)){
  plot_s<-ggplot(NULL, mapping = aes(x = Delitos_SP$ano, y = series[[s]]))+
    geom_line(size = 0.9, color = 'blue')+
    labs(x = "Tempo",
         y = "Número",
         title = str_glue("Número de ", s),
         subtitle = "Fonte: SSP-SP")+
    theme_minimal()
  show(plot_s)
}


CRIMES<-readxl::read_xlsx("Datasets/indicadoressegurancapublicauf.xlsx")
CRIMES<-CRIMES|>
  rename(Mes = 'Mês')

ufs<-unique(CRIMES$UF)
dfs_uf<-list()
for(nome in ufs){
  dfs_uf[[nome]]<-CRIMES[CRIMES$UF == nome,]
}


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
