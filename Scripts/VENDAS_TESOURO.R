# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${VENDAS_TESOURO.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 26/09/2025
# DESCRIÇÃO: ANÁLISE DO VOLUME E RECEITA DE VENDAS DE TÍTULOS DO TESOURO DIRETO
#            POR CATEGORIA DE TÍTULO

library(forecast)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(dplyr)

VENDAS<-read.csv2("Datasets/vendastesourodireto.csv")
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



for (n in names(df_tipos)){
  t<-df_tipos[[n]]
  ts_tipo<- ts(t$Total, start = c(year(t$AnoMes[1]),month(t$AnoMes[1])), frequency = 12)
  plot<-autoplot(ts_tipo, ylab = "Valor (x 100 milhões R$)")+
    labs(title = str_glue("Vendas de títulos: {n} "),
         subtitle = "Ministerio da Fazenda")+
    geom_line(size = 0.9, colour = "blue")+
    theme_minimal()
  show(plot)
  ggsave(filename = str_glue("Vendas_{n}.png"),
         plot = plot,
         path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
         width = 8,
         height = 6,
         units = "in",
         dpi = 300)
}

VENDAS$Data.Venda <- dmy(VENDAS$Data.Venda)
VENDAS$Valor <- as.numeric(gsub("\\.", "", gsub(",", ".", VENDAS$Valor)))
VENDAS$Valor <- VENDAS$Valor / 1e+08  

VENDAS_total_mensal <- VENDAS %>%
  mutate(AnoMes = floor_date(Data.Venda, "month")) %>%
  group_by(AnoMes) %>%
  summarise(Total = sum(Valor, na.rm = TRUE))

ts_total <- ts(
  VENDAS_total_mensal$Total,
  start = c(year(VENDAS_total_mensal$AnoMes[1]),
            month(VENDAS_total_mensal$AnoMes[1])),
  frequency = 12
)

p2<-autoplot(ts_total, ylab = "Valor agregado (x 100 milhões R$)") +
  labs(title = "Vendas agregadas do Tesouro Direto",
       subtitle = "Fonte: Ministério da Fazenda") +
  geom_line(size = 0.9, colour = "darkred") +
  theme_minimal()
ggsave(filename = str_glue("Vendas_Total_TD.png"),
       plot = p2,
       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)