# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${Consumo_Energia.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 14/09/2025
# DESCRIÇÃO: ANÁLISE DO CONSUMO RESIDÊNCIAL DE ENERGIA NO BRASIL E ESTADOS 


library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(forecast)
library(ggplot2)
library(openxlsx)

df_raw <- read_excel("C:\\Users\\josef\\OneDrive\\Documentos\\PUB\\Datasets\\CONSUMO RESIDENCIAL POR UF.xlsx")


dfs_ufs<-list()
for (uf in unique(df_raw$UF)) {
  df_uf<-filter(df_raw, UF == uf)
  df_uf<-pivot_longer(df_uf,
                      cols = -UF,
                      names_to = 'Date',
                      values_to = 'Consumo')
  dfs_ufs[[uf]]<-df_uf
}

for (uf in dfs_ufs){
  ts_uf<- ts(uf$Consumo, start = 2004,frequency = 12)
  plot<-autoplot(ts_uf, ylab = 'Consumo(MWh')+
    labs(title = str_glue("Consumo residêncial de Energia em {uf$UF[1]}"),
         subtitle ="Fonte: EPE " )+
    geom_line(size = 0.9, colour = "blue")+
    theme_minimal()
  ggsave(filename = str_glue("Energia_{uf$UF[1]}.png"),
         plot = plot,
         path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
         width = 8,
         height = 6,
         units = "in",
         dpi = 300)
}
