# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${AIDS_GRL}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 19/09/2025
# DESCRIÇÃO: ANÁLISE DOS CASOS DE AIDS NA POPULAÇÃO GERAL BRASILEIRA


library(openxlsx)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

AIDS<- read.xlsx("Datasets/AIDS_GRL.xlsx")
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
  ggsave(filename = str_glue("AIDS_GRL_{n$UF[1]}.png"),
         plot = plot,
         path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
         width = 8,
         height = 6,
         units = "in",
         dpi = 300)
}
