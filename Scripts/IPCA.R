# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${IPCA.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 22/09/2025
# DESCRIÇÃO: ANÁLISE DO IPCA NOS MAIORES GRUPOS CATEGÓRICOS

library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

IPCA<- read_excel('Datasets/IPCA.xlsx')
IPCA$Territorio<-NULL
col_ig<-c("Grupo")
datas<- setdiff(colnames(IPCA),col_ig)
novas_datas<-as.Date(as.numeric(datas), origin = "1899-12-30")
nomes<-c(col_ig,as.character(novas_datas))
colnames(IPCA)<-nomes

grupos<-unique(IPCA$Grupo)
grupos
objetos<-list()
for(grupo in grupos){
  df_grp<-filter(IPCA,Grupo == grupo)
  df_grp<-pivot_longer(df_grp,
                          cols = -Grupo,
                           names_to = "Data",
                           values_to = "indice"
       )
  objetos[[grupo]]<-df_grp
}

for (obj in objetos){
  ts_grp<-ts(obj$indice, start = c(2020,1), frequency = 12)
  plot<-autoplot(ts_grp, ylab = 'Valor índice')+
    labs(title = str_glue("IPCA referente a categoria {obj$Grupo[1]}"),
         subtitle ="Fonte: IBGE" )+
    geom_line(size = 0.9, colour = "blue")+
    theme_minimal()
  ggsave(filename = str_glue("IPCA_{obj$Grupo[1]}.png"),
         plot = plot,
         path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
         width = 8,
         height = 6,
         units = "in",
         dpi = 300)
}
