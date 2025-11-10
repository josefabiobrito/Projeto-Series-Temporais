# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${DSI.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 22/09/2025
# DESCRIÇÃO: ANÁLISE Do VOLUME DE DECLARAÇÕES SIMPLIFICADAS DE IMPORTAÇÃO

library(ggplot2)
library(forecast)
library(readxl)

DSI<-read.csv2("Datasets/balanco-aduaneiro-qtd-dsi.csv")
DSI<- DSI%>%
  rename(
    Ano = ANO.REGIS,
    Quantidade = QTD.DSI
  )
DSI_ts<-ts(DSI$Quantidade, start = c(2000,01,01), frequency = 1)
plot<-autoplot(DSI_ts, ylab = "Quantidade (1.000)")+
  labs(title = "Declarações Simplificadas de Importação por ano",
       subtitle = "Fonte: Ministério da Fazenda")+
  geom_line(size = 0.9, colour = "blue")+
  theme_minimal()
ggsave(filename = "DSI.png",
       plot = plot,
       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
       width = 8,
       height = 6,
       units = "in",
       dpi = 300
       )
