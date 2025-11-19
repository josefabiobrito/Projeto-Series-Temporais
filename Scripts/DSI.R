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
DSI_ts<-ts(as.numeric(DSI$Quantidade), start = 2000, frequency = 1)
plot<-autoplot(DSI_ts, ylab = "Quantidade (1.000)", size = 0.9, color = 'blue')+
  labs(title = "Declarações Simplificadas de Importação por ano",
       subtitle = "Fonte: Ministério da Fazenda")+
  theme_minimal()
show(plot)
#ggsave(filename = "DSI.png",
#       plot = plot,
#       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
#       width = 8,
#       height = 6,
#       units = "in",
#       dpi = 300
#       )

