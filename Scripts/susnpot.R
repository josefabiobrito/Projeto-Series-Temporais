# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${sunspot.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 14/09/2025
# DESCRIÇÃO: ANÁLISE DO NÚMERO MÉDIO DE MANCHAS SOLARES NO MUNDO

library('forecast')
library('ggplot2')
library(openxlsx)

SPY<-sunspot.year

class(SPY)
start(SPY);end(SPY);frequency(SPY)
plot<-autoplot(sunspot.year,
         xlab = 'Tempo',
         ylab = 'Número médio',
         size =0.9,
         color = 'blue')+
  labs(title = 'Número médio de manchas solares por ano')+
  theme_minimal()+
show(plot)
#ggsave(filename = "Sunspot.jpg",
#       plot = plot,
#       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
#       width = 8,
#       height = 6,
#       units = "in")

acf_SPY<-Acf(SPY, lag.max = 20, type = 'correlation')
plot(acf_SPY)
