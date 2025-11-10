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
plot<-autoplot(SPY,
         xlab = 'Tempo',
         ylab = 'Número médio')+
  geom_line(size = 0.9, colour = 'blue')+
  labs(title = 'Número médio de manchas solares por ano')+
  scale_x_continuous(breaks = seq(1700,1988, by = 25))+
  theme_light()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"          
  )

ggsave(filename = "Sunspot.jpg",
       plot = plot,
       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
       width = 8,
       height = 6,
       units = "in")

