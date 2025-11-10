# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${ARRECADACAO.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 22/09/2025
# DESCRIÇÃO: ANÁLISE DA  RECEITA DE ARRECADAÇÃO TRIBUTÁRIA 
#            BRUTA DA RECEITA FEDERAL BRASILEIRA


library(ggplot2)
library(forecast)
library(tidyr)
library(dplyr)
library(stringr)
library(openxlsx)

ARRECADACAO<- read.xlsx("Datasets/arrecadacao-cnae.xlsx", sheet= "arrecadacao_total")
ts_ARRRECADACAO<-ts(ARRECADACAO$Receita,start = c(2016,01), frequency = 12)
plot<-autoplot(ts_ARRRECADACAO,ylab = "Receita Bruta(1BR$)")+
  labs(title = "Arrecadação Bruta do Brasil",
       subtitle = "Fonte: Ministerio da Fazenda")+
  geom_line(size = 0.9, colour = "blue")+
  theme_minimal()
show(plot)
ggsave(filename = "Arrecadação_Bruta.png",
       plot = plot,
       path = "Gráficos",
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)
