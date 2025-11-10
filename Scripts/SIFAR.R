# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${SIFAR.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 28/09/2025
# DESCRIÇÃO: ANÁLISE DO NÚMERO DE CASOS DE SÍFILIS ADQUIRIDA NO BRASIL

library(stringr)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)


SIFBR<-read.xlsx("Datasets/SIFABR_GRL.xlsx")

ts_SIFBR<-ts(SIFBR$Casos, start = c(SIFBR$Ano[1],01), frequency = 1)
plot<-autoplot(ts_SIFBR,
               xlab = "Tempo",
               ylab = "Casos")+
  labs(title = "Número de casos de Sífilis Adquirida no Brasil",
       subtitle = "Fonte: SUS")+
  geom_line(size = 0.9, colour = "red")+
  theme_minimal()
show(plot)
