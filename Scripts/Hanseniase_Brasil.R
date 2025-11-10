# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${Hanseniase.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 19/09/2025
# DESCRIÇÃO: ANÁLISE DO NÚMERO DE NOTIFICAÇÕES DE HANSENIASE NO SUS

library(openxlsx)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)

HBR<-read.xlsx("Datasets/Hanseniase_Brasil.xlsx")
cols<- colnames(HBR)[2:length(colnames(HBR))]
for (col_name in cols) {
  HBR[[col_name]][HBR[[col_name]] == "-"] <- 0
}
HBR <- HBR %>%
  mutate(across(-UF, as.integer))

TT_BR<- filter(HBR, UF=="Total")
TT_BR<- pivot_longer(TT_BR,
                     cols = -UF,
                     names_to = "Data",
                     values_to = "Notificações")
TT_BR_TS<-ts(TT_BR$Notificações,start = 1970,frequency = 1)
autoplot(TT_BR_TS, ylab = "Notificações")+
  labs(title = "Número de Notificações de Hanseníase no Brasil",
       subtitle = "Fonte: SUS")+
  geom_line(size = 1.1, colour = 'blue')+
  theme_minimal()
