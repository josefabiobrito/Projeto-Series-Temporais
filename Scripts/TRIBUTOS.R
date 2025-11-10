library(ggplot2)
library(forecast)
library(tidyr)
library(dplyr)
library(stringr)
library(openxlsx)

ARRECADACAO<- read_excel("Datasets/arrecadacao-cnae.xlsx", sheet = 2)
ano<-ARRECADACAO$Ano
mes<-ARRECADACAO$Mês
ARRECADACAO<- ARRECADACAO%>%
  mutate(Data = paste(mes,sep = "-",ano))%>%
  select(-Ano,-`Mês`)
write.xlsx(ARRECADACAO,"C:/Users/josef/OneDrive/Documentos/PUB/Datasets/arrecadacao_geral.xlsx")

