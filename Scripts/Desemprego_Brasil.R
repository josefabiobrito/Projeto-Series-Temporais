# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${Desemprego_Brasil.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 12/09/2025
# DESCRIÇÃO: ANÁLISE DA TAXA DE DESOCUPAÇÃO E RENDIMENTO MÉDIO NO BRASIL


library('tsibble')
library('dplyr')
library('forecast')
library('ggplot2')
library('stringr')
library(openxlsx)

df<-read.csv("Datasets/20250912071448.csv.csv")|>
  rename(Taxa = Taxa.de.desocupação)

meses<- c('jan' = 1,'fev' = 2,'mar' = 3,'abr' = 4,'mai' = 5,'jun' = 6 , 'jul'= 7,
          'ago'=8,'set'=9,'out'=10,'nov'=11,'dez'=12)
df_final <- df |>
  mutate(
    Mês = recode(
      str_extract(Tempo, "\\w{3}(?=\\s\\d{4}$)"), 
      !!!meses
    ),
    Ano = as.integer(str_extract(Tempo, "\\d{4}"))
    )|>
  select(Mês, Ano, Taxa)

write.xlsx(df_final,'C:/Users/josef/OneDrive/Documentos/PUB/df_desemprego_BR.xlsx')
ts_desemprego<-ts(df_final$Taxa, start = c(df_final$Ano[1],df_final$Mês[1]), frequency = 12)


autoplot(ts_desemprego, 
         xlab = "Tempo",
         ylab = "Taxa de Desocupação",
         colour = "blue") +
  labs(title = "Série Histórica do Desemprego no Brasil",
       subtitle = "Fonte: IBGE",
       colour = "Legenda da Série") +
  scale_x_continuous(breaks = seq(2012, 2025, by = 2)) +
  theme_minimal() +                      
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"          
  )



df2<- readxl::read_excel("C:/Users/josef/Downloads/Tabela_Ocupacao (1).xlsx")
df2_final<-df2 |>
  mutate(
    Mês = recode(
      str_extract(Tempo, "\\w{3}(?=\\s\\d{4}$)"), 
      !!!meses
    ),
    Ano = as.integer(str_extract(Tempo, "\\d{4}"))
  )|>
  select(Mês, Ano, Rendimento)
ts_Rendimento<- ts(df2_final$Rendimento,start =c(df2_final$Ano[1],df2_final$Mês[1]),frequency = 12)
autoplot(ts_Rendimento, 
         xlab = "Tempo",
         ylab = "Rendimento Médio Mensal",
         colour = "blue") +
  geom_line(size = 1, colour = "blue")+
  labs(title = "Série Histórica do Rendimento Médio no Brasil",
       subtitle = "Fonte: IBGE",
       colour = "Legenda da Série") +
  scale_x_continuous(breaks = seq(2012, 2025, by = 2)) +
  theme_minimal() +                      
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"          
  )




