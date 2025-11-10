library(rbcb)
library(forecast)
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)

dados_dolar<-get_currency("USD","2011-08-01","2025-10-2025", "data.frame")
ts_compra<-ts(dados_dolar$bid, 
              start = c(year(dados_dolar$date[1]),yday(dados_dolar$date[1])), 
              frequency = 252)
ts_venda<-ts(dados_dolar$ask, 
              start = c(year(dados_dolar$date[1]),yday(dados_dolar$date[1])), 
              frequency = 252)

plot_bid<-autoplot(ts_compra)+
  geom_line(size = 0.5, color = 'blue')+
  labs(title = "Cotação de compra do Dólar",
       subtitle ="Fonte: Banco Central do Brasil" )+
  ylab("R$")+
  theme_minimal()
ggsave(filename = "Compra_dolar.png",
       plot = plot_bid,
       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)
show(plot)

plot_ask<-autoplot(ts_venda)+
  geom_line(size = 0.5, color = 'blue')+
  labs(title = "Cotação de venda do Dólar",
       subtitle ="Fonte: Banco Central do Brasil" )+
  ylab("R$")+
  theme_minimal()
ggsave(filename = "Venda_dolar.png",
       plot = plot_ask,
       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)
show(plot_ask)
