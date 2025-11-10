library(rbcb)
library(forecast)
library(ggplot2)
library(readr)
library(dplyr)

dados_producao<-get_series(
  code = c("Producao_Total" = 1391),
  start_date = "1979-01-31",
  end_date = "2025-10-30",
  as = "ts"
)
plot_prod<-autoplot(dados_producao)+
  geom_line(size = 0.9, color = 'blue')+
  labs(title = "Produção de derivados de petróleo total",
       subtitle= "Fonte: Agência Nacional do Petróleo, Gás Natural e Biocombustíveis do Brasil")+
  ylab("Barris/dia (mil)")+
  theme_minimal()
ggsave(filename = "Producao_petroleo.png",
       plot = plot_prod,
       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)
show(plot)  



dados_consumo<-get_series(
  code = c("Consumo_Total" = 1398),
  start_date = "1979-01-31",
  end_date = "2025-10-30",
  as = "ts"
)
plot_cons<-autoplot(dados_consumo)+
  geom_line(size = 0.5, color = 'blue')+
  labs(title = "Consumo de derivados de petróleo total",
       subtitle= "Fonte: Agência Nacional do Petróleo, Gás Natural e Biocombustíveis do Brasil")+
  ylab("Barris/dia (mil)")+
  theme_minimal()
ggsave(filename = "Consumo_petroleo.png",
       plot = plot_cons,
       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)
show(plot)
