library(rbcb)
library(forecast)
library(ggplot2)
library(readr)
library(dplyr)

dados_divida <- get_series(
  code = c("Divida_Liquida_PIB" = 4504),
  start_date = "2001-12-31",
  end_date = "2025-10-30",
  as = "ts"
)

plot<-autoplot(dados_divida)+
  geom_line(size = 0.9, color = 'blue')+
  labs(title = "Dívida Liquida do setor público- Federal",
       subtitle= "Fonte: Banco Central do Brasil")+
  ylab("% PIB")+
  theme_minimal()
ggsave(filename = "Divida_liquida_BR.png",
       plot = plot,
       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)
show(plot)
