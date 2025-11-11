library(forecast)
library(ggplot2)

ts_AP<-AirPassengers
ts_AP_MAC2<-ma(ts_AP,order = 2, centre = TRUE)
ts_AP_MAC12<-ma(ts_AP,order = 12, centre = TRUE)
plot_AP <- autoplot(ts_AP, series = "Passageiros") +
  autolayer(ts_AP_MAC2, series = "Média Móvel 2") +
  autolayer(ts_AP_MAC12, series = "Média Móvel 12") +
  labs(
    title = 'Número de passageiros internacionais',
    x = "Tempo",
    y = "Contagem",
    color = "Legenda"
  ) +
  scale_color_manual(
    values = c("Passageiros" = "gray", 
               "Média Móvel 2" = "red", 
               "Média Móvel 12" = "yellow")
  ) +
  theme_minimal()+
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "gray50"),
    legend.title = element_text(face = "bold")
  )

show(plot_AP)
AP_stl<- stl(ts_AP,s.window = 'periodic')
plot(AP_stl)


ts_co<-co2
autoplot(ts_co)+
  geom_line(linewidth = 0.9, color = 'blue')+
  labs(title = 'Concentração atmosférica de CO2')+
  xlab("Tempo")+
  ylab("partes por milhão (ppm)")+
  theme_minimal()
co_stl<-stl(ts_co, s.window = 'periodic')
plot(co_stl)


lambda_AP<-BoxCox.lambda(ts_AP, method = 'guerrero')
lambda_AP
ts_AP_BC<-BoxCox(ts_AP,lambda_AP)
autoplot(ts_AP_BC)+
  geom_line(linewidth = 0.9, color = 'blue')+
  xlab("Tempo")+
  ylab("")+
  theme_minimal()



