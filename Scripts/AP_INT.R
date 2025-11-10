library(forecast)
library(ggplot2)

ts_AP<-AirPassengers
autoplot(ts_AP)+
  geom_line(linewidth = 0.9, color = 'blue')+
  labs(title = 'Número de passageiros internacionais')+
  xlab("Tempo")+
  ylab("Contagem")+
  theme_minimal()
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
