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


lambda_AP<-BoxCox.lambda(ts_AP, method = 'guerrero')
lambda_AP
ts_AP_BC<-BoxCox(ts_AP,lambda_AP)
plot_AP_BC<-autoplot(ts_AP_BC)+
  geom_line(linewidth = 0.9, color = 'blue')+
  xlab('Tempo')+
  ylab('')+
  theme_minimal()
plot_AP_BC

lambda_co<-BoxCox.lambda(ts_co, method = 'guerrero')
lambda_co
ts_co_BC<-BoxCox(ts_co,lambda_co)
autoplot(ts_co_BC)+
  geom_line()
