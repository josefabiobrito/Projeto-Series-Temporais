library(forecast)
library(ggplot2)

ts_AP<-AirPassengers
ts_AP_MAC5<-ma(ts_AP,order = 5, centre = TRUE) #Média móvel central de ordem 5
ts_AP_MAC12<-ma(ts_AP,order = 12, centre = TRUE) #Média Móvel central de ordem 12
ts_AP_MANC5<- ma(ts_AP, order = 5, centre = FALSE) #Média móvel não central de ordem 5
ts_AP_MANC12<- ma(ts_AP, order = 12, centre = FALSE)#Média móvel não central de ordem 12
#Gráfico das médias moveis centrais
plot_APMAC <- autoplot(ts_AP, series = "Passageiros") +
  autolayer(ts_AP_MAC5, series = "Média Móvel central 5") +
  autolayer(ts_AP_MAC12, series = "Média Móvel central 12") +
  labs(
    x = "Tempo",
    y = "Contagem (x1.000)",
    color = "Legenda"
  ) +
  scale_color_manual(
    values = c("Passageiros" = "gray", 
               "Média Móvel central 5" = "red", 
               "Média Móvel central 12" = "yellow")
  ) +
  theme_minimal()+
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "gray50"),
    legend.title = element_text(face = "bold")
  )
show(plot_APMAC)

#Gráfico das médias moveis não-centrais
plot_APMANC <- autoplot(ts_AP, series = "Passageiros") +
  autolayer(ts_AP_MANC5, series = "Média Móvel  não central 5") +
  autolayer(ts_AP_MANC12, series = "Média Móvel não central 12") +
  labs(
    x = "Tempo",
    y = "Contagem (x1.000)",
    color = "Legenda"
  ) +
  scale_color_manual(
    values = c("Passageiros" = "gray", 
               "Média Móvel  não central 5" = "red", 
               "Média Móvel não central 12" = "yellow")
  ) +
  theme_minimal()+
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "gray50"),
    legend.title = element_text(face = "bold")
  )
show(plot_APMANC)

#Decomposição usando STL
AP_stl<- stl(ts_AP,s.window = 'periodic')
plot(AP_stl)


ts_co<-co2
ts_co_MAC5<-ma(ts_co,order = 5, centre = TRUE) #Média móvel central de ordem 5
ts_co_MAC12<-ma(ts_co,order = 12, centre = TRUE) #Média móvel central de ordem 12
ts_co_MANC5<- ma(ts_co, order = 5, centre = FALSE) #Média móvel não central de ordem 5
ts_co_MANC12<- ma(ts_co, order = 12, centre = FALSE) #Média móvel não central de ordem 12

#Gráfico da série temporal original
autoplot(ts_co)+
  geom_line(linewidth = 0.9, color = 'blue')+
  labs(title = 'Concentração atmosférica de CO2')+
  xlab("Tempo")+
  ylab("partes por milhão (ppm)")+
  theme_minimal()

#Gráfico das médias moveis centrais
plot_coMAC <- autoplot(ts_co, series = "Concentração") +
  autolayer(ts_co_MAC5, series = "Média Móvel central 5") +
  autolayer(ts_co_MAC12, series = "Média Móvel central 12") +
  labs(
    x = "Tempo",
    y = "partes por milhao (ppm)",
    color = "Legenda"
  ) +
  scale_color_manual(
    values = c("Concentração" = "gray", 
               "Média Móvel central 5" = "red", 
               "Média Móvel central 12" = "yellow")
  ) +
  theme_minimal()+
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "gray50"),
    legend.title = element_text(face = "bold")
  )
show(plot_coMAC)

#Gráfico das médias móveis não centrais
plot_coMANC <- autoplot(ts_co, series = "Concentração") +
  autolayer(ts_co_MANC5, series = "Média Móvel  não central 5") +
  autolayer(ts_co_MANC12, series = "Média Móvel não central 12") +
  labs(
    x = "Tempo",
    y = "partes por milhão (ppm)",
    color = "Legenda"
  ) +
  scale_color_manual(
    values = c("Concentração" = "gray", 
               "Média Móvel  não central 5" = "red", 
               "Média Móvel não central 12" = "yellow")
  ) +
  theme_minimal()+
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "gray50"),
    legend.title = element_text(face = "bold")
  )
show(plot_coMANC)

#Decomposição usando STL
co_stl<-stl(ts_co, s.window = 'periodic')
plot(co_stl)


#Transformação de Box-Cox

lambda_AP<-BoxCox.lambda(ts_AP, method = 'guerrero')
lambda_AP
ts_AP_BC<-BoxCox(ts_AP,lambda_AP)
autoplot(ts_AP_BC)+
  geom_line(linewidth = 0.9, color = 'blue')+
  xlab("Tempo")+
  ylab("")+
  theme_minimal()


#SUAVIZAÇAO EXPONENCIAL SIMPLES
fit_SES<-HoltWinters(Nile, beta= FALSE, gamma = FALSE)
pred_SES<-predict(fit_SES, n.ahead=10)
plot_Nile <- autoplot(Nile, series = "Original") +
  autolayer(fitted(fit_SES)[, "xhat"], series = "Suavização Exponencial") +
  autolayer(pred_SES, series = "Previsão com SES")+
  labs(
    x = "Tempo",
    y = "Fluxo (x100hm³)",
    color = "Legenda"
  ) +
  scale_color_manual(
    values = c("Original" = "gray", 
               "Suavização Exponencial" = "red",
               "Previsão com SES" = "orange")
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.62, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "gray50"),
    legend.title = element_text(face = "bold")
  )
plot_Nile

#SUAVIZAÇÃO DE HOLT
fit_Holt<-HoltWinters(uspop,gamma = FALSE)
pred_Holt<- predict(fit_Holt, n.ahead = 5)
plot_Holt<- autoplot(uspop, series = "Original") +
  autolayer(fitted(fit_Holt)[, "xhat"], series = "Suavização de Holt") +
  autolayer(pred_Holt, series = "Previsão com Holt")+
  labs(
    x = "Tempo",
    y = "População (1M)",
    color = "Legenda"
  ) +
  scale_color_manual(
    values = c("Original" = "gray", 
               "Suavização de Holt" = "red",
               "Previsão com Holt" = "orange")
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "gray50"),
    legend.title = element_text(face = "bold")
  )
plot_Holt

#SUAVIZAÇÃO DE HOLT-WINTERS
fit_HW<-HoltWinters(AirPassengers, seasonal = 'multiplicative')
pred_HW<-predict(fit_HW, n.ahead = 10)
plot_HW<- autoplot(AirPassengers, series = "Original") +
  autolayer(fitted(fit_HW)[, "xhat"], series = "Suavização de Holt-Winters") +
  autolayer(pred_HW, series = "Previsão com Holt-Winters")+
  labs(
    x = "Tempo",
    y = "Contagem (x1.000)",
    color = "Legenda"
  ) +
  scale_color_manual(
    values = c("Original" = "gray", 
               "Suavização de Holt-Winters" = "red",
               "Previsão com Holt-Winters" = 'orange')
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "gray50"),
    legend.title = element_text(face = "bold")
  )
plot_HW
