#Bibliotecas
library(forecast)
library(ggplot2)
library(stringr)

#Carregando dados
ts_AP<-AirPassengers
ts_co<-co2

#Gráfico da série temporal original
autoplot(ts_co)+
  geom_line(linewidth = 0.9, color = 'blue')+
  labs(title = 'Concentração atmosférica de CO2')+
  xlab("Tempo")+
  ylab("partes por milhão (ppm)")+
  theme_minimal()

#Detrending:
AP <- AirPassengers
time <- time(AP)
reg <- lm(AP ~ time)
plot <- autoplot(AP) +
  geom_line(linewidth = 0.9, color = 'blue') +
  geom_abline(intercept = coef(reg)[1], slope = coef(reg)[2], color = 'red', linewidth = 0.8) +
  labs(title = 'Número de passageiros 1949-1960') +
  xlab("Tempo") +
  ylab("Passageiros (x1000)") +
  theme_minimal()
show(plot)

plot_res <- autoplot(ts(reg$residuals, start = start(AP), frequency = frequency(AP))) +
  geom_line(linewidth = 0.8, color = 'darkorange') +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = 'Resíduos da Regressão Linear ao longo do Tempo') +
  xlab("Tempo") +
  ylab("Resíduos") +
  theme_minimal()
show(plot_res)



difs<-ndiffs(AP)
plot <- autoplot(diff(AP,difs)) +
  geom_line(linewidth = 0.9, color = 'blue') +
  labs(title = 'Número de passageiros diferenciados',
       subtitle = str_glue('Número de diferenciações:{difs}')) +
  xlab("Tempo") +
  ylab("Passageiros (x1000)") +
  theme_minimal()
show(plot)

#Média móvel central de ordem 5
ts_AP_MAC5<-ma(ts_AP,order = 5, centre = TRUE)
ts_co_MAC5<-ma(ts_co,order = 5, centre = TRUE)

#Média Móvel central de ordem 12
ts_AP_MAC12<-ma(ts_AP,order = 12, centre = TRUE)
ts_co_MAC12<-ma(ts_co,order = 12, centre = TRUE)

#Média móvel não central de ordem 5
ts_AP_MANC5<- ma(ts_AP, order = 5, centre = FALSE)
ts_co_MANC5<- ma(ts_co, order = 5, centre = FALSE)

#Média móvel não central de ordem 12
ts_AP_MANC12<- ma(ts_AP, order = 12, centre = FALSE)
ts_co_MANC12<- ma(ts_co, order = 12, centre = FALSE)


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
AP_stl<- stl(ts_AP,s.window = 'periodic')
plot(AP_stl)

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
  autolayer(pred_SES, series = "Previsão com SES")
plot_Nile <- autoplot(Nile, series = "Original") +
  autolayer(fitted(fit_SES)[, "xhat"], series = "Suavização Exponencial") +
  labs(
    x = "Tempo",
    y = "Fluxo (x100hm³)",
    color = "Legenda"
  ) +
  scale_color_manual(
    values = c("Original" = "gray", 
               "Suavização Exponencial" = "red",
               "Previsão com SES" = "orange")
  )+
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
  autolayer(pred_Holt, series = "Previsão com Holt")
plot_Holt<- autoplot(uspop, series = "Original") +
  autolayer(fitted(fit_Holt)[, "xhat"], series = "Suavização de Holt") +
  labs(
    x = "Tempo",
    y = "População (1M)",
    color = "Legenda"
  ) +
  scale_color_manual(
    values = c("Original" = "gray", 
               "Suavização de Holt" = "red",
               "Previsão com Holt" = "orange")
  )+
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
  autolayer(pred_HW, series = "Previsão com Holt-Winters")
plot_HW<- autoplot(ts_AP, series = "Original") +
  autolayer(fitted(fit_HW)[, "xhat"], series = "Suavização de Holt-Winters") +
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


#Ajuste e seleção de modelos
treino <- head(AirPassengers, -12)
teste  <- tail(AirPassengers, 12)

fit_auto <- auto.arima(treino, lambda = 0)

fit_manual1 <- Arima(treino, 
                     order = c(1, 1, 2), 
                     seasonal = c(0, 1, 1), 
                     lambda = 0)

fit_manual2 <- Arima(treino, 
                     order = c(2, 1, 1), 
                     seasonal = c(0, 1, 0), 
                     lambda = 0)

extrair_metricas <- function(modelo, dados_teste) {
  aic_val <- modelo$aic
  prev <- forecast(modelo, h = length(dados_teste))
  acc  <- accuracy(prev, dados_teste)
  rmse_val <- acc[2, "RMSE"]
  mase_val <- acc[2, "MASE"]
  nome <- forecast:::arima.string(modelo, padding = FALSE)
  
  return(data.frame(Modelo = nome, 
                    AIC = round(aic_val, 2), 
                    RMSE_Teste = round(rmse_val, 2), 
                    MASE_Teste = round(mase_val, 3)))
}

tabela_comparativa <- bind_rows(
  extrair_metricas(fit_auto, teste),
  extrair_metricas(fit_manual1, teste),
  extrair_metricas(fit_manual2, teste)
) %>% 
  arrange(AIC)

print(tabela_comparativa)

melhor_modelo <- if(tabela_comparativa$AIC[1] <= fit_auto$aic) fit_auto else fit_manual1
autoplot(forecast(melhor_modelo, h=12)) +
  autolayer(teste, series="Dados Reais") +
  labs(title = "Previsão AirPassengers vs Realidade",
       subtitle = str_glue("Modelo:{forecast:::arima.string(melhor_modelo)}"))

checkresiduals(modelo, plot = TRUE)