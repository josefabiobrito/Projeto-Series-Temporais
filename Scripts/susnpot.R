# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${sunspot.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 14/09/2025
# DESCRIÇÃO: ANÁLISE DO NÚMERO MÉDIO DE MANCHAS SOLARES NO MUNDO

library('forecast')
library('ggplot2')
library(openxlsx)
library(stringr)
library(tseries)

SPY<-sunspot.year

class(SPY)
start(SPY);end(SPY);frequency(SPY)
plot<-autoplot(sunspot.year,
         xlab = 'Tempo',
         ylab = 'Número médio',
         size =0.9,
         color = 'blue')+
  labs(title = 'Número médio de manchas solares por ano')+
  theme_minimal()+
show(plot)
#ggsave(filename = "Sunspot.jpg",
#       plot = plot,
#       path = "C:/Users/josef/OneDrive/Documentos/PUB/Gráficos",
#       width = 8,
#       height = 6,
#       units = "in")

adf.test(diff(sunspot.year))

n_diffs <- 0

acf_SPY <- ggAcf(sunspot.year, lag.max = 26) +
  labs(
    title = "Correlograma"
  )

acf_SPY

pacf_SPY<-ggAcf(sunspot.year,lag.max = 26, type = 'partial')+
  labs(
    title = "Correlograma parcial"
  )
pacf_SPY

#simulando AR(p):

sim <- arima.sim(model = list(ar = 0.7), n = 150)
plot<-autoplot(sim,
               xlab = 'Tempo',
               ylab = 'Número médio',
               size =0.9,
               color = 'blue')+
  labs(title = 'Série simulada')+
  theme_minimal()+
show(plot)
#Teste de modelo
p <- as.integer(length(sunspot.year) * 0.90)

treino <- head(sunspot.year, p)
teste  <- tail(sunspot.year, length(sunspot.year) - p)
fit_806<-Arima(sunspot.year, order = c(8,0,6))
rmse_806<-accuracy(fit_806)[1,2]
fit_809<-Arima(sunspot.year, order = c(8,0,9))
rmse_809<-accuracy(fit_809)[1,2]
autoplot(SPY, ylab = "Manchas Solares")+
  autolayer(fit_806$fitted, PI=FALSE, series=forecast:::arima.string(fit_806))+
  autolayer(fit_809$fitted, PI=FALSE, series=forecast:::arima.string(fit_809))+
  labs(title = "Ajuste do modelo de Manchas solares vs Realidade",
       caption = str_glue("RMSE, AIC:\n ARIMA(8,0,9):{round(rmse_809,3)}, {round(fit_806$aic,3)} 
                          \n ARIMA(8,0,6): {round(rmse_806,3)}, {round(fit_809$aic,3)}"))+
  theme_minimal()



for (p in 1:15){
  fit_modelo<-Arima(sunspot.year, order = c(8,0,p))
  print(forecast:::arima.string(fit_modelo))
  print(fit_modelo$aic)
}


#RESÍDUOS MODELO

res<-fit_809$residuals
autoplot(res,
         xlab = 'Tempo',
         ylab = 'Resíduos',
         size =0.9,
         color = 'blue')+
  labs(title = 'Série dos resíduos')+
  theme_minimal()
 
 
acf_Res <- ggAcf(res, lag.max = 26) +
  labs(
    title = "Correlograma"
  )

pacf_Res<-ggAcf(res,lag.max = 26, type = 'partial')+
  labs(
    title = "Correlograma parcial"
  )

acf_Res
pacf_Res

library(car)

qqPlot(as.numeric(residuals(fit_809)), 
       main = "Normal Q-Q Plot: Resíduos do ARIMA(8,0,9)",
       ylab = "Quantis da Amostra",
       xlab = "Quantis Teóricos",
       pch = 20, 
       col.lines = "red") 

library(forecast)

# Roda o Ljung-Box e plota os gráficos de diagnóstico
checkresiduals(fit_809,lag = 25)

Box.test(res, type = "Ljung-Box", lag = 25, fitdf = 17)
