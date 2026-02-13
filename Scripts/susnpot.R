# PROJETO: ${PUB- SÉRIES TEMPORAIS}
# ARQUIVO: ${sunspot.R}
# AUTOR: ${José Fábio Viana de Brito}
# DATA: 14/09/2025
# DESCRIÇÃO: ANÁLISE DO NÚMERO MÉDIO DE MANCHAS SOLARES NO MUNDO

library('forecast')
library('ggplot2')
library(openxlsx)
library(stringr)

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

acf_SPY<-Acf(diff(sunspot.year), lag.max = 20, type = 'partial')

#simulando AR(p):

sim <- arima.sim(model = list(ar = 0.7), n = 100)
plot<-autoplot(sim,
               xlab = 'Tempo',
               ylab = 'Número médio',
               size =0.9,
               color = 'blue')+
  labs(title = 'Série simulada')+
  theme_minimal()+
  show(plot)
#Teste de modelo

treino <- head(SPY, -22)
teste  <- tail(SPY, 22)
fit_modelo<-Arima(treino,order = c(6,1,6))
autoplot(forecast(fit_modelo, h=22)) +
  autolayer(teste, series="Dados Reais") +
  labs(title = "Previsão Manchas solares vs Realidade",
       subtitle = str_glue("Modelo: {forecast:::arima.string(fit_modelo)}, AIC: {round(fit_modelo$aic,2)}"))+
  theme_minimal()

for (p in 1:15){
  fit_modelo<-Arima(SPY, order = c(p,1,0))
  print(forecast:::arima.string(fit_modelo))
  print(fit_modelo$aic)
}


library(forecast)

otimizar_arima <- function(dados, max_p=11, max_d=2, max_q=7) {
  
  # 1. Criar todas as combinações possíveis (O espaço de busca Z^3)
  grid <- expand.grid(p = 5:max_p, 
                      d = 0:max_d, 
                      q = 0:max_q)
  
  # Coluna para armazenar o AIC (iniciamos com NA)
  grid$AIC <- NA
  
  # Barra de progresso (opcional, mas útil para grids grandes)
  total <- nrow(grid)
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  
  # 2. Loop através do Grid
  for(i in 1:total) {
    
    # Extrair parâmetros da linha atual
    p_try <- grid$p[i]
    d_try <- grid$d[i]
    q_try <- grid$q[i]
    
    # 3. Bloco de Tentativa (TryCatch)
    # É CRUCIAL em loops ARIMA, pois alguns modelos falham ao convergir
    resultado <- tryCatch({
      
      # Ajusta o modelo
      modelo <- Arima(dados, order = c(p_try, d_try, q_try), method="ML")
      
      # Retorna o AIC
      modelo$aic
      
    }, error = function(e) {
      # Se der erro, retorna Infinito para que esse modelo nunca seja escolhido
      return(Inf)
    })
    
    grid$AIC[i] <- resultado
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  # 4. Limpeza e Ordenação
  # Remove modelos que deram erro (AIC = Inf)
  grid_limpo <- grid[grid$AIC != Inf, ]
  
  # Ordena do menor AIC para o maior (Minimização)
  grid_final <- grid_limpo[order(grid_limpo$AIC), ]
  
  return(grid_final)
}

# --- Exemplo de Uso com seus dados ---
# Supondo que 'minha_serie' seja o seu objeto ts (ex: sunspot.year)
resultado <- otimizar_arima(treino)

# Visualizar o Top 5 modelos
head(resultado, 5)
