library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(forecast)
library(stringr)
library(tidyr)

for (i in 

url <- "https://apidadosabertos.saude.gov.br/vacinacao/doses-aplicadas-pni-2025?limit=350&offset=0"

response <- GET(url)

if (status_code(response) == 200) {
  dados <- fromJSON(content(response, "text", encoding = "UTF-8"))
  options(max.print = 999999)
  View(dados)
  print(dados)
} else {
  print(paste("Erro na requisição:", status_code(response)))
}