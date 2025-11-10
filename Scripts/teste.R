# Ler o arquivo CSV
df <- read.csv("Datasets/arrecadacao-cnae.csv")

# Identificar colunas de tributos
colunas_tributos <- names(df)[5:ncol(df)]

# Converter para numérico
for (col in colunas_tributos) {
  df[[col]] <- as.numeric(gsub("\\.", "", gsub(",", ".", df[[col]])))
}

# Calcular total por linha
df$Total <- rowSums(df[colunas_tributos], na.rm = TRUE)

# Agrupar por Ano e Mês
arrecadacao_mensal <- aggregate(Total ~ Ano + Mês, data = df, sum)

# Filtrar a partir de 2016
arrecadacao_2016_em_diante <- arrecadacao_mensal[arrecadacao_mensal$Ano >= 2016, ]

# Ordenar
arrecadacao_2016_em_diante <- arrecadacao_2016_em_diante[order(arrecadacao_2016_em_diante$Ano, arrecadacao_2016_em_diante$Mês), ]

# Visualizar resultado
print(arrecadacao_2016_em_diante)