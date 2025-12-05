# Instalar o pacote se necessário
# install.packages("NADA")

library(NADA)

# Carregar os dados
df <- read.csv("multi_censored_lognormal_data.csv")

# Definir os vetores de interesse
valores <- df$value
cens <- df$censored == 0  # TRUE se censurado

# Aplicar o modelo de Kaplan-Meier para dados censurados (cenros)
cenfit <- cenfit(valores, cens)

# Imputar os valores censurados com a função cenros()
# Esta função substitui valores censurados por valores estimados usando K-M
imputado <- cenros(valores, cens)

# Ver o resultado
head(imputado)

# Acrescentar ao data.frame original
df$imputado_km <- imputado$modeled

# Guardar num ficheiro CSV, se necessário
write.csv(df, "imputado_kaplan_meier.csv", row.names = FALSE)
