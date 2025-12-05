
library(survival)
library(data.table)

# 1. Carregar e preparar
dados <- fread("multi_censored_lognormal_data.csv")
dados[, is_censored := censored == 0]
dados[, left := ifelse(is_censored, 0, value)]
dados[, right := value]
surv_obj <- with(dados, Surv(left, right, type = "interval2"))

# 2. Ajustar o modelo lognormal
mod_ln <- survreg(surv_obj ~ 1, dist = "lognormal")
mu    <- mod_ln$coefficients       # intercepto (log-scale location)
sigma <- mod_ln$scale               # scale parameter

# 3. Índices dos censurados e seus LOQs
idx_cens     <- which(dados$is_censored)
loqs         <- dados$value[idx_cens]

# 4. Para cada censurado, amostrar da log-normal truncada em [0, LOQ]:
set.seed(123)
u <- runif(length(loqs), min = 0, max = plnorm(loqs, meanlog = mu, sdlog = sigma))
x_imp <- qlnorm(u, meanlog = mu, sdlog = sigma)

# 5. Atribuir de volta, garantindo ≤ LOQ
x_imp <- pmin(x_imp, loqs)
dados[, imputado := value]
dados[idx_cens, imputado := x_imp]

# 6. Conferir
summary(dados[is_censored == TRUE, .(LOQ = value, imputado)])


# 11. (Opcional) Exportar resultado
# fwrite(dados, "dados_imputados_survreg.csv")
