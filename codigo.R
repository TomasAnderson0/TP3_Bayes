library(ggplot2)
library(rstan)
library(loo)
set.seed(1997)
beta0 = rnorm(1000, mean = 2.69, sd = 0.017)
beta1 = rnorm(1000, mean = -0.248, sd = 0.018)
varia = abs(rnorm(1000, mean = 0, sd = 0.01))
x = -rgamma(1000, shape = 5, scale = 0.1) + 5.883


prior = data.frame(beta0, beta1, x)

grid <- seq(4, 14, length.out = 100)
mu_prior_matrix <- matrix(nrow = 1000, ncol = 100)

for (i in seq_along(grid)) {
  mu_prior_matrix[, i] <- exp(prior$beta0 + prior$beta1 * (grid[i] - prior$x)) + 22
}

mu_mean <- apply(mu_prior_matrix, 2, mean)
mu_qts <- t(apply(mu_prior_matrix, 2, function(x) quantile(x, c(0.025, 0.975))))
mu_qts2 <- t(apply(mu_prior_matrix, 2, function(x) quantile(x, c(0.25, 0.75))))

# Finalmente, se lamacenan los valores calculados en un data frame
data_mu_prior <- data.frame(
  x = grid, 
  y = mu_mean,
  lower_95 = mu_qts[, 1],
  upper_95 = mu_qts[, 2],
  lower_50 = mu_qts2[, 1],
  upper_50 = mu_qts2[, 2]
)

ggplot(data_mu_prior) +
  geom_ribbon(
    aes(x, ymin = lower_95, ymax = upper_95),
    fill = "grey50",
    alpha = 0.6
  ) +
  geom_ribbon(
    aes(x, ymin = lower_50, ymax = upper_50),
    fill = "grey35",
    alpha = 0.6
  ) +
  geom_line(
    aes(x, y), 
    color = "red"
  ) +
  geom_hline(yintercept = c(37.5, 36), color = "green") +
  geom_vline(xintercept = c(5.83), color = "blue") + theme_bw() +
  scale_x_continuous(breaks = c(4, 5.83, 8, 10, 12, 14, 16, 18, 20, 22, 24),
                     labels = c("4:00", "5:53", "8:00", "10:00",
                                "12:00", "14:00", "16:00", "18:00",
                                "20:00", "22:00", "00:00"),
                     name = "Hora") +
  scale_y_continuous(name = "Temperatura (°C)", breaks = c(37.5, 36, 35, 32.5, 30, 27.5, 25, 22))

grafico <- NA
for(i in  1:1000) {
 y <-  prior$beta0[i] + prior$beta1[i] * (seq(3.5, 24, length.out = 100) - x[i])
 x1 <- seq(3.5, 24, length.out = 100)
 muestra <- i
 lol <- cbind(y, x1, muestra)
 grafico <- rbind(grafico, lol)
}

grafico <- data.frame(grafico)
grafico$muestra <- as.factor(grafico$muestra)
View(grafico)
ggplot(grafico) + aes(x = x1, y = y, group = muestra) + geom_line(alpha = 0.1) + theme(legend.position = "none")

ggplot(grafico) + aes(x = x1, y = (exp(y) + 22), group = muestra) + geom_line(alpha = 0.1) +
  geom_hline(yintercept = c(37.5, 36), color = "green") +
  geom_vline(xintercept = c(5.83), color = "blue") + theme_bw() +
  scale_x_continuous(breaks = c(4, 5.83, 8, 10, 12, 14, 16, 18, 20, 22, 24),
                     labels = c("4:00", "5:53", "8:00", "10:00",
                                "12:00", "14:00", "16:00", "18:00",
                                "20:00", "22:00", "00:00"),
                     name = "Hora") +
  scale_y_continuous(name = "Temperatura (°C)", breaks = c(37.5, 36, 35, 32.5, 30, 27.5, 25, 22),
                     limits = c(22, 40)) + geom_text(x = 15, y = 36.75, label = "Rango de temperatura inicial") +
  geom_text(x = 5.40, y = 27.5, label = "Llegada de los policias", angle = 90)



dgamma(seq(0,5, length.out = 100), shape = 2, scale = 0.5)
ggplot() + aes(y = dgamma(seq(0,5, length.out = 100), shape = 2, scale = 0.4), x = (-(seq(0,5, length.out = 100)) + 5.883)) +
  geom_line() + geom_vline(xintercept = 5.5)

# Correr el modelo Stan y ver los posterior

init_list <- list(
  list(b0 = 2.7, b1 = 0.25, sigma = , x =),
  list(a = 0.5, b = -0.1),
  list(a = 0.2, b = 0.05)
)

primera_obs <- list(
  N = 1, 
  t = 6.75, 
  y = log(32.8 - 22)
)

modelo1obs <- stan( 
  file = "Modelo1dato.stan",
  data = primera_obs,
  model_name = "modelo 1 obs",
  chains = 3,
  refresh = 0,
  seed = 1997,
  iter = 10000,
  control = list(adapt_delta = 0.99),
  warmup = 1000
)

traceplot(modelo1obs)
modelo1obs
posterior1 <- data.frame(extract(modelo1obs, c("b0", "b1", "x" ,"sigma")), observaciones = "1 observación")
ggplot(posterior1) + aes(x = b0) + geom_histogram() + geom_line(aes(x = seq(2.64, 2.76, length.out = 27000), y = 125*dnorm(seq(2.64, 2.76, length.out = 27000), mean = 2.69, sd = 0.017)))
ggplot(posterior1) + aes(x = b1) + geom_histogram() + geom_line(aes(x = seq(-0.3, -0.175, length.out = 27000), y = 120*dnorm(seq(-0.3, -0.175, length.out = 27000), mean = -0.2465, sd = 0.015)))
ggplot(posterior1) + aes(x = sigma) + geom_histogram() + geom_line(aes(x = abs(seq(-0.04, 0.04, length.out = 27000)),
                                                                       y = 70*abs(dnorm(seq(-0.04, 0.04, length.out = 27000),
                                                                                         mean = 0, sd = 0.007))))
ggplot(posterior1) + aes(x = x) + geom_histogram() + geom_line(aes(y = 1350*dgamma(seq(0,1.2, length.out = 27000), shape = 5, scale = 0.1), x = (-(seq(0,1.2, length.out = 27000)) + 5.883)))


segunda_obs <- list(
  N = 2, 
  t = c(6.75, 8.25), 
  y = c(log(32.8 - 22), log(30.5 - 22))
)

modelo2obs <- stan( 
  file = "Modelo.stan",
  data = segunda_obs,
  model_name = "modelo 2 obs",
  chains = 3,
  refresh = 0,
  seed = 1997,
  warmup = 1000,
  control = list(adapt_delta = 0.99),
  iter = 10000
)

modelo2obs

traceplot(modelo2obs)
posterior2 <- data.frame(extract(modelo2obs, c("b0", "b1", "x" ,"sigma")), observaciones = "2 observaciones")
posterior12 <- rbind(posterior1, posterior2)
ggplot(posterior12) + aes(x = b0, fill = observaciones) + geom_density(alpha = 0.5)# + geom_line(aes(x = seq(2.62, 2.77, length.out = 27000), y = 150*dnorm(seq(2.62, 2.77, length.out = 27000), mean = 2.69, sd = 0.017)))
ggplot(posterior12) + aes(x = b1, fill = observaciones) + geom_density(alpha = 0.5)
ggplot(posterior12) + aes(x = sigma, fill = observaciones) + geom_density(alpha = 0.5)
ggplot(posterior12) + aes(x = x, fill = observaciones) + geom_density(alpha = 0.5)

tercera_obs <- list(
  N = 3, 
  t = c(6.75, 8.25, 13.5), 
  y = c(log(32.8 - 22), log(30.5 - 22), log(23.7 - 22))
)

modelo3obs <- stan( 
  file = "Modelo.stan",
  data = tercera_obs,
  model_name = "modelo 3 obs",
  chains = 3,
  refresh = 0,
  seed = 1997,
  warmup = 1000,
  control = list(adapt_delta = 0.99),
  iter = 10000
)

loo(modelo3obs)

loo_compare(loo(modelo2obs),loo(modelo3obs))

traceplot(modelo3obs)
modelo3obs
posterior3 <- data.frame(extract(modelo3obs, c("b0", "b1", "x" ,"sigma")), observaciones = "3 observaciones")
posterior23 <- rbind(posterior2, posterior3)
ggplot(posterior23) + aes(x = b0, fill = observaciones) + geom_density(alpha = 0.5)
ggplot(posterior23) + aes(x = b1, fill = observaciones) + geom_density(alpha = 0.5)
ggplot(posterior23) + aes(x = sigma, fill = observaciones) + geom_density(alpha = 0.5)
ggplot(posterior23) + aes(x = x, fill = observaciones) + geom_density(alpha = 0.5)

posterior123 <- rbind(posterior1, posterior2, posterior3)
ggplot(posterior123) + aes(x = x, fill = observaciones) + geom_density(alpha = 0.5) + geom_line(aes(y = dgamma(seq(0,1.2, length.out = 81000), shape = 5, scale = 0.1), x = (-(seq(0,1.2, length.out = 81000)) + 5.883)))


# Predicciones del modelo con 2 obs


grafico_post <- NA
for(i in  1:1000) {
  y <-  posterior2$b0[i] + posterior2$b1[i] * (seq(3.5, 14, length.out = 100) - 
                                                 posterior2$x[i])
  x1 <- seq(3.5, 14, length.out = 100)
  muestra <- i
  lol <- cbind(y, x1, muestra)
  grafico_post <- rbind(grafico_post, lol)
}

predicciones <- data.frame(y = exp(extract(modelo2obs, "log_dif_temp")$log_dif_temp[,1]) + 22, x = 6.75)
predicciones <- rbind(data.frame(y = exp(extract(modelo2obs, "log_dif_temp")$log_dif_temp[,2]) + 22, x = 8.25), predicciones)

x_grid <- seq(4, 14, length.out = 100)
mu_matrix <- matrix(nrow = 27000, ncol = 100)

for (i in seq_along(x_grid)) {
  mu_matrix[, i] <- exp(posterior2$b0 + posterior2$b1 * (x_grid[i] - posterior2$x)) + 22
}

mu_mean <- apply(mu_matrix, 2, mean)
mu_qts <- t(apply(mu_matrix, 2, function(x) quantile(x, c(0.025, 0.975))))
mu_qts2 <- t(apply(mu_matrix, 2, function(x) quantile(x, c(0.25, 0.75))))

data_mu <- data.frame(
  x = x_grid, 
  y = mu_mean,
  lower_95 = mu_qts[, 1],
  upper_95 = mu_qts[, 2],
  lower_50 = mu_qts2[, 1],
  upper_50 = mu_qts2[, 2]
)

ggplot() + 
  geom_ribbon(
    aes(x, ymin = lower_95, ymax = upper_95),
    fill = "grey50",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_ribbon(
    aes(x, ymin = lower_50, ymax = upper_50),
    fill = "grey35",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_line(
    aes(x, y), 
    color = "firebrick",
    data = data_mu
  ) +
  stat_summary(data = predicciones, fun.data = mean_sdl,
               mapping = aes(x = x, y = y), color = "blue", size = 0.5) +
  scale_y_continuous(limits = c(28, 36), breaks = seq(28, 36, 1),
                     name = "Temperatura (ºC)") + 
  scale_x_continuous(limits = c(6, 9), breaks = seq(6, 9, 0.5), 
                     labels = c("6:00", "6:30", "7:00", "7:30",
                                "8:00", "8:30", "9:00"), 
                     name = "Hora") +
  geom_point(aes(x = c(6.75, 8.25), y = c(32.8, 30.5)),
             shape = 13, size = 2, color = "red") +
  theme_bw()

