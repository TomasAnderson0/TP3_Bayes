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
  scale_y_continuous(name = "Temperatura (°C)", breaks = c(37.5, 36, 35, 32.5, 30, 27.5, 25, 22)) +
  geom_text(x = 10, y = 36.75, label = "Rango de temperatura inicial", color = "#333333") +
  geom_text(x = 5.40, y = 27.5, label = "Llegada de los policias", angle = 90, color = "#333333")



# Correr el modelo Stan y ver los posterior

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
posterior1 <- data.frame(extract(modelo1obs, c("b0", "b1", "x" ,"sigma")), observaciones = "1 observación")
ggplot(posterior1) + aes(x = b0) + geom_histogram() + geom_line(aes(x = seq(2.64, 2.76, length.out = 27000), y = 125*dnorm(seq(2.64, 2.76, length.out = 27000), mean = 2.69, sd = 0.017)))
ggplot(posterior1) + aes(x = b1) + geom_histogram() + geom_line(aes(x = seq(-0.3, -0.175, length.out = 27000), y = 120*dnorm(seq(-0.3, -0.175, length.out = 27000), mean = -0.2465, sd = 0.015)))
ggplot(posterior1) + aes(x = sigma) + geom_histogram() + geom_line(aes(x = abs(seq(-0.04, 0.04, length.out = 27000)),
                                                                       y = 70*abs(dnorm(seq(-0.04, 0.04, length.out = 27000),
                                                                                         mean = 0, sd = 0.007))))
ggplot(posterior1) + aes(x = x) + geom_density(fill = "pink", alpha = 0.5) + geom_line(aes(y = dgamma(seq(0,1.2, length.out = 27000), shape = 5, scale = 0.1), x = (-(seq(0,1.2, length.out = 27000)) + 5.883)))

# Predicciones del modelo con 1 obs

predicciones1 <- data.frame(y = exp(extract(modelo1obs, "log_dif_temp")$log_dif_temp) + 22, x = 6.75)

x_grid <- seq(4, 14, length.out = 100)
mu_matrix <- matrix(nrow = 27000, ncol = 100)

for (i in seq_along(x_grid)) {
  mu_matrix[, i] <- exp(posterior1$b0 + posterior1$b1 * (x_grid[i] - posterior1$x)) + 22
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
  scale_y_continuous(name = "Temperatura (°C)", breaks = c(42.5, 40, 37.5, 36, 35, 32.5, 30, 27.5, 25, 22)) +
  scale_x_continuous(breaks = seq(6, 9, 0.5), 
                     labels = c("6:00", "6:30", "7:00", "7:30",
                                "8:00", "8:30", "9:00"), 
                     name = "Hora") +
  geom_point(aes(x = c(6.75), y = c(32.8)),
             shape = 13, size = 2, color = "red") +
  theme_bw() 

# Modelo 2

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

traceplot(modelo2obs, pars = c("b0", "b1", "sigma", "x"))
posterior2 <- data.frame(extract(modelo2obs, c("b0", "b1", "x" ,"sigma")), observaciones = "2 observaciones")
posterior12 <- rbind(posterior1, posterior2)
ggplot(posterior12) + aes(x = b0, fill = observaciones) + geom_density(alpha = 0.5)# + geom_line(aes(x = seq(2.62, 2.77, length.out = 27000), y = 150*dnorm(seq(2.62, 2.77, length.out = 27000), mean = 2.69, sd = 0.017)))
ggplot(posterior12) + aes(x = b1, fill = observaciones) + geom_density(alpha = 0.5)
ggplot(posterior12) + aes(x = sigma, fill = observaciones) + geom_density(alpha = 0.5)
ggplot(posterior12) + aes(x = x, fill = observaciones) + geom_density(alpha = 0.5) + theme_bw()

# Predicciones del modelo con 2 obs

predicciones2 <- data.frame(y = exp(extract(modelo2obs, "log_dif_temp")$log_dif_temp[,1]) + 22, x = 6.75)
predicciones2 <- rbind(data.frame(y = exp(extract(modelo2obs, "log_dif_temp")$log_dif_temp[,2]) + 22, x = 8.25), predicciones2)

x_grid2 <- seq(4, 24, length.out = 200)
mu_matrix2 <- matrix(nrow = 27000, ncol = 200)

for (i in seq_along(x_grid2)) {
  mu_matrix2[, i] <- exp(posterior2$b0 + posterior2$b1 * (x_grid2[i] - posterior2$x)) + 22
}

mu_mean2 <- apply(mu_matrix2, 2, mean)
mu_qts2 <- t(apply(mu_matrix2, 2, function(x) quantile(x, c(0.025, 0.975))))
mu_qts22 <- t(apply(mu_matrix2, 2, function(x) quantile(x, c(0.25, 0.75))))

data_mu2 <- data.frame(
  x = x_grid2, 
  y = mu_mean2,
  lower_95 = mu_qts2[, 1],
  upper_95 = mu_qts2[, 2],
  lower_50 = mu_qts22[, 1],
  upper_50 = mu_qts22[, 2]
)

ggplot() + 
  geom_ribbon(
    aes(x, ymin = lower_95, ymax = upper_95),
    fill = "grey50",
    alpha = 0.6,
    data = data_mu2
  ) +
  geom_ribbon(
    aes(x, ymin = lower_50, ymax = upper_50),
    fill = "grey35",
    alpha = 0.6,
    data = data_mu2
  ) +
  geom_line(
    aes(x, y), 
    color = "firebrick",
    data = data_mu2
  ) +
  stat_summary(data = predicciones2, fun.data = mean_sdl,
               mapping = aes(x = x, y = y), color = "blue", size = 0.5) +
  scale_y_continuous(breaks = seq(22, 44, 2),
                     name = "Temperatura (ºC)") + 
  scale_x_continuous(limits = c(4, 24), breaks = seq(4, 24, 2), 
                     labels = c("4:00", "6:00", "8:00", "10:00",
                                "12:00", "14:00", "16:00","18:00",
                                "20:00", "22:00", "00:00"), 
                     name = "Hora") +
  geom_point(aes(x = c(6.75, 8.25), y = c(32.8, 30.5)),
             shape = 13, size = 2, color = "red") +
  theme_bw()

ggplot() + 
  geom_ribbon(
    aes(x, ymin = lower_95, ymax = upper_95),
    fill = "grey50",
    alpha = 0.6,
    data = data_mu2
  ) +
  geom_ribbon(
    aes(x, ymin = lower_50, ymax = upper_50),
    fill = "grey35",
    alpha = 0.6,
    data = data_mu2
  ) +
  geom_line(
    aes(x, y), 
    color = "firebrick",
    data = data_mu2
  ) +
  stat_summary(data = predicciones2, fun.data = mean_sdl,
               mapping = aes(x = x, y = y), color = "blue", size = 0.5) +
  scale_y_continuous(name = "Temperatura (°C)",
                     breaks = c(42.5, 40, 37.5, 36, 35,
                                32.5, 30, 27.5, 25, 22),
                     limits = c(27, 36)) +
  scale_x_continuous(limits = c(6, 9), breaks = seq(6, 9, 0.5), 
                     labels = c("6:00", "6:30", "7:00", "7:30",
                                "8:00", "8:30", "9:00"), 
                     name = "Hora") +
  geom_point(aes(x = c(6.75, 8.25), y = c(32.8, 30.5)),
             shape = 13, size = 2, color = "red") +
  theme_bw()

# Modelo con 3 obs

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

# Predicciones del modelo con 3 obs

predicciones3 <- data.frame(y = exp(extract(modelo3obs, "log_dif_temp")$log_dif_temp[,1]) + 22, x = 6.75)
predicciones3 <- rbind(data.frame(y = exp(extract(modelo3obs, "log_dif_temp")$log_dif_temp[,2]) + 22, x = 8.25), predicciones3)
predicciones3 <- rbind(data.frame(y = exp(extract(modelo3obs, "log_dif_temp")$log_dif_temp[,3]) + 22, x = 13.5), predicciones3)

x_grid3 <- seq(4, 14, length.out = 100)
mu_matrix3 <- matrix(nrow = 27000, ncol = 100)

for (i in seq_along(x_grid3)) {
  mu_matrix3[, i] <- exp(posterior3$b0 + posterior3$b1 * (x_grid3[i] - posterior3$x)) + 22
}

mu_mean3 <- apply(mu_matrix3, 2, mean)
mu_qts3 <- t(apply(mu_matrix3, 2, function(x) quantile(x, c(0.025, 0.975))))
mu_qts23 <- t(apply(mu_matrix3, 2, function(x) quantile(x, c(0.25, 0.75))))

data_mu3 <- data.frame(
  x = x_grid3, 
  y = mu_mean3,
  lower_95 = mu_qts3[, 1],
  upper_95 = mu_qts3[, 2],
  lower_50 = mu_qts23[, 1],
  upper_50 = mu_qts23[, 2]
)

ggplot() + 
  geom_ribbon(
    aes(x, ymin = lower_95, ymax = upper_95),
    fill = "grey50",
    alpha = 0.6,
    data = data_mu3
  ) +
  geom_ribbon(
    aes(x, ymin = lower_50, ymax = upper_50),
    fill = "grey35",
    alpha = 0.6,
    data = data_mu3
  ) +
  geom_line(
    aes(x, y), 
    color = "firebrick",
    data = data_mu3
  ) +
  stat_summary(data = predicciones3, fun.data = mean_sdl,
               mapping = aes(x = x, y = y), color = "blue", size = 0.5) +
  scale_x_continuous(breaks = c(4, 5.83, 8, 10, 12, 14, 16, 18, 20, 22, 24),
                     labels = c("4:00", "5:53", "8:00", "10:00",
                                "12:00", "14:00", "16:00", "18:00",
                                "20:00", "22:00", "00:00"),
                     name = "Hora") +
  scale_y_continuous(name = "Temperatura (°C)", breaks = c(42.5, 40, 37.5, 36, 35, 32.5, 30, 27.5, 25, 22)) +
  geom_point(aes(x = c(6.75, 8.25, 13.5), y = c(32.8, 30.5, 23.7)),
             shape = 13, size = 2, color = "red") +
  theme_bw()


# Guardar modelo

save(modelo1obs, modelo2obs, modelo3obs, file = "posteriors_stan.Rdata")
