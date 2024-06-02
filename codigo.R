library(ggplot2)
library(rstan)
set.seed(1997)
beta0 = rnorm(1000, mean = 2.69, sd = 0.017)
beta1 = rnorm(1000, mean = -0.2465, sd = 0.015)
varia = abs(rnorm(1000, mean = 0, sd = 0.00749))
x = -rgamma(1000, shape = 7, scale = 0.08) + 5.883

prior = data.frame(beta0, beta1, x)

prior$beta0[1] + prior$beta1[1] * (seq(-1, 24, length.out = 100) - x[1])
cbind(prior$beta0[1] + prior$beta1[1] * (seq(-1, 24, length.out = 100) - x[1]), 1)

grafico <- NA
for(i in  1:200) {
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
                     limits = c(22, 38)) + guide()


dgamma(seq(0,5, length.out = 100), shape = 2, scale = 0.5)
ggplot() + aes(y = dgamma(seq(0,5, length.out = 100), shape = 5, scale = 0.1), x = (-(seq(0,5, length.out = 100)) + 5.883)) +
  geom_line() + geom_vline(xintercept = 5.5)
53/60


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
  seed = 181222,
  iter = 10000,
  warmup = 1000
)

modelo1obs
posterior1 <- as.data.frame(extract(modelo1obs, c("b0", "b1", "x" ,"sigma")))
ggplot(posterior1) + aes(x = b0) + geom_histogram() + geom_line(aes(x = seq(2.64, 2.76, length.out = 27000), y = 125*dnorm(seq(2.64, 2.76, length.out = 27000), mean = 2.69, sd = 0.017)))
ggplot(posterior1) + aes(x = b1) + geom_histogram() + geom_line(aes(x = seq(-0.3, -0.175, length.out = 27000), y = 120*dnorm(seq(-0.3, -0.175, length.out = 27000), mean = -0.2465, sd = 0.015)))
ggplot(posterior1) + aes(x = sigma) + geom_histogram() + geom_line(aes(x = seq(0, 0.4, length.out = 27000), y = 120*dnorm(seq(0, 0.4, length.out = 27000), mean = -0.2465, sd = 0.015)))
ggplot(posterior1) + aes(x = x) + geom_histogram()


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
  iter = 10000
)

modelo2obs
posterior2 <- as.data.frame(extract(modelo2obs, c("b0", "b1", "x" ,"sigma")))
ggplot(posterior2) + aes(x = b0) + geom_histogram()
ggplot(posterior2) + aes(x = b1) + geom_histogram()
ggplot(posterior2) + aes(x = sigma) + geom_histogram()
ggplot(posterior2) + aes(x = x) + geom_histogram()

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
  iter = 10000
)

modelo3obs
posterior3 <- as.data.frame(extract(modelo3obs, c("b0", "b1", "x" ,"sigma")))
ggplot(posterior3) + aes(x = b0) + geom_histogram()
ggplot(posterior3) + aes(x = b1) + geom_histogram()
ggplot(posterior3) + aes(x = sigma) + geom_histogram()
ggplot(posterior3) + aes(x = x) + geom_histogram()
