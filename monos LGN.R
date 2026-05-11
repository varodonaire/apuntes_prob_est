library(ggplot2)

set.seed(42)

resultados <- replicate(1000, sum(rbinom(10, 1, 0.5)))

ggplot(data.frame(caras = resultados), aes(x = caras)) +
  geom_bar(fill = "#4C72B0", color = "white") +
  scale_x_continuous(breaks = 0:10) +
  labs(
    title = "Distribución del número de caras en 10 lanzamientos",
    subtitle = "1000 repeticiones del experimento",
    x = "Número de caras",
    y = "Frecuencia"
  ) +
  theme_minimal()

library(ggplot2)

set.seed(42)

n_exp <- 1000
resultados <- cumsum(replicate(n_exp, sum(rbinom(10, 1, 0.5)))) / seq_len(n_exp)

ggplot(data.frame(experimento = 1:n_exp, promedio = resultados), 
       aes(x = experimento, y = promedio)) +
  geom_line(color = "#4C72B0") +
  geom_hline(yintercept = 5, color = "tomato", linetype = "dotted") +
  labs(
    title = "Convergencia del promedio de caras",
    subtitle = "La línea roja es el valor esperado (5 caras)",
    x = "Número de experimentos",
    y = "Promedio acumulado de caras"
  ) +
  theme_minimal()
