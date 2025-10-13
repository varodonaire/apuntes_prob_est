# Simulador de lanzamiento de monedas
# Coin toss simulator

# Función principal para simular lanzamientos de monedas
# Main function to simulate coin tosses
simular_lanzamientos <- function(n_monedas = 10, 
                                  prob_cara = 0.5, 
                                  n_repeticiones = 1) {
  #' Simula lanzamientos de monedas
  #' 
  #' @param n_monedas Número de monedas a lanzar en cada repetición
  #' @param prob_cara Probabilidad de obtener cara (0 a 1)
  #' @param n_repeticiones Número de veces que se repite el experimento
  #' @return Una lista con los resultados detallados y resumen estadístico
  
  # Validar parámetros
  if (prob_cara < 0 || prob_cara > 1) {
    stop("La probabilidad debe estar entre 0 y 1")
  }
  if (n_monedas < 1 || n_repeticiones < 1) {
    stop("El número de monedas y repeticiones debe ser al menos 1")
  }
  
  # Realizar las simulaciones
  resultados <- matrix(nrow = n_repeticiones, ncol = n_monedas)
  
  for (i in 1:n_repeticiones) {
    # 1 = cara, 0 = cruz
    resultados[i, ] <- rbinom(n_monedas, 1, prob_cara)
  }
  
  # Calcular estadísticas
  caras_por_repeticion <- rowSums(resultados)
  cruces_por_repeticion <- n_monedas - caras_por_repeticion
  
  # Crear resumen
  resumen <- list(
    parametros = list(
      n_monedas = n_monedas,
      prob_cara = prob_cara,
      n_repeticiones = n_repeticiones
    ),
    resultados_completos = resultados,
    caras_por_repeticion = caras_por_repeticion,
    cruces_por_repeticion = cruces_por_repeticion,
    estadisticas = list(
      media_caras = mean(caras_por_repeticion),
      mediana_caras = median(caras_por_repeticion),
      sd_caras = sd(caras_por_repeticion),
      min_caras = min(caras_por_repeticion),
      max_caras = max(caras_por_repeticion),
      proporcion_media_caras = mean(caras_por_repeticion) / n_monedas
    )
  )
  
  class(resumen) <- "simulacion_monedas"
  return(resumen)
}

# Método para imprimir resultados
print.simulacion_monedas <- function(x, ...) {
  cat("=== SIMULACIÓN DE LANZAMIENTO DE MONEDAS ===\n\n")
  cat("Parámetros:\n")
  cat(sprintf("  - Monedas por lanzamiento: %d\n", x$parametros$n_monedas))
  cat(sprintf("  - Probabilidad de cara: %.2f\n", x$parametros$prob_cara))
  cat(sprintf("  - Número de repeticiones: %d\n\n", x$parametros$n_repeticiones))
  
  cat("Resultados por repetición (número de caras):\n")
  if (x$parametros$n_repeticiones <= 20) {
    for (i in 1:x$parametros$n_repeticiones) {
      cat(sprintf("  Repetición %2d: %d caras, %d cruces\n", 
                  i, x$caras_por_repeticion[i], x$cruces_por_repeticion[i]))
    }
  } else {
    cat(sprintf("  Primeras 10 repeticiones:\n"))
    for (i in 1:10) {
      cat(sprintf("    Repetición %2d: %d caras, %d cruces\n", 
                  i, x$caras_por_repeticion[i], x$cruces_por_repeticion[i]))
    }
    cat("  ...\n")
  }
  
  cat("\nEstadísticas:\n")
  cat(sprintf("  - Media de caras: %.2f\n", x$estadisticas$media_caras))
  cat(sprintf("  - Mediana de caras: %.1f\n", x$estadisticas$mediana_caras))
  cat(sprintf("  - Desviación estándar: %.2f\n", x$estadisticas$sd_caras))
  cat(sprintf("  - Mínimo de caras: %d\n", x$estadisticas$min_caras))
  cat(sprintf("  - Máximo de caras: %d\n", x$estadisticas$max_caras))
  cat(sprintf("  - Proporción media de caras: %.3f\n", x$estadisticas$proporcion_media_caras))
}

# Función para graficar resultados
graficar_resultados <- function(simulacion, tipo = "histograma") {
  #' Grafica los resultados de una simulación
  #' 
  #' @param simulacion Objeto de clase simulacion_monedas
  #' @param tipo Tipo de gráfico: "histograma", "densidad", o "ambos"
  
  # Cargar ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("El paquete ggplot2 es necesario. Instálalo con: install.packages('ggplot2')")
  }
  library(ggplot2)
  
  if (!inherits(simulacion, "simulacion_monedas")) {
    stop("El objeto debe ser de clase simulacion_monedas")
  }
  
  caras <- simulacion$caras_por_repeticion
  n_monedas <- simulacion$parametros$n_monedas
  prob_cara <- simulacion$parametros$prob_cara
  n_repeticiones <- simulacion$parametros$n_repeticiones
  
  # Crear data frame para ggplot
  df <- data.frame(caras = caras)
  
  # Calcular estadísticas
  media_observada <- mean(caras)
  valor_esperado <- n_monedas * prob_cara
  
  if (tipo == "histograma" || tipo == "ambos") {
    p <- ggplot(df, aes(x = caras)) +
      geom_histogram(binwidth = 1, 
                     fill = "lightblue", 
                     color = "darkblue",
                     alpha = 0.7) +
      geom_vline(aes(xintercept = media_observada, color = "Media observada"),
                 linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = valor_esperado, color = "Valor esperado teórico"),
                 linetype = "dashed", linewidth = 1) +
      scale_color_manual(name = "Referencia",
                        values = c("Media observada" = "red",
                                  "Valor esperado teórico" = "green")) +
      labs(title = sprintf("Distribución de caras en %d repeticiones", n_repeticiones),
           subtitle = sprintf("%d monedas, p(cara) = %.2f", n_monedas, prob_cara),
           x = "Número de caras",
           y = "Frecuencia") +
      theme_minimal() +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 11))
    
    print(p)
  }
  
  if (tipo == "densidad") {
    p <- ggplot(df, aes(x = caras)) +
      geom_density(fill = "lightblue", 
                   color = "darkblue",
                   alpha = 0.5,
                   linewidth = 1) +
      geom_vline(aes(xintercept = media_observada, color = "Media observada"),
                 linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = valor_esperado, color = "Valor esperado teórico"),
                 linetype = "dashed", linewidth = 1) +
      scale_color_manual(name = "Referencia",
                        values = c("Media observada" = "red",
                                  "Valor esperado teórico" = "green")) +
      labs(title = sprintf("Densidad de caras en %d repeticiones", n_repeticiones),
           subtitle = sprintf("%d monedas, p(cara) = %.2f", n_monedas, prob_cara),
           x = "Número de caras",
           y = "Densidad") +
      theme_minimal() +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 11))
    
    print(p)
  }
  
  if (tipo == "ambos") {
    p2 <- ggplot(df, aes(x = caras)) +
      geom_density(fill = "lightblue", 
                   color = "darkblue",
                   alpha = 0.5,
                   linewidth = 1) +
      geom_vline(aes(xintercept = media_observada, color = "Media observada"),
                 linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = valor_esperado, color = "Valor esperado teórico"),
                 linetype = "dashed", linewidth = 1) +
      scale_color_manual(name = "Referencia",
                        values = c("Media observada" = "red",
                                  "Valor esperado teórico" = "green")) +
      labs(title = sprintf("Densidad de caras en %d repeticiones", n_repeticiones),
           subtitle = sprintf("%d monedas, p(cara) = %.2f", n_monedas, prob_cara),
           x = "Número de caras",
           y = "Densidad") +
      theme_minimal() +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 11))
    
    print(p2)
  }
}

# ============================================
# EJEMPLOS DE USO
# ============================================

cat("Ejemplos de uso:\n\n")

# Ejemplo 1: Lanzamiento básico
cat("# Ejemplo 1: 10 monedas justas, 1 repetición\n")
set.seed(123)
sim1 <- simular_lanzamientos(n_monedas = 10, prob_cara = 0.5, n_repeticiones = 1)
print(sim1)
cat("\n")

# Ejemplo 2: Múltiples repeticiones
cat("# Ejemplo 2: 20 monedas justas, 10 repeticiones\n")
set.seed(456)
sim2 <- simular_lanzamientos(n_monedas = 20, prob_cara = 0.5, n_repeticiones = 10)
print(sim2)
cat("\n")

# Ejemplo 3: Moneda sesgada
cat("# Ejemplo 3: 15 monedas sesgadas (70% cara), 100 repeticiones\n")
set.seed(789)
sim3 <- simular_lanzamientos(n_monedas = 15, prob_cara = 0.7, n_repeticiones = 5)
print(sim3)
cat("\n")

# Ejemplo 4: Gran número de repeticiones
cat("# Ejemplo 4: 30 monedas justas, 1000 repeticiones (para análisis estadístico)\n")
set.seed(101112)
sim4 <- simular_lanzamientos(n_monedas = 30, prob_cara = 0.5, n_repeticiones = 100000)
print(sim4)

# Descomentar para ver gráficos
graficar_resultados(sim3, tipo = "histograma")
# graficar_resultados(sim4, tipo = "histograma")
