# Aplicación Shiny para el Teorema del Límite Central
# Shiny App for Central Limit Theorem

library(shiny)
library(ggplot2)
library(shinythemes)
library(scales)

# Definir la interfaz de usuario
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Título de la aplicación
  titlePanel(
    div(
      h2("Teorema del Límite Central", style = "font-weight: bold;"),
    )
  ),
  
  # Barra lateral con controles
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Parámetros de Simulación", style = "font-weight: bold;"),
      hr(),
      
      # Distribución original
      selectInput(
        "distribucion",
        "Distribución de la población:",
        choices = c(
          "Uniforme (0, 1)" = "uniforme",
          "Exponencial (λ=1)" = "exponencial",
          "Binomial (n=10, p=0.5)" = "binomial",
          "Poisson (λ=5)" = "poisson",
          "Normal (μ=0, σ=1)" = "normal"
        ),
        selected = "uniforme"
      ),
      
      # Tamaño de la muestra
      sliderInput(
        "n_muestra",
        "Tamaño de cada muestra (n):",
        min = 2,
        max = 100,
        value = 5,
        step = 1
      ),
      
      # Número de muestras
      numericInput(
        "n_muestras",
        "Número de muestras:",
        value = 1000,
        min = 100,
        max = 50000,
        step = 100
      ),
      
      hr(),
      
      # Botón para ejecutar simulación
      actionButton(
        "simular",
        "Ejecutar Simulación",
        class = "btn-primary btn-lg btn-block",
        style = "font-weight: bold;"
      ),
      
      br(),
      
      # Opciones de semilla aleatoria
      checkboxInput(
        "usar_semilla",
        "Usar semilla aleatoria (para reproducibilidad)",
        value = FALSE
      ),
      
      conditionalPanel(
        condition = "input.usar_semilla == true",
        numericInput(
          "semilla",
          "Semilla:",
          value = 123,
          min = 1
        )
      ),
      
      hr(),
      
      # Información adicional
      wellPanel(
        h5("Teorema del Límite Central", style = "font-weight: bold;"),
        p("Establece que la distribución de las medias muestrales se aproxima a una distribución normal, independientemente de la distribución de la población original, cuando el tamaño de la muestra es suficientemente grande.", 
          style = "font-size: 12px;")
      ),
      
      helpText(
        "Tip: Aumenta el tamaño de la muestra (n) para ver cómo mejora la aproximación a la normal."
      )
    ),
    
    # Panel principal con resultados
    mainPanel(
      width = 9,
      
      # Pestañas para diferentes visualizaciones
      tabsetPanel(
        type = "tabs",
        
        # Pestaña de gráficos
        tabPanel(
          "Gráficos",
          br(),
          fluidRow(
            column(
              6,
              h4("Distribución de la Población Original", style = "font-weight: bold;"),
              plotOutput("plot_poblacion", height = "350px")
            ),
            column(
              6,
              h4("Distribución de las Medias Muestrales", style = "font-weight: bold;"),
              plotOutput("plot_medias", height = "350px")
            )
          ),
        ),
        
        # Pestaña de estadísticas
        tabPanel(
          "Estadísticas",
          br(),
          fluidRow(
            column(6, wellPanel(
              h4("Población Original", style = "font-weight: bold;"),
              htmlOutput("estadisticas_poblacion")
            )),
            column(6, wellPanel(
              h4("Distribución de Medias Muestrales", style = "font-weight: bold;"),
              htmlOutput("estadisticas_medias")
            ))
          ),
          br(),
          fluidRow(
            column(12, wellPanel(
              h4("Teoría del Límite Central", style = "font-weight: bold;"),
              htmlOutput("teoria_comparacion")
            ))
          ),
          br(),
          fluidRow(
            column(12, wellPanel(
              h4("Test de Normalidad (Shapiro-Wilk)", style = "font-weight: bold;"),
              htmlOutput("test_normalidad")
            ))
          )
        ),
        
        # Pestaña de ayuda
        tabPanel(
          "Ayuda",
          br(),
          wellPanel(
            h3("Cómo usar esta aplicación", style = "font-weight: bold;"),
            hr(),
            h4("Parámetros:"),
            tags$ul(
              tags$li(strong("Distribución de la población:"), "Elige la distribución original de donde se extraerán las muestras."),
              tags$li(strong("Tamaño de cada muestra (n):"), "Número de observaciones en cada muestra (2-100)."),
              tags$li(strong("Número de muestras:"), "Cuántas muestras se tomarán para calcular sus medias (100-50000).")
            ),
            br(),
            h4("Interpretación:"),
            tags$ul(
              tags$li("El primer gráfico muestra la", strong("distribución original"), "de la población."),
              tags$li("El segundo gráfico muestra la", strong("distribución de las medias muestrales"), "calculadas. Observa cómo esta distribución tiende a ser normal (línea verde), incluso si la población original no lo es."),
              tags$li("La", strong("curva verde punteada"), "en el segundo gráfico representa la distribución normal teórica según el Teorema del Límite Central."),
              tags$li("El", strong("test de Shapiro-Wilk"), "evalúa formalmente si las medias siguen una distribución normal. Un p-valor > 0.05 sugiere normalidad.")
            ),
            br(),
            h4("Teorema del Límite Central:"),
            p("Si", tags$code("X₁, X₂, ..., Xₙ"), "son variables aleatorias independientes e idénticamente distribuidas con media μ y varianza σ², entonces:"),
            tags$ul(
              tags$li("La media muestral", tags$code("X̄ = (X₁ + X₂ + ... + Xₙ)/n"), "tiene:"),
              tags$ul(
                tags$li(strong("Media:"), "E[X̄] = μ"),
                tags$li(strong("Desviación estándar:"), "SD[X̄] = σ/√n (error estándar)")
              ),
              tags$li("Cuando n es grande, la distribución de X̄ se aproxima a", strong("Normal(μ, σ²/n)"), ", independientemente de la distribución original.")
            ),
            br(),
            h4("Observaciones importantes:"),
            tags$ul(
              tags$li("Con tamaños de muestra pequeños (n < 30), la aproximación puede no ser buena, especialmente para distribuciones muy asimétricas."),
              tags$li("Con tamaños de muestra grandes (n ≥ 30), la aproximación mejora significativamente."),
              tags$li("Si la población original ya es normal, las medias muestrales son normales para cualquier n.")
            )
          )
        )
      )
    )
  )
)

# Definir el servidor
server <- function(input, output, session) {
  
  # Mostrar el tamaño de muestra con descripción
  observe({
    n <- input$n_muestra
    descripcion <- if (n < 10) {
      "muy pequeño"
    } else if (n < 30) {
      "pequeño"
    } else if (n < 50) {
      "mediano"
    } else {
      "grande"
    }
  })
  
  # Variable reactiva para almacenar los resultados de la simulación
  simulacion <- eventReactive(input$simular, {
    # Configurar semilla si se solicita
    if (input$usar_semilla) {
      set.seed(input$semilla)
    }
    
    # Ejecutar simulación
    withProgress(message = 'Ejecutando simulación...', value = 0, {
      incProgress(0.3, detail = "Generando población...")
      
      # Generar muestra grande de la población para visualización
      n_poblacion <- 10000
      poblacion <- switch(
        input$distribucion,
        "uniforme" = runif(n_poblacion, 0, 1),
        "exponencial" = rexp(n_poblacion, rate = 1),
        "binomial" = rbinom(n_poblacion, size = 10, prob = 0.5),
        "poisson" = rpois(n_poblacion, lambda = 5),
        "normal" = rnorm(n_poblacion, mean = 0, sd = 1)
      )
      
      incProgress(0.3, detail = "Calculando medias muestrales...")
      
      # Calcular medias muestrales
      medias <- numeric(input$n_muestras)
      for (i in 1:input$n_muestras) {
        muestra <- switch(
          input$distribucion,
          "uniforme" = runif(input$n_muestra, 0, 1),
          "exponencial" = rexp(input$n_muestra, rate = 1),
          "binomial" = rbinom(input$n_muestra, size = 10, prob = 0.5),
          "poisson" = rpois(input$n_muestra, lambda = 5),
          "normal" = rnorm(input$n_muestra, mean = 0, sd = 1)
        )
        medias[i] <- mean(muestra)
      }
      
      incProgress(0.4, detail = "Finalizando...")
      
      # Parámetros teóricos de la población
      parametros_teoricos <- switch(
        input$distribucion,
        "uniforme" = list(media = 0.5, varianza = 1/12, nombre = "Uniforme(0, 1)"),
        "exponencial" = list(media = 1, varianza = 1, nombre = "Exponencial(λ=1)"),
        "binomial" = list(media = 5, varianza = 2.5, nombre = "Binomial(n=10, p=0.5)"),
        "poisson" = list(media = 5, varianza = 5, nombre = "Poisson(λ=5)"),
        "normal" = list(media = 0, varianza = 1, nombre = "Normal(μ=0, σ=1)")
      )
      
      list(
        poblacion = poblacion,
        medias = medias,
        parametros = parametros_teoricos,
        n_muestra = input$n_muestra,
        n_muestras = input$n_muestras
      )
    })
  })
  
  # Gráfico de la población original
  output$plot_poblacion <- renderPlot({
    req(simulacion())
    
    sim <- simulacion()
    df <- data.frame(valor = sim$poblacion)
    
    ggplot(df, aes(x = valor)) +
      geom_histogram(aes(y = after_stat(density)), 
                     bins = 50, 
                     fill = "lightcoral", 
                     color = "darkred",
                     alpha = 0.7) +
      geom_density(linewidth = 1.2, color = "darkred") +
      labs(title = sim$parametros$nombre,
           x = "Valor",
           y = "Densidad") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14))
  })
  
  # Gráfico de las medias muestrales
  output$plot_medias <- renderPlot({
    req(simulacion())
    
    sim <- simulacion()
    df <- data.frame(media = sim$medias)
    
    # Parámetros teóricos de la distribución de medias
    media_teorica <- sim$parametros$media
    sd_teorico <- sqrt(sim$parametros$varianza / sim$n_muestra)
    
    ggplot(df, aes(x = media)) +
      geom_histogram(aes(y = after_stat(density)), 
                     bins = 40, 
                     fill = "lightblue", 
                     color = "darkblue",
                     alpha = 0.7) +
      geom_density(linewidth = 1.2, color = "darkblue") +
      stat_function(fun = dnorm, 
                    args = list(mean = media_teorica, sd = sd_teorico),
                    linewidth = 1.2, 
                    color = "green",
                    linetype = "dashed") +
      labs(title = sprintf("Distribución de medias (n=%d)", sim$n_muestra),
           subtitle = "Línea verde: Normal teórica",
           x = "Media muestral",
           y = "Densidad") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 11, color = "darkgreen"))
  })
  
  # Estadísticas de la población
  output$estadisticas_poblacion <- renderUI({
    req(simulacion())
    sim <- simulacion()
    
    HTML(sprintf(
      "<p style='font-size: 15px;'><strong>Distribución:</strong> %s</p>
       <p style='font-size: 15px;'><strong>Media observada:</strong> %.4f</p>
       <p style='font-size: 15px;'><strong>Media teórica:</strong> %.4f</p>
       <p style='font-size: 15px;'><strong>Desviación estándar observada:</strong> %.4f</p>
       <p style='font-size: 15px;'><strong>Desviación estándar teórica:</strong> %.4f</p>",
      sim$parametros$nombre,
      mean(sim$poblacion),
      sim$parametros$media,
      sd(sim$poblacion),
      sqrt(sim$parametros$varianza)
    ))
  })
  
  # Estadísticas de las medias muestrales
  output$estadisticas_medias <- renderUI({
    req(simulacion())
    sim <- simulacion()
    
    media_medias <- mean(sim$medias)
    sd_medias <- sd(sim$medias)
    
    HTML(sprintf(
      "<p style='font-size: 15px;'><strong>Número de muestras:</strong> %d</p>
       <p style='font-size: 15px;'><strong>Tamaño de cada muestra:</strong> %d</p>
       <p style='font-size: 15px;'><strong>Media de las medias:</strong> %.4f</p>
       <p style='font-size: 15px;'><strong>Desviación estándar de las medias:</strong> %.4f</p>
       <p style='font-size: 15px;'><strong>Error estándar (observado):</strong> %.4f</p>",
      sim$n_muestras,
      sim$n_muestra,
      media_medias,
      sd_medias,
      sd_medias
    ))
  })
  
  # Comparación con la teoría
  output$teoria_comparacion <- renderUI({
    req(simulacion())
    sim <- simulacion()
    
    media_teorica <- sim$parametros$media
    error_estandar_teorico <- sqrt(sim$parametros$varianza / sim$n_muestra)
    
    media_observada <- mean(sim$medias)
    error_estandar_observado <- sd(sim$medias)
    
    diff_media <- abs(media_observada - media_teorica)
    diff_se <- abs(error_estandar_observado - error_estandar_teorico)
    
    color_media <- if(diff_media < 0.05) "green" else if(diff_media < 0.1) "orange" else "red"
    color_se <- if(diff_se < 0.05) "green" else if(diff_se < 0.1) "orange" else "red"
    
    HTML(sprintf(
      "<div style='font-size: 15px;'>
       <p><strong>Según el Teorema del Límite Central:</strong></p>
       <p>• Media de X̄ debe ser μ = %.4f</p>
       <p>• Error estándar debe ser σ/√n = %.4f/√%d = %.4f</p>
       <hr>
       <p><strong>Resultados observados:</strong></p>
       <p>• Media observada: %.4f <span style='color: %s;'>(diferencia: %.4f)</span></p>
       <p>• Error estándar observado: %.4f <span style='color: %s;'>(diferencia: %.4f)</span></p>
       <hr>
       <p style='font-style: italic; color: gray;'>
         💡 Con más muestras y/o mayor tamaño de muestra, la aproximación mejora.
       </p>
       </div>",
      media_teorica,
      sqrt(sim$parametros$varianza), sim$n_muestra, error_estandar_teorico,
      media_observada, color_media, diff_media,
      error_estandar_observado, color_se, diff_se
    ))
  })
  
  # Test de normalidad
  output$test_normalidad <- renderUI({
    req(simulacion())
    sim <- simulacion()
    
    # Realizar test de Shapiro-Wilk (máximo 5000 observaciones)
    n_test <- min(5000, length(sim$medias))
    medias_test <- sample(sim$medias, n_test)
    
    test <- shapiro.test(medias_test)
    
    interpretacion <- if(test$p.value > 0.05) {
      "<span style='color: green; font-weight: bold;'>✓ No se rechaza la hipótesis de normalidad</span>"
    } else {
      "<span style='color: red; font-weight: bold;'>✗ Se rechaza la hipótesis de normalidad</span>"
    }
    
    HTML(sprintf(
      "<div style='font-size: 15px;'>
       <p><strong>Test de Shapiro-Wilk</strong></p>
       <p>• Estadístico W: %.4f</p>
       <p>• P-valor: %.6f</p>
       <p>• Interpretación (α=0.05): %s</p>
       <hr>
       <p style='font-size: 13px; color: gray;'>
         <em>Nota: Se utiliza una muestra de hasta 5000 observaciones para el test.</em>
       </p>
       <p style='font-size: 13px;'>
         Un p-valor > 0.05 sugiere que los datos son consistentes con una distribución normal.
       </p>
       </div>",
      test$statistic,
      test$p.value,
      interpretacion
    ))
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
