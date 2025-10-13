# Aplicación Shiny para Simulador de Lanzamiento de Monedas
# Shiny App for Coin Toss Simulator

library(shiny)
library(ggplot2)
library(shinythemes)
library(scales)

# Cargar las funciones de simulación del archivo lanz_monedas.R
source("lanz_monedas.R")

# Definir la interfaz de usuario
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Título de la aplicación
  titlePanel(
    div(
      h2("🪙 Simulador de Lanzamiento de Monedas", style = "font-weight: bold;"),
    )
  ),
  
  # Barra lateral con controles
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Parámetros de Simulación", style = "font-weight: bold;"),
      hr(),
      
      # Número de monedas
      sliderInput(
        "n_monedas",
        "Número de monedas por lanzamiento:",
        min = 1,
        max = 100,
        value = 20,
        step = 1
      ),
      
      # Probabilidad de cara
      sliderInput(
        "prob_cara",
        "Probabilidad de cara:",
        min = 0,
        max = 1,
        value = 0.5,
        step = 0.01
      ),
      
      # Mostrar porcentaje de la probabilidad
      textOutput("prob_display"),
      br(),
      
      # Número de repeticiones
      numericInput(
        "n_repeticiones",
        "Número de repeticiones:",
        value = 100,
        min = 1,
        max = 100000,
        step = 1
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
      helpText(
        "💡 Tip: Usa más repeticiones para ver mejor la convergencia a la distribución teórica."
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
          "📊 Gráficos",
          br(),
          fluidRow(
            column(
              12,
              plotOutput("plot_histograma", height = "400px")
            )
          ),
          br(),
          fluidRow(
            column(
              3,
              selectInput(
                "tipo_grafico_acum",
                "Tipo de gráfico:",
                choices = c(
                  "Diferencia con valor esperado" = "diferencia",
                  "Proporción acumulada" = "proporcion"
                ),
                selected = "proporcion"
              )
            ),
            column(
              9,
              plotOutput("plot_acumulado_seleccionado", height = "400px")
            )
          )
        ),
        
        # Pestaña de estadísticas
        tabPanel(
          "📈 Estadísticas",
          br(),
          fluidRow(
            column(6, wellPanel(
              h4("Parámetros Utilizados", style = "font-weight: bold;"),
              htmlOutput("parametros_info")
            )),
            column(6, wellPanel(
              h4("Valores Teóricos", style = "font-weight: bold;"),
              htmlOutput("teoricos_info")
            ))
          ),
          br(),
          fluidRow(
            column(12, wellPanel(
              h4("Estadísticas Observadas", style = "font-weight: bold;"),
              htmlOutput("estadisticas_info")
            ))
          ),
          br(),
          fluidRow(
            column(12, wellPanel(
              h4("Comparación: Observado vs. Teórico", style = "font-weight: bold;"),
              htmlOutput("comparacion_info")
            ))
          )
        ),
        
        # Pestaña de datos
        tabPanel(
          "📋 Datos",
          br(),
          wellPanel(
            h4("Resultados por Repetición", style = "font-weight: bold;"),
            p("Primeras 100 repeticiones:"),
            DT::dataTableOutput("tabla_resultados")
          )
        ),
        
        # Pestaña de ayuda
        tabPanel(
          "❓ Ayuda",
          br(),
          wellPanel(
            h3("Cómo usar esta aplicación", style = "font-weight: bold;"),
            hr(),
            h4("Parámetros:"),
            tags$ul(
              tags$li(strong("Número de monedas por lanzamiento:"), "Cuántas monedas se lanzan en cada repetición (1-100)."),
              tags$li(strong("Probabilidad de cara:"), "La probabilidad de que salga cara en cada lanzamiento (0-1). 0.5 es una moneda justa."),
              tags$li(strong("Número de repeticiones:"), "Cuántas veces se repite el experimento completo (1-10000).")
            ),
            br(),
            h4("Interpretación:"),
            tags$ul(
              tags$li("El", strong("histograma"), "muestra la distribución de frecuencias del número de caras obtenidas."),
              tags$li("El", strong("gráfico de diferencia con valor esperado"), "muestra cuántas caras más (o menos) se obtuvieron respecto al valor esperado teórico en cada lanzamiento. La línea verde en cero representa el caso ideal donde observado = esperado."),
              tags$li("El", strong("gráfico de proporción acumulada"), "muestra la proporción de caras dividida por el total de monedas lanzadas hasta ese momento. Este gráfico ilustra la", strong("Ley de los Grandes Números"), "- con más lanzamientos, la proporción converge a la probabilidad teórica."),
              tags$li("Las líneas verticales en el histograma muestran la", strong("media observada"), "(roja) y el", strong("valor esperado teórico"), "(verde)."),
              tags$li("Con muchas repeticiones, la distribución observada debería aproximarse a la distribución binomial teórica.")
            ),
            br(),
            h4("Distribución Binomial:"),
            p("El número de caras sigue una distribución binomial con parámetros n (número de monedas) y p (probabilidad de cara)."),
            tags$ul(
              tags$li(strong("Media teórica:"), "E[X] = n × p"),
              tags$li(strong("Varianza teórica:"), "Var[X] = n × p × (1-p)"),
              tags$li(strong("Desviación estándar teórica:"), "SD[X] = √(n × p × (1-p))")
            )
          )
        )
      )
    )
  )
)

# Definir el servidor
server <- function(input, output, session) {
  
  # Mostrar la probabilidad como porcentaje
  output$prob_display <- renderText({
    sprintf("Probabilidad: %.1f%% cara, %.1f%% cruz", 
            input$prob_cara * 100, 
            (1 - input$prob_cara) * 100)
  })
  
  # Variable reactiva para almacenar los resultados
  simulacion <- eventReactive(input$simular, {
    # Configurar semilla si se solicita
    if (input$usar_semilla) {
      set.seed(input$semilla)
    }
    
    # Ejecutar simulación
    withProgress(message = 'Ejecutando simulación...', value = 0, {
      incProgress(0.5)
      resultado <- simular_lanzamientos(
        n_monedas = input$n_monedas,
        prob_cara = input$prob_cara,
        n_repeticiones = input$n_repeticiones
      )
      incProgress(0.5)
      resultado
    })
  })
  
  # Gráfico de histograma
  output$plot_histograma <- renderPlot({
    req(simulacion())
    
    sim <- simulacion()
    df <- data.frame(caras = sim$caras_por_repeticion)
    
    media_observada <- sim$estadisticas$media_caras
    valor_esperado <- sim$parametros$n_monedas * sim$parametros$prob_cara
    
    ggplot(df, aes(x = caras)) +
      geom_histogram(binwidth = 1, 
                     fill = "lightblue", 
                     color = "darkblue",
                     alpha = 0.7) +
      geom_vline(aes(xintercept = media_observada, color = "Media observada"),
                 linetype = "dashed", linewidth = 1.2) +
      geom_vline(aes(xintercept = valor_esperado, color = "Valor esperado teórico"),
                 linetype = "dashed", linewidth = 1.2) +
      scale_color_manual(name = "",
                         values = c("Media observada" = "red",
                                    "Valor esperado teórico" = "green")) +
      labs(title = sprintf("Distribución de caras en %d repeticiones", 
                           sim$parametros$n_repeticiones),
           subtitle = sprintf("%d monedas, p(cara) = %.2f", 
                              sim$parametros$n_monedas, 
                              sim$parametros$prob_cara),
           x = "Número de caras",
           y = "Frecuencia") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(size = 12),
            legend.text = element_text(size = 12))
  })
  
  # Gráfico acumulado seleccionado (diferencia o proporción)
  output$plot_acumulado_seleccionado <- renderPlot({
    req(simulacion())
    
    if (input$tipo_grafico_acum == "diferencia") {
      # Gráfico de diferencia con valor esperado
      plot_diferencia_esperado()
    } else {
      # Gráfico de proporción acumulada
      plot_proporcion_acumulada()
    }
  })
  
  # Función para gráfico de diferencia con valor esperado
  plot_diferencia_esperado <- function() {
    sim <- simulacion()
    n_rep <- sim$parametros$n_repeticiones
    n_monedas <- sim$parametros$n_monedas
    prob_cara <- sim$parametros$prob_cara
    
    # Calcular el número acumulado de caras observadas
    caras_acumuladas <- cumsum(sim$caras_por_repeticion)
    
    # Calcular el número esperado de caras acumuladas en cada lanzamiento
    caras_esperadas <- (1:n_rep) * n_monedas * prob_cara
    
    # Calcular la diferencia
    diferencia <- caras_acumuladas - caras_esperadas
    
    # Crear data frame para el gráfico
    df_diff <- data.frame(
      lanzamiento = 1:n_rep,
      diferencia = diferencia
    )
    
    # Crear el gráfico
    ggplot(df_diff, aes(x = lanzamiento, y = diferencia)) +
      geom_line(color = "darkblue", linewidth = 1) +
      geom_hline(yintercept = 0, 
                 aes(color = "Sin diferencia (valor esperado)"),
                 linetype = "dashed", linewidth = 1.2) +
      scale_color_manual(name = "",
                         values = c("Sin diferencia (valor esperado)" = "green")) +
      labs(title = "Diferencia entre caras observadas y esperadas (acumulado)",
           subtitle = sprintf("%d monedas por lanzamiento, p(cara) = %.2f", 
                              n_monedas, 
                              prob_cara),
           x = "Número de lanzamiento",
           y = "Diferencia (observadas - esperadas)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(size = 12),
            legend.text = element_text(size = 12))
  }
  
  # Función para gráfico de proporción acumulada
  plot_proporcion_acumulada <- function() {
    sim <- simulacion()
    n_rep <- sim$parametros$n_repeticiones
    n_monedas <- sim$parametros$n_monedas
    
    # Calcular el número acumulado de caras
    caras_acumuladas <- cumsum(sim$caras_por_repeticion)
    
    # Calcular monedas acumuladas totales
    monedas_acumuladas <- (1:n_rep) * n_monedas
    
    # Calcular proporción acumulada
    proporcion_acumulada <- caras_acumuladas / monedas_acumuladas
    
    # Crear data frame para el gráfico
    df_prop <- data.frame(
      lanzamiento = 1:n_rep,
      proporcion = proporcion_acumulada
    )
    
    # Proporción esperada
    proporcion_esperada <- sim$parametros$prob_cara
    
    # Crear el gráfico
    ggplot(df_prop, aes(x = lanzamiento, y = proporcion)) +
      geom_line(color = "darkblue", linewidth = 1) +
      geom_hline(aes(yintercept = proporcion_esperada, color = "Proporción esperada"),
                 linetype = "dashed", linewidth = 1.2) +
      scale_color_manual(name = "",
                         values = c("Proporción esperada" = "green")) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
      labs(title = "Proporción acumulada de caras por lanzamiento",
           subtitle = sprintf("Convergencia a la Ley de los Grandes Números (p = %.2f)", 
                              sim$parametros$prob_cara),
           x = "Número de lanzamiento",
           y = "Proporción de caras") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(size = 12),
            legend.text = element_text(size = 12))
  }
  
  # Información de parámetros
  output$parametros_info <- renderUI({
    req(simulacion())
    sim <- simulacion()
    
    HTML(sprintf(
      "<p style='font-size: 15px;'><strong>Monedas por lanzamiento:</strong> %d</p>
       <p style='font-size: 15px;'><strong>Probabilidad de cara:</strong> %.3f (%.1f%%)</p>
       <p style='font-size: 15px;'><strong>Número de repeticiones:</strong> %d</p>",
      sim$parametros$n_monedas,
      sim$parametros$prob_cara,
      sim$parametros$prob_cara * 100,
      sim$parametros$n_repeticiones
    ))
  })
  
  # Información teórica
  output$teoricos_info <- renderUI({
    req(simulacion())
    sim <- simulacion()
    
    n <- sim$parametros$n_monedas
    p <- sim$parametros$prob_cara
    
    media_teorica <- n * p
    varianza_teorica <- n * p * (1 - p)
    sd_teorica <- sqrt(varianza_teorica)
    
    HTML(sprintf(
      "<p style='font-size: 15px;'><strong>Media teórica:</strong> %.3f</p>
       <p style='font-size: 15px;'><strong>Varianza teórica:</strong> %.3f</p>
       <p style='font-size: 15px;'><strong>Desviación estándar teórica:</strong> %.3f</p>",
      media_teorica,
      varianza_teorica,
      sd_teorica
    ))
  })
  
  # Estadísticas observadas
  output$estadisticas_info <- renderUI({
    req(simulacion())
    sim <- simulacion()
    
    HTML(sprintf(
      "<div style='font-size: 15px;'>
       <div style='display: flex; justify-content: space-between;'>
         <div style='flex: 1; padding: 10px;'>
           <p><strong>Media:</strong> %.3f</p>
           <p><strong>Mediana:</strong> %.1f</p>
         </div>
         <div style='flex: 1; padding: 10px;'>
           <p><strong>Desviación estándar:</strong> %.3f</p>
           <p><strong>Varianza:</strong> %.3f</p>
         </div>
         <div style='flex: 1; padding: 10px;'>
           <p><strong>Mínimo:</strong> %d</p>
           <p><strong>Máximo:</strong> %d</p>
         </div>
       </div>
       </div>",
      sim$estadisticas$media_caras,
      sim$estadisticas$mediana_caras,
      sim$estadisticas$sd_caras,
      sim$estadisticas$sd_caras^2,
      sim$estadisticas$min_caras,
      sim$estadisticas$max_caras
    ))
  })
  
  # Comparación observado vs teórico
  output$comparacion_info <- renderUI({
    req(simulacion())
    sim <- simulacion()
    
    n <- sim$parametros$n_monedas
    p <- sim$parametros$prob_cara
    
    media_teorica <- n * p
    sd_teorica <- sqrt(n * p * (1 - p))
    
    diff_media <- sim$estadisticas$media_caras - media_teorica
    diff_sd <- sim$estadisticas$sd_caras - sd_teorica
    
    proporcion_obs <- sim$estadisticas$media_caras / n
    proporcion_teo <- p
    diff_proporcion <- proporcion_obs - proporcion_teo
    
    color_media <- if(abs(diff_media) < 0.5) "green" else if(abs(diff_media) < 1) "orange" else "red"
    color_sd <- if(abs(diff_sd) < 0.5) "green" else if(abs(diff_sd) < 1) "orange" else "red"
    
    HTML(sprintf(
      "<div style='font-size: 15px;'>
       <p><strong>Diferencia en media:</strong> <span style='color: %s;'>%.3f</span> (%.2f%% de la media teórica)</p>
       <p><strong>Diferencia en desviación estándar:</strong> <span style='color: %s;'>%.3f</span> (%.2f%% de la SD teórica)</p>
       <p><strong>Proporción observada de caras:</strong> %.4f</p>
       <p><strong>Proporción teórica de caras:</strong> %.4f</p>
       <p><strong>Diferencia en proporción:</strong> %.4f</p>
       <hr>
       <p style='font-style: italic; color: gray;'>
         💡 Con más repeticiones, las diferencias deberían disminuir debido a la Ley de los Grandes Números.
       </p>
       </div>",
      color_media,
      diff_media,
      abs(diff_media / media_teorica * 100),
      color_sd,
      diff_sd,
      abs(diff_sd / sd_teorica * 100),
      proporcion_obs,
      proporcion_teo,
      diff_proporcion
    ))
  })
  
  # Tabla de resultados
  output$tabla_resultados <- DT::renderDataTable({
    req(simulacion())
    sim <- simulacion()
    
    n_mostrar <- min(100, sim$parametros$n_repeticiones)
    
    df_tabla <- data.frame(
      Repeticion = 1:n_mostrar,
      Caras = sim$caras_por_repeticion[1:n_mostrar],
      Cruces = sim$cruces_por_repeticion[1:n_mostrar],
      Proporcion_Caras = sim$caras_por_repeticion[1:n_mostrar] / sim$parametros$n_monedas
    )
    
    DT::datatable(
      df_tabla,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = "Proporcion_Caras", digits = 3)
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
