library(shiny)
library(ggplot2)
library(dplyr)

# Media poblacional fija para toda la actividad
MU <- 500

# ── Interfaz de usuario ────────────────────────────────────────────────────────

ui <- fluidPage(

  titlePanel("Varianza muestral"),

  sidebarLayout(

    sidebarPanel(

      sliderInput("n", "Tamaño de muestra (n)",
                  min = 2, max = 30, value = 5, step = 1),

      sliderInput("sigma2", "Varianza poblacional verdadera σ²",
                  min = 100, max = 5000, value = 2500, step = 100),

      sliderInput("n_sim", "Número de muestras simuladas",
                  min = 100, max = 5000, value = 1000, step = 100),

      actionButton("simular", "Simular",
                   class = "btn-primary btn-block",
                   style = "width:100%; margin-top:10px; font-size:1rem;"),

      br(), br(),

      # El checkbox solo aparece después de la primera simulación
      uiOutput("checkbox_ui")
    ),

    mainPanel(
      # Los tabs se construyen dinámicamente según el estado de la simulación
      uiOutput("main_tabs")
    )
  )
)

# ── Servidor ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Estado reactivo ──────────────────────────────────────────────────────────

  # Bandera: indica si ya se ejecutó al menos una simulación
  sim_done <- reactiveVal(FALSE)

  # Resultados de la última simulación
  sim_results <- reactiveVal(NULL)

  # ── Simulación ───────────────────────────────────────────────────────────────

  observeEvent(input$simular, {

    n      <- input$n
    sigma2 <- input$sigma2
    sigma  <- sqrt(sigma2)
    n_sim  <- input$n_sim

    # Semilla determinista: mismos parámetros → mismos resultados
    set.seed(n * 1000 + sigma2 + n_sim)

    # Generar n_sim muestras de tamaño n (cada fila = una muestra)
    muestras <- matrix(
      rnorm(n_sim * n, mean = MU, sd = sigma),
      nrow = n_sim, ncol = n
    )

    # Estimador sesgado (÷n): varianza de la muestra sin corrección de Bessel
    s2_sesgado <- apply(muestras, 1, function(x) mean((x - mean(x))^2))

    # Estimador insesgado (÷n−1): función var() de R
    s2_insesgado <- apply(muestras, 1, var)

    # Una muestra al azar para mostrar como ejemplo visual en Tab 1
    muestra_ejemplo <- muestras[sample(n_sim, 1), ]

    sim_results(list(
      s2_sesgado      = s2_sesgado,
      s2_insesgado    = s2_insesgado,
      muestra_ejemplo = muestra_ejemplo,
      n      = n,
      sigma2 = sigma2,
      sigma  = sigma,
      n_sim  = n_sim
    ))

    sim_done(TRUE)
  })

  # ── UI dinámica ──────────────────────────────────────────────────────────────

  # Checkbox de chi² solo visible después de simular
  output$checkbox_ui <- renderUI({
    req(sim_done())
    checkboxInput("show_chi2", "Mostrar distribución χ²", value = FALSE)
  })

  # Tabset progresivo: Tab1 siempre, Tab2 tras simular, Tab3 con el checkbox
  output$main_tabs <- renderUI({

    tab1 <- tabPanel(
      "Población",
      plotOutput("plot_poblacion", height = "460px")
    )

    tab2 <- tabPanel(
      "Estimadores de varianza",
      plotOutput("plot_estimadores", height = "420px"),
      br(),
      tableOutput("tabla_estimadores")
    )

    tab3 <- tabPanel(
      "Distribución chi-cuadrado",
      plotOutput("plot_chi2", height = "460px")
    )

    if (!sim_done()) {
      tabsetPanel(tab1)
    } else if (!isTRUE(input$show_chi2)) {
      tabsetPanel(tab1, tab2)
    } else {
      tabsetPanel(tab1, tab2, tab3)
    }
  })

  # ── Tab 1: Población ─────────────────────────────────────────────────────────

  output$plot_poblacion <- renderPlot({

    sigma2 <- input$sigma2
    sigma  <- sqrt(sigma2)
    mu     <- MU

    # Curva de densidad normal poblacional
    x_seq    <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 600)
    df_curva <- data.frame(x = x_seq, y = dnorm(x_seq, mean = mu, sd = sigma))

    # Región sombreada ±1σ
    df_sombra <- df_curva[df_curva$x >= mu - sigma & df_curva$x <= mu + sigma, ]

    y_max <- max(df_curva$y)

    # Posición vertical de las etiquetas (por debajo del pico)
    y_etiq <- y_max * 0.07

    p <- ggplot(df_curva, aes(x = x, y = y)) +
      geom_area(data = df_sombra, fill = "steelblue", alpha = 0.2) +
      geom_line(color = "darkblue", linewidth = 1.2) +
      # Líneas verticales en μ−σ, μ y μ+σ
      geom_vline(xintercept = c(mu - sigma, mu, mu + sigma),
                 linetype = "dotted", color = "gray50", linewidth = 0.8) +
      annotate("text",
               x     = c(mu - sigma, mu, mu + sigma),
               y     = y_etiq,
               label = c(paste0("μ − σ\n", round(mu - sigma, 1)),
                         paste0("μ\n", mu),
                         paste0("μ + σ\n", round(mu + sigma, 1))),
               size  = 3.5, color = "gray40", hjust = 0.5) +
      labs(
        title    = "Distribución poblacional",
        subtitle = "De aquí provienen las muestras de cada estudiante",
        x        = "Salario mensual (miles CLP)",
        y        = "Densidad",
        caption  = paste0("μ = ", mu, "   σ² = ", sigma2,
                          "   σ = ", round(sigma, 2))
      ) +
      theme_minimal(base_size = 14)

    # Después de simular: superponer los n puntos de una muestra típica como rug
    res <- sim_results()
    if (!is.null(res)) {
      df_rug <- data.frame(x = res$muestra_ejemplo)
      p <- p +
        geom_rug(data = df_rug, aes(x = x),
                 inherit.aes = FALSE,
                 color = "coral", linewidth = 1,
                 length = unit(0.05, "npc"), sides = "b") +
        labs(caption = paste0(
          "Los puntos representan una muestra típica de tamaño n = ", res$n,
          "\nμ = ", mu, "   σ² = ", sigma2,
          "   σ = ", round(sigma, 2)
        ))
    }

    p
  })

  # ── Tab 2: Estimadores de varianza ───────────────────────────────────────────

  output$plot_estimadores <- renderPlot({

    res <- sim_results()
    req(res)

    sigma2          <- res$sigma2
    n               <- res$n
    media_sesgado   <- mean(res$s2_sesgado)
    media_insesgado <- mean(res$s2_insesgado)
    bias_n          <- round(media_sesgado - sigma2, 1)
    bias_n1         <- round(media_insesgado - sigma2, 1)

    # Formato largo para geom_histogram con dos grupos
    df_long <- data.frame(
      valor = c(res$s2_sesgado, res$s2_insesgado),
      tipo  = factor(
        rep(c("S² (÷ n)", "S² (÷ n−1)"), each = res$n_sim),
        levels = c("S² (÷ n)", "S² (÷ n−1)")
      )
    )

    # Altura máxima para ubicar las anotaciones
    d1    <- density(res$s2_sesgado)
    d2    <- density(res$s2_insesgado)
    y_top <- max(d1$y, d2$y)

    ggplot(df_long, aes(x = valor, fill = tipo)) +
      geom_histogram(aes(y = after_stat(density)),
                     alpha = 0.4, position = "identity", bins = 60) +
      # Línea vertical del σ² verdadero
      geom_vline(xintercept = sigma2,
                 linetype = "dashed", color = "black", linewidth = 1) +
      annotate("text", x = sigma2, y = y_top * 0.95,
               label = paste0("σ² verdadero\n", sigma2),
               hjust = -0.08, size = 3.5, color = "black") +
      # Media del estimador sesgado
      geom_vline(xintercept = media_sesgado,
                 linetype = "dotted", color = "coral", linewidth = 1) +
      annotate("text", x = media_sesgado, y = y_top * 0.78,
               label = round(media_sesgado, 1),
               hjust = 1.15, size = 3.5, color = "coral") +
      # Media del estimador insesgado
      geom_vline(xintercept = media_insesgado,
                 linetype = "dotted", color = "steelblue", linewidth = 1) +
      annotate("text", x = media_insesgado, y = y_top * 0.62,
               label = round(media_insesgado, 1),
               hjust = -0.15, size = 3.5, color = "steelblue") +
      scale_fill_manual(
        values = c("S² (÷ n)" = "coral", "S² (÷ n−1)" = "steelblue")
      ) +
      labs(
        title    = "Distribución muestral de la varianza estimada",
        subtitle = paste0("n = ", n,
                          " | Sesgo promedio con ÷n: ", bias_n,
                          " | Sesgo promedio con ÷(n−1): ", bias_n1),
        x    = "Varianza estimada",
        y    = "Densidad",
        fill = "Estimador"
      ) +
      theme_minimal(base_size = 14)
  })

  # Tabla resumen de los tres estimadores
  output$tabla_estimadores <- renderTable({

    res <- sim_results()
    req(res)

    sigma2 <- res$sigma2

    data.frame(
      "Estimador"           = c("S² (÷ n)", "S² (÷ n−1)", "σ² (verdadero)"),
      "Media simulada"      = c(round(mean(res$s2_sesgado), 1),
                                 round(mean(res$s2_insesgado), 1),
                                 sigma2),
      "Sesgo respecto a σ²" = c(round(mean(res$s2_sesgado) - sigma2, 1),
                                 round(mean(res$s2_insesgado) - sigma2, 1),
                                 0),
      check.names = FALSE
    )

  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lrr")

  # ── Tab 3: Distribución chi-cuadrado ─────────────────────────────────────────

  output$plot_chi2 <- renderPlot({

    res <- sim_results()
    req(res)
    req(input$show_chi2)  # no renderizar si el checkbox está desmarcado

    n      <- res$n
    sigma2 <- res$sigma2

    # Estadístico T = (n−1)·S²_insesgada / σ² ~ chi²(n−1) bajo normalidad
    T_vals <- (n - 1) * res$s2_insesgado / sigma2

    # Curva teórica chi²(df = n−1)
    x_max <- max(qchisq(0.999, df = n - 1), quantile(T_vals, 0.999))
    x_seq <- seq(0.001, x_max * 1.08, length.out = 600)
    df_teo <- data.frame(x = x_seq, y = dchisq(x_seq, df = n - 1))

    # La media de una chi²(k) es exactamente k
    media_chi <- n - 1

    ggplot(data.frame(T = T_vals), aes(x = T)) +
      geom_histogram(aes(y = after_stat(density)),
                     fill = "steelblue", alpha = 0.5, bins = 60) +
      # Densidad teórica en rojo
      geom_line(data = df_teo, aes(x = x, y = y),
                color = "red", linewidth = 1, inherit.aes = FALSE) +
      # Línea en la media teórica
      geom_vline(xintercept = media_chi,
                 linetype = "dashed", color = "darkred", linewidth = 0.9) +
      annotate("text", x = media_chi, y = Inf,
               label = paste0("Media χ²(", n - 1, ") = ", media_chi),
               vjust = 1.6, hjust = -0.08, size = 3.5, color = "darkred") +
      labs(
        title    = "Distribución de (n−1)S² / σ²",
        subtitle = paste0("Distribución chi-cuadrado con ", n - 1,
                          " grados de libertad"),
        x = "(n−1)S² / σ²",
        y = "Densidad"
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui = ui, server = server)
