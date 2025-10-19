library(tidyverse)
library(knitr)
library(kableExtra)

lanzamientos <- 10
resultados <- c("C", "S")
simulaciones <- 10000000
set.seed(1234)
lanzamoneda <- replicate(simulaciones, sample(resultados, lanzamientos, replace = TRUE))
lanzamoneda_df <- as.data.frame(t(lanzamoneda))
colnames(lanzamoneda_df) <- paste0("Lanzamiento_", 1:lanzamientos)
lanzamoneda_df <- lanzamoneda_df %>%
  mutate(Conteo_Caras = rowSums(lanzamoneda_df == "C"),
         Proporcion_Caras = Conteo_Caras / lanzamientos)
titulo <- paste("Distribución de la proporción de caras en", simulaciones, "simulaciones de", lanzamientos, "lanzamientos de moneda")
ggplot(lanzamoneda_df, aes(x = Proporcion_Caras)) +
    geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = titulo,
         x = "Proporción de Caras",
         y = "Frecuencia") +
    theme_minimal()

    ggsave("lanzamoneda_histograma4.png", width = 10, height = 6)
    

    resumen_tabla <- lanzamoneda_df %>%
        group_by(Conteo_Caras) %>%
        summarise(Frecuencia = n(),
                            Proporcion = n() / simulaciones) %>%
        arrange(Conteo_Caras)

    kable(resumen_tabla, 
                format = "latex",
                booktabs = TRUE,
                caption = "Distribución de Frecuencias de Caras en las Simulaciones",
                col.names = c("Número de Caras", "Frecuencia", "Proporción")) %>%
        kable_styling(latex_options = c("striped", "hold_position"))
