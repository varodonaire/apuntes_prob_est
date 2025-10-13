# Script para ejecutar la aplicación Shiny
# Script to run the Shiny app

# Instalar paquetes necesarios si no están instalados
paquetes_necesarios <- c("shiny", "ggplot2", "shinythemes", "DT", "scales")

for (paquete in paquetes_necesarios) {
  if (!require(paquete, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Instalando %s...\n", paquete))
    install.packages(paquete)
  }
}

# Ejecutar la aplicación
cat("Iniciando la aplicación Shiny...\n")
cat("La aplicación se abrirá en tu navegador.\n")
cat("Para detener la aplicación, presiona Ctrl+C en la consola.\n\n")

shiny::runApp("app_lanz_monedas.R")
