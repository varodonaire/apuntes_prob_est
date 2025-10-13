# Script para ejecutar la aplicación del Teorema del Límite Central
# Script to run the Central Limit Theorem Shiny app

# Instalar paquetes necesarios si no están instalados
paquetes_necesarios <- c("shiny", "ggplot2", "shinythemes", "scales")

for (paquete in paquetes_necesarios) {
  if (!require(paquete, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Instalando %s...\n", paquete))
    install.packages(paquete)
  }
}

# Ejecutar la aplicación
cat("Iniciando la aplicación del Teorema del Límite Central...\n")
cat("La aplicación se abrirá en tu navegador.\n")
cat("Para detener la aplicación, presiona Ctrl+C en la consola.\n\n")

shiny::runApp("app_teorema_limite_central.R")
