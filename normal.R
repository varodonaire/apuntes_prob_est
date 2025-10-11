# standard normal cumulative distribution function
# Usage: std_normal_cdf(x)
# x: numeric scalar or vector
std_normal_cdf <- function(x) {
    if (!is.numeric(x)) stop("x must be numeric")
    pnorm(x, mean = 0, sd = 1)
}

# Function to create a standard normal distribution table in LaTeX format
# Parameters:
#   z_min: minimum z-value (default: 0)
#   z_max: maximum z-value (default: 4)
#   step: step size for z-values (default: 0.1)
#   decimals: number of decimal places for rounding (default: 4)
#   write_file: logical, whether to write to file (default: FALSE)
#   filename: output filename if write_file is TRUE (default: "normal_table.tex")
create_normal_table_latex <- function(z_min = 0, z_max = 4, step = 0.1, 
                                     decimals = 4, write_file = FALSE, 
                                     filename = "normal_table.tex") {
    
    # Generate z-values for rows (first column)
    z_rows <- seq(z_min, z_max, by = step)
    
    # Generate hundredths values for columns (0.00 to 0.09)
    hundredths <- seq(0, 0.09, by = 0.01)
    
    # Start building LaTeX code
    latex_code <- c(
        "\\begin{table}[h]",
        "\\centering",
        "\\scalebox{0.8}{",
        "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|}",
        "\\hline"
    )
    
    # Create header row
    header <- "z"
    for (h in hundredths) {
        header <- paste0(header, " & ", sprintf("%.2f", h))
    }
    header <- paste0(header, " \\\\")
    latex_code <- c(latex_code, header, "\\hline")
    
    # Create data rows
    for (z in z_rows) {
        row_data <- sprintf("%.1f", z)
        
        for (h in hundredths) {
            z_value <- z - h
            prob <- pnorm(z_value, mean = 0, sd = 1)
            prob_rounded <- round(prob, decimals)
            row_data <- paste0(row_data, " & ", sprintf(paste0("%.", decimals, "f"), prob_rounded))
        }
        
        row_data <- paste0(row_data, " \\\\")
        latex_code <- c(latex_code, row_data)
    }
    
    # Close the table
    latex_code <- c(
        latex_code,
        "\\hline",
        "\\end{tabular}",
        "}",
        "\\caption{Tabla de la distribución normal estándar}",
        "\\label{tab:normal_standard}",
        "\\end{table}"
    )
    
    # Combine all lines
    full_latex <- paste(latex_code, collapse = "\n")
    
    # Write to file if requested
    if (write_file) {
        writeLines(latex_code, filename)
        cat("LaTeX table written to:", filename, "\n")
    }
    
    # Return the LaTeX code
    return(full_latex)
}

# Example usage:
# Generate the table and print LaTeX code
table_latex <- create_normal_table_latex()
cat(table_latex)
table2 <- create_normal_table_latex(z_min = -4, z_max = 0)
cat(table2)