globalVariables(c("ano", "dmu"))

.onAttach <- function(lib, pkg) {
    packageStartupMessage(paste0("\n* Please cite the 'productivity' package as:\n", 
        "  Dakpo K.H., Desjeux Y. and Latruffe L. (2017). productivity: Indices of Productivity and Profitability Using Data Envelopment Analysis (DEA). R package version 1.0.0.\n\n", 
        "See also: citation(\"productivity\")\n\n", 
        "* For any questions, suggestions, or comments on the 'productivity' package, please make use of Tracker facilities at:\n",
        "  https://r-forge.r-project.org/projects/productivity/\n"), 
        domain = NULL, appendLF = TRUE)
}
