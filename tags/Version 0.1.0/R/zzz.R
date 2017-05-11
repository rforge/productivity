globalVariables(c("ano", "COST", "dmu", "OME.IME", "OSE.ISE", "OSME.ISME", "OTE.ITE", "ROSE.RISE"))

.onAttach <- function(lib, pkg) {
    packageStartupMessage(paste0("\nPlease cite the 'productivity' package as:\n", "Dakpo K.H., Desjeux Y. and Latruffe L. (2016). productivity: Indices of Productivity and Profitability Using Data Envelopment Analysis (DEA). R package version 0.1.0. https://CRAN.R-Project.org/package=productivity.\n\n", 
        "If you have questions, suggestions, or comments regarding the 'productivity' package, please use a forum or 'tracker' at productivity's R-Forge site:\n", "https://r-forge.r-project.org/projects/productivity/"), 
        domain = NULL, appendLF = TRUE)
}
