# parade - Pen's Income Parades in R
# Author: Marek Hlavac

.onAttach <- 
  function(libname, pkgname) {
    packageStartupMessage("\nPlease cite as: \n")
    packageStartupMessage(" Hlavac, Marek (2019). parade: Pen's Income Parades in R.")
    packageStartupMessage(" R package version 0.1. https://CRAN.R-project.org/package=parade \n")
  }

.parade.wrap <- function(height, line.fun, line.col, line.lty, line.lwd, ...) {
  
    # check arguments
    error.msg <- NULL
    
    if ( (!is.numeric(height)) || (!is.vector(height)) ) { error.msg <- c(error.msg,"Argument 'height' must be a numeric vector.\n")}
    if ( (!is.null(line.fun)) && (!is.function(line.fun)) ) { error.msg <- c(error.msg,"Argument 'line.fun' must be a function.\n")}
    
    if ( (!is.null(line.col)) && (!is.numeric(line.col)) && (!is.character(line.col)) ) { error.msg <- c(error.msg,"Argument 'line.col' must be numeric or a character string.\n")}
    if ( (is.numeric(line.col)) && (line.col < 0) ) { error.msg <- c(error.msg,"Argument 'line.col' cannot be a negative number.\n")}
    
    if ( (!is.null(line.lty)) && (!is.numeric(line.lty)) && (!is.character(line.lty)) ) { error.msg <- c(error.msg,"Argument 'line.lty' must be numeric or a character string.\n")}
    if ( (is.numeric(line.lty)) && (line.lty %% 1 != 0) ) { error.msg <- c(error.msg,"Argument 'line.lty' must be an integer.\n")}
    if ( (is.numeric(line.lty)) && (line.lty < 0) ) { error.msg <- c(error.msg,"Argument 'line.lty' cannot be negative.\n")}
    
    if ( (!is.null(line.lwd)) && (!is.numeric(line.lwd)) ) { error.msg <- c(error.msg,"Argument 'line.lwd' must be numeric.\n")}
    if ( (is.numeric(line.lwd)) && (line.lwd <= 0) ) { error.msg <- c(error.msg,"Argument 'line.lwd' must be a positive number.\n")}
    
    # stop execution if errors found
    if (!is.null(error.msg)) { 
      message(error.msg) 
      return(NULL)
    }
    
    #####
    
    y <- height[order(height)]
    l <- length(height)
    
    barplot(y, horiz = FALSE, ...)
    
    # is line.col, line.lty or line.wd are NULL, then do not draw the horizontal line
    if ((!is.null(line.col)) & (!is.null(line.lty)) & (!is.null(line.lwd))) {
      if (is.null(line.fun)) {    # by default, draw horizontal line at mean, ignoring missing values
        abline(h = mean(y, na.rm = TRUE), col = line.col, lwd = line.lwd, lty = line.lty)
      } 
      else {
        abline(h = line.fun(y), col = line.col, lwd = line.lwd, lty = line.lty)
      }
    }
}

