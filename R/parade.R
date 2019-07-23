# parade - Pen's Income Parades in R
# Author: Marek Hlavac

parade <- function(height, line.fun = NULL, line.col = "red", line.lty = 1, line.lwd = 2, ...) {
    return(.parade.wrap(height = height,  
                        line.fun = line.fun, line.col = line.col, line.lty = line.lty, line.lwd = line.lwd,
                         ...))
}
