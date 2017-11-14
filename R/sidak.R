
#' Perform the Sidak method.
#'
#' The Sidak method uses the minimum p-value but corrects it for the number of p-values that are aggregated.
#' @param pvalues A vector of p-values to be aggregated. NAs will be filtered.
#' @examples
#' sidak(c(.1, .2, .3))
#' @export
sidak <- function(pvalues)
{
    pvalues <- pvalues[!is.na(pvalues)]
    if(length(pvalues) == 0)
    {
        return(NA)
    }
    if(any(pvalues < 0) || any(pvalues > 1))
    {
	stop('p-values must be between 0 and 1')
    }
    if(length(pvalues) == 1)
    {
        return(pvalues)
    }
    if(any(pvalues < 10e-17))
    {
	warning('Extreme p-values around and below 10e-17 will produce aggregated p-value of 0. Replace extreme p-values with 10e-17 to obtain upper bound on the aggregated p-value')
    }
    n <- length(pvalues)
    m <- min(pvalues)
    1 - (1-m) ^ n
}

