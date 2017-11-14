
#' Fisher's Method
#'
#' Aggregate p-values with equal weights. Equivalent to the Lancaster method with all p-values weighted at 2.
#' @param pvalues A vector of p-values (i.e. between 0 and 1) to be aggregated with Fisher's method. NAs will be filtered out.
#' @examples
#' fisher(c(.1, .2, .3))
#' @export
fisher <- function(pvalues)
{
	pvalues <- pvalues[!is.na(pvalues)]
	if(length(pvalues)==0)
	{
		return(NA)
	}
	if(any(pvalues <0) || any(pvalues >1))
	{
		stop('p-values must be between 0 and 1')
	}
	if(length(pvalues)==1)
	{
		return(pvalues)
	}
	if(any(pvalues < 10e-320))
	{
		warning('Extremely low p-values at and around 10e-320 will produce an aggregated p-value of 0. Replace extreme p-values with 10e-320 to obtain an upper bound for the aggregated p-value.')
	}
	chisq = -2 * sum(log(pvalues))
	df <- 2* length(pvalues)
	pchisq(chisq, df, lower.tail = FALSE)
}

