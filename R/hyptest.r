# /usr/bin/r
#
# Created: 2016.04.06
# Copyright: Steven E. Pav, 2016
# Author: Steven E. Pav <steven@corecast.io>
# Comments: Steven E. Pav

#' @param alternative a character string specifying the alternative hypothesis,
#'       must be one of \code{"two.sided"} (default) \code{"greater"} or
#'       \code{"less"}.  You can specify just the initial letter. (NYI)
#' @rdname bws_test
#' @export
bws_test <- function(x,y,alternative=c("two.sided","greater","less")) 
{
	dname <- paste(deparse(substitute(x)),'vs.',deparse(substitute(y)))
	method <- "two-sample BWS test"

	bval <- bws_stat(x,y)
	names(bval) <- "b"
	pval <- bws_cdf(bval)

	retval <- list(statistic = bval, 
								 p.value = pval,
								 method = method, data.name = dname)
	class(retval) <- "htest"
	return(retval)
}

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
