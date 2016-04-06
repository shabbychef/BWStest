# /usr/bin/r
#
# Created: 2016.04.06
# Copyright: Steven E. Pav, 2016
# Author: Steven E. Pav <steven@corecast.io>
# Comments: Steven E. Pav

#' @rdname bws_test
#' @export
bws_test <- function(x,y) {
	bval <- bws_stat(x,y)
	names(bval) <- "b"
	pval <- bws_cdf(bval)

	retval <- list(statistic = bval, 
								 p.value = pval)
	class(retval) <- "htest"
	return(retval)
}

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
