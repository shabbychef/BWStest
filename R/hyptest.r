# /usr/bin/r
#
# Created: 2016.04.06
# Copyright: Steven E. Pav, 2016
# Author: Steven E. Pav <steven@corecast.io>
# Comments: Steven E. Pav

#' @rdname bws_test

#' @title Perform the Baumgartner-Weiss-Schindler hypothesis test.
#'
#' @description 
#'
#' Perform the Baumgartner-Weiss-Schindler hypothesis test.
#'
#' @param x a vector of the first sample.
#' @param y a vector of the first sample.
#' @param alternative a character string specifying the alternative hypothesis,
#'       must be one of \code{"two.sided"} (default) \code{"greater"} or
#'       \code{"less"}.  You can specify just the initial letter. (NYI)
#' @return Object of class \code{htest}, a list of the test statistic,
#' the p-value, and the \code{method} noted.
#' @keywords htest
#' @seealso \code{\link{bws_test}}, \code{\link{bws_stat}} 
#' @template etc
#' @template ref-bws
#' @examples 
#'
#' # under the null
#' set.seed(123)
#' x <- rnorm(100)
#' y <- rnorm(100)
#' hval <- bws_test(x,y)
#' 
#' # under the alternative
#' set.seed(123)
#' x <- rnorm(100)
#' y <- rnorm(100,mean=0.5)
#' hval <- bws_test(x,y)
#'
#' @export
bws_test <- function(x,y,alternative=c("two.sided","greater","less")) 
{
	dname <- paste(deparse(substitute(x)),'vs.',deparse(substitute(y)))
	method <- "two-sample BWS test"

	bval <- bws_stat(x,y)
	names(bval) <- "b"
	pval <- bws_cdf(bval,lower_tail=FALSE)

	retval <- list(statistic = bval, 
								 p.value = pval,
								 method = method, data.name = dname)
	class(retval) <- "htest"
	return(retval)
}

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
