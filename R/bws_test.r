# /usr/bin/r
#
# Created: 2016.04.06
# Copyright: Steven E. Pav, 2016
# Author: Steven E. Pav <steven@corecast.io>
# Comments: Steven E. Pav

# getAnywhere('print.htest') 

#' @title Perform the Baumgartner-Weiss-Schindler hypothesis test.
#'
#' @description 
#'
#' Perform the Baumgartner-Weiss-Schindler hypothesis test.
#'
#' @param x a vector of the first sample.
#' @param y a vector of the first sample.
#' @return Object of class \code{htest}, a list of the test statistic,
#' the p-value, and the \code{method} noted.
#' @keywords htest
#' @seealso \code{\link{bws_test}}, \code{\link{bws_stat}} 
#' @template etc
#' @template ref-bws
#' @note Eventually this will support the one-sided tests of
#' Neuhauser and Murakami.
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
#' @rdname bws_test
#' @export
bws_test <- function(x,y)
{
	dname <- paste(deparse(substitute(x)),'vs.',deparse(substitute(y)))
	method <- "two-sample BWS test"
	x <- x[!is.na(x)]
	y <- y[!is.na(y)]
	nx <- length(x)
	ny <- length(y)
	if (max(nx,ny) <= 8) {
		warning('A permutation test would likely make more sense.')
	} else if (min(nx,ny) <= 10) {
		warning('Small sample size may cause loss of nominal coverage.')
	}
	alternative <- 'two.sided'

	bval <- bws_stat(x,y)
	names(bval) <- "b"
	pval <- bws_cdf(bval,lower_tail=FALSE)

	retval <- list(statistic = bval, 
								 p.value = pval,
								 alternative = alternative,
								 method = method, 
								 data.name = dname)
	class(retval) <- "htest"
	return(retval)
}
#bws_test <- function(x,y,alternative=c("two.sided","greater","less")) 

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
