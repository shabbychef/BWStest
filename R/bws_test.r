# Copyright 2016-2016 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav

# This file is part of BWStest.
#
# BWStest is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# BWStest is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with BWStest.  If not, see <http://www.gnu.org/licenses/>.

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
