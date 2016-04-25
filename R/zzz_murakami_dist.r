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

.murakami_memo_stats <- memoise::memoise(murakami_stat_perms)

#' @title Murakami test statistic distribution.
#'
#' @description 
#'
#' Estimates the CDF of the Murakami test statistics via permutations.
#'
#' @details
#'
#' Given the Murakami test statistic \eqn{B_j} for \eqn{0 \le j \le 5}{0 <= j <= 5},
#' computes the CDF under the null that the two samples come from the same
#' distribution. The CDF is computed by permutation test and memoization.
#'
#' @param B the Murakami test statistic or a vector of the same.
#' @param n1 number of elements in the first sample.
#' @param n2 number of elements in the second sample.
#' @param flavor the 'flavor' of the test statistic. See
#' \code{\link{murakami_stat}}.
#' @param lower_tail boolean, when \code{TRUE} returns the CDF, \eqn{\Psi}{Psi}, otherwise 
#' compute the upper tail, \eqn{1-\Psi}{1 - Psi}, which is potentially more useful for hypothesis tests.
#' @return a vector of the same size as \code{B} of the CDF under the null.
#' @seealso \code{\link{murakami_stat}}.
#' @template etc
#' @template ref-bws
#' @template ref-modtests
#' @note the CDF is approximately computed by evaluating the permutations up to
#' some reasonably small sample size (currently the cutoff is 9). When larger
#' sample sizes are used, the distribution of the test statistic may not
#' converge. This is apparently seen in flavors 3 through 5.
#' @examples 
#'
#' # basic usage:
#' xv <- seq(0,4,length.out=101)
#' yv <- murakami_cdf(xv, n1=8, n2=6, flavor=1L)
#' plot(xv,yv)
#' zv <- bws_cdf(xv)
#' lines(xv,zv,col='red')
#'
#' # check under the null:
#' \dontrun{
#' flavor <- 1L
#' n1 <- 8
#' n2 <- 8
#' set.seed(1234)
#' Bvals <- replicate(2000,murakami_stat(rnorm(n1),rnorm(n2),flavor))
#' # should be uniform:
#' plot(ecdf(murakami_cdf(Bvals,n1,n2,flavor)))
#' }
#'
#' @rdname murakami_cdf
#' @export
murakami_cdf <- function(B, n1, n2, flavor=0L, lower_tail=TRUE) {
	# errors on flavor can come later, but this is important here:
	stopifnot(n1 > 0,n2 > 0)
	CUTOFF <- 11 
	allv <- .murakami_memo_stats(min(CUTOFF,n1),min(CUTOFF,n2),flavor)

	# normal approximation when beyond CUTOFF?
	ple <- stats::ecdf(allv)(B)
	pge <- stats::ecdf(-allv)(-B)
	retv <- 0.5 * (ple + (1 - pge))
	if (!lower_tail) {
		retv <- 1 - retv
	}
	return(retv)
}

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
