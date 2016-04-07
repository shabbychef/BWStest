# Copyright 2016 Steven E. Pav. All Rights Reserved.
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

# env var:
# nb: 
# see also:
# todo:
# changelog: 
#
# Created: 2016.04.06
# Copyright: Steven E. Pav, 2016-2016
# Author: Steven E. Pav
# Comments: Steven E. Pav

# helpers#FOLDUP
set.char.seed <- function(str) {
	set.seed(as.integer(charToRaw(str)))
}
THOROUGHNESS <- getOption('test.thoroughness',1.0)
#UNFOLD

context("basic sanity checks")#FOLDUP
test_that("bws_stat commutative",{#FOLDUP
	set.char.seed("dbf07485-dfb6-4b1f-bb63-7ac53e9fb4fa")
	x <- rnorm(100)
	y <- rnorm(100)

	b1 <- bws_stat(x,y)
	b2 <- bws_stat(y,x)
	expect_equal(b1,b2)

	# sentinel
	expect_true(TRUE)
})#UNFOLD
test_that("bws_stat translation invariant",{#FOLDUP
	set.char.seed("f2d7b1a6-3c47-4a4c-8039-55c8a6e78301")
	x <- rnorm(100)
	y <- rnorm(100)

	b1 <- bws_stat(x,y)
	b2 <- bws_stat(x + 1,y + 1)
	expect_equal(b1,b2)

	# sentinel
	expect_true(TRUE)
})#UNFOLD
test_that("bws_stat monotonic transform invariant",{#FOLDUP
	set.char.seed("f2d7b1a6-3c47-4a4c-8039-55c8a6e78301")
	x <- runif(100)
	y <- runif(100)
	ffunc <- function(z) { log(1+z) }

	b1 <- bws_stat(x,y)
	b2 <- bws_stat(ffunc(x),ffunc(y))
	expect_equal(b1,b2)

	# sentinel
	expect_true(TRUE)
})#UNFOLD
test_that("bws_cdf matches table I",{#FOLDUP
	bvals <- c(1.933,2.493,3.076,3.880,4.500,5.990)
	tab1v <- c(0.9,0.95,0.975,0.990,0.995,0.999)
	pvals <- bws_cdf(bvals,lower_tail=TRUE)
	show(data.frame(B=bvals,BWS_psi=tab1v,our_psi=pvals))
	expect_equal(tab1v,pvals,tolerance=1e-4) 

	# sentinel
	expect_true(TRUE)
})#UNFOLD
test_that("bws_test under alternative",{#FOLDUP
	set.char.seed("ee76160c-844e-4b3d-9cb7-193d2621355c")
	x <- rnorm(100)
	y <- rnorm(100,mean=3)
	ht <- bws_test(x,y)
	expect_lt(ht$p.value,1e-8)

	# sentinel
	expect_true(TRUE)
})#UNFOLD
test_that("bws_cdf sane",{#FOLDUP
	bvals <- exp(seq(log(0.1),log(100),length.out=201))
	pvals <- bws_cdf(bvals,lower_tail=TRUE)
	expect_true(all(pvals >= 0))
	expect_true(all(pvals <= 1))
	pvals <- bws_cdf(bvals,lower_tail=FALSE)
	expect_true(all(pvals >= 0))
	expect_true(all(pvals <= 1))

	# sentinel
	expect_true(TRUE)
})#UNFOLD
test_that("bws_cdf sane",{#FOLDUP
	is.sorted <- function(x) { all(diff(x) >= 0) }
	bvals <- exp(seq(log(0.1),log(4),length.out=201))
	pvals <- bws_cdf(bvals,lower_tail=TRUE)
	expect_true(is.sorted(pvals))
	pvals <- bws_cdf(bvals,lower_tail=FALSE)
	expect_true(is.sorted(rev(pvals)))

	# sentinel
	expect_true(TRUE)
})#UNFOLD
test_that("bws_cdf deterministic",{#FOLDUP
	bvals <- exp(seq(log(0.1),log(100),length.out=201))
	pv1 <- bws_cdf(bvals)
	pv2 <- bws_cdf(bvals)
	expect_equal(pv1,pv2)

	# sentinel
	expect_true(TRUE)
})#UNFOLD

# 2FIX: check the effects of NA
#UNFOLD

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
