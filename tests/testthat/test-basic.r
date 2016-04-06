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

context("code runs at all")#FOLDUP
test_that("runs without error",{#FOLDUP
	set.char.seed("46316ac4-f424-43f9-b5c2-1f1897e5abbd")
	x <- rnorm(100)
	y <- rnorm(100)

	b <- bws_stat(x,y)
	b <- bws_stat(x,rnorm(1))
	b <- bws_stat(rnorm(1),y)

	bvals <- replicate(500,bws_stat(rnorm(50),rnorm(40)))
	pvals <- bws_cdf(bvals)

	htest <- bws_test(x,y)

	# sentinel
	expect_true(TRUE)
})#UNFOLD

# 2FIX: check the effects of NA
#UNFOLD

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
