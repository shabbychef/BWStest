dnl divert here just means the output from basedefs does not appear.
divert(-1)
include(basedefs.m4)
divert(0)dnl
Package: PKG_NAME()
Type: Package
Maintainer: Steven E. Pav <shabbychef@gmail.com>
Authors@R: c(person(c("Steven", "E."), "Pav", 
    role=c("aut","cre"),
    email="shabbychef@gmail.com",
		comment = c(ORCID = "0000-0002-4197-6195")))
Version: VERSION()
Date: DATE()
License: LGPL-3
Title: Baumgartner Weiss Schindler Test of Equal Distributions
BugReports: https://github.com/shabbychef/PKG_NAME()/issues
Description: Performs the 'Baumgartner-Weiss-Schindler' two-sample test of equal
   probability distributions, <doi:10.2307/2533862>. Also performs
   similar rank-based tests for equal probability distributions due to
   Neuhauser <doi:10.1080/10485250108832874> and
   Murakami <doi:10.1080/00949655.2010.551516>.
Imports:
    memoise,
    Rcpp (>= 0.12.3)
LinkingTo: Rcpp
Suggests:
    testthat
RoxygenNote: 6.1.1
URL: https://github.com/shabbychef/PKG_NAME()
dnl VignetteBuilder: knitr
Collate:
m4_R_FILES()
dnl vim:ts=2:sw=2:tw=79:syn=m4:ft=m4
