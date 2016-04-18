
/*
 
  This file is part of BWStest.
  
  BWStest is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.
  
  BWStest is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public License
  along with BWStest.  If not, see <http://www.gnu.org/licenses/>.

  Baumgartner-Weiss-Schindler 2-sample test of equal distributions.
 
  see also: 

  + W. Baumgartner, P. Weiss, H. Schindler, 'A Nonparametric Test for the General Two-Sample Problem', 
    Biometrics, Vol. 54, No. 3 (Sep., 1998), pp. 1129-1135. http://doai.io/10.2307/2533862
  + M. Neuhauser, 'Exact Tests Based on the Baumgartner-Weiss-Schindler Statistic--A Survey', 
    Statistical Papers, Vol 46 (2005), pp. 1-30. http://doai.io/10.1007/BF02762032
  + M. Neuhauser, 'One-Sided Two-Sample and Trend Tests Based on a Modified Baumgartner-Weiss-Schindler 
    Statistic', J. Nonparametric Statistics, Vol 13 (2001) pp 729-739. http://doai.io/10.1080/10485250108832874
  + H. Murakami, 'K-Sample Rank Test Based on Modified Baumgartner Statistic and its Power Comparison', 
    J. Jpn. Comp. Statist. Vol 19 (2006), pp. 1-13. http://doai.io/10.1080/00949655.2010.551516


  Created: 2016.04.06
  Copyright: Steven E. Pav, 2016
  Author: Steven E. Pav <steven@corecast.io>
  Comments: Steven E. Pav
*/

#ifndef __DEF_BWS_STAT__
#define __DEF_BWS_STAT__

#include <math.h>

// 2FIX: deal with NA/NAN here?
#define MAX(a,b) ((a>b)? (a):(b))
#define MIN(a,b) ((a<b)? (a):(b))

#endif /* __DEF_BWS_STAT__ */

#include <Rcpp.h>
using namespace Rcpp;

// return number of elements in sortx[lo:hi) <= val
// or lo - 1 if none.
// these are C indexed, so you should have
// lo = 0 and hi = length(sortx) to do the
// whole vector.
template <typename T,typename D,bool comp_with_eq>
int bin_search_lstar(T sortx, D val, const int lo, const int hi) {
	int kidx;
	int ilo=lo;
	int ihi=hi;

	if (ilo < 0) { stop("out of bounds"); }

	while (ilo < ihi) {
		kidx = (ilo + ihi) / 2;

		if (comp_with_eq) {
			if (val <= sortx[kidx]) {
				ihi = kidx;
			} else {
				ilo = kidx + 1;
			}
		} else {
			if (val < sortx[kidx]) {
				ihi = kidx;
			} else {
				ilo = kidx + 1;
			}
		}
	}
	return ilo - 1;
}

// for each val in y, 
// return number of elements in sortx[lo:hi) <= val
// or lo - 1 if none.
// these are C indexed, so you should have
// lo = 0 and hi = length(sortx) to do the
// whole vector.
template <typename T,typename D,bool comp_with_eq>
IntegerVector zip_index_lstar(T sortx, T refy, const int lo, const int hi) {
	int kidx;
	int xidx, yidx, lastv;
	int ynel = refy.length();
	IntegerVector retv(ynel);

	if (ynel == 1) {
		retv[0] = bin_search_lstar<T,D,comp_with_eq>(sortx, refy[0], lo, hi);
	} else {
		if (lo < 0) { stop("out of bounds"); }
		xidx = lo;
		yidx = 0;
		if (comp_with_eq) {

			while ((xidx < hi) && (yidx < ynel)) {
				if (sortx[xidx] <= refy[yidx]) {
					xidx++;
				} else {
					retv[yidx] = xidx - 1;
					yidx++;
				}
			}

		} else {

			while ((xidx < hi) && (yidx < ynel)) {
				if (sortx[xidx] < refy[yidx]) {
					xidx++;
				} else {
					retv[yidx] = xidx - 1;
					yidx++;
				}
			}

		} 
		lastv = xidx - 1;
		while (yidx < ynel) {
			retv[yidx] = lastv;
			yidx++;
		}
	}

	return retv;
}

//2FIX: this is a stupid hacky way to compute this.
//smarter would be to modify zip_index_lstar to 
//do the addition for you?
//ah, but ties, right.
// given sorted x and sorted y, for each element in y
// find the number of elements in union(x,y) less than
// or equal to it. that is
// retv[i] = # { z in union(x,y) | z <= y[i] } for 1 <= i <= length(y)
template <typename T,typename D>
IntegerVector full_rank(T sortx, T sorty) {
	IntegerVector retv;
	retv = 2 + zip_index_lstar<T, D, true>(sortx, sorty, 0, sortx.length()) +
		zip_index_lstar<T, D, true>(sorty, sorty, 0, sorty.length());
	return retv;
}

/*
 
set.seed(1234)
x <- rnorm(1000)
y <- rnorm(1000)
stopifnot(all(unlist(lapply(sort(x),function(anx) { sum(c(x,y) <= anx) })) == fool(x,y)))

//// [[Rcpp::export]]
//IntegerVector fool(NumericVector x,NumericVector y) {
	//NumericVector sortx = clone(x); std::sort(sortx.begin(), sortx.end());
	//NumericVector sorty = clone(y); std::sort(sorty.begin(), sorty.end());
	//IntegerVector G = full_rank<NumericVector, double>(sorty, sortx);
	//return G;
//}

 */

//' @title
//' Compute the test statistic of the Baumgartner-Weiss-Schindler test.
//'
//' @description
//'
//' Compute the Baumgartner-Weiss-Schindler test statistic.
//'
//' @details
//'
//' Given vectors \eqn{X} and \eqn{Y}, computes \eqn{B_X} and \eqn{B_Y} as
//' described by Baumgartner \emph{et al.}, returning their average, \eqn{B}.
//' The test statistic approximates the variance-weighted square norm of the
//' difference in CDFs of the two distributions. For sufficiently large sample
//' sizes (more than 20, say), under the null the test statistic approaches the asymptotic
//' value computed in \code{\link{bws_cdf}}.
//'
//' The test value is an approximation of
//' \deqn{\tilde{B} = \frac{mn}{m+n} \int_0^1 \frac{1}{z(1-z)} \left(F_X(z) - F_Y(z)\right)^2 \mathrm{dz},}
//' where \eqn{m} (\eqn{n}) is the number of elements in \eqn{X} (\eqn{Y}), and
//' \eqn{F_X(z)}{F_X(z)} (\eqn{F_Y(z)}{F_Y(z)}) is the CDF of \eqn{X} (\eqn{Y}).
//'
//' The test statistic is based only on the ranks of the input. If the same
//' monotonic transform is applied to both vectors, the result should be unchanged.
//' Moreover, the test is inherently two-sided, so swapping \eqn{X} and \eqn{Y}
//' should also leave the test statistic unchanged.
//'
//' @param x a vector.
//' @param y a vector.
//'
//' @return The BWS test statistic, \eqn{B}.
//' @seealso \code{\link{bws_cdf}}, \code{\link{bws_test}}
//' @examples
//'
//' set.seed(1234)
//' x <- runif(1000)
//' y <- runif(100)
//' bval <- bws_stat(x,y)
//' # check a monotonic transform:
//' ftrans <- function(x) { log(1 + x) }
//' bval2 <- bws_stat(ftrans(x),ftrans(y))
//' stopifnot(all.equal(bval,bval2))
//' # check commutivity
//' bval3 <- bws_stat(y,x)
//' stopifnot(all.equal(bval,bval3))
//'
//' @template etc
//' @template ref-bws
//' @rdname bws_stat
//' @export
// [[Rcpp::export]]
double bws_stat(NumericVector x,NumericVector y) {
	NumericVector sortx = clone(x); std::sort(sortx.begin(), sortx.end());
	NumericVector sorty = clone(y); std::sort(sorty.begin(), sorty.end());
	IntegerVector G = full_rank<NumericVector, double>(sorty, sortx);
	IntegerVector H = full_rank<NumericVector, double>(sortx, sorty);
	double m = (double)y.size();
	double n = (double)x.size();
	double mpn = m + n;
	double nrat = mpn / n;
	double mrat = mpn / m;
	IntegerVector iseq = seq_len((int)n);
	IntegerVector jseq = seq_len((int)m);
	NumericVector iratseq = as<NumericVector>(iseq) / (n + 1.0);
	NumericVector jratseq = as<NumericVector>(jseq) / (m + 1.0);
	NumericVector xsum = (pow((as<NumericVector>(G) - nrat * as<NumericVector>(iseq)),2.0)) / (iratseq * (1.0 - iratseq) * (m * nrat));
	NumericVector ysum = (pow((as<NumericVector>(H) - mrat * as<NumericVector>(jseq)),2.0)) / (jratseq * (1.0 - jratseq) * (n * mrat));
	double Bx,By;
	Bx = sum(xsum) / n;
	By = sum(ysum) / m;
	double B = 0.5 * (Bx + By);
	return B;
}

// now compute the bws cdf essentially;
// this is via eqn (2.5) of BW&S

double gamrat(double j) {
	return pow(-1.0,j) * exp(Rf_lgammafn(j + 0.5) - Rf_lgammafn(0.5) - Rf_lgammafn(j + 1.0));
}

//' @title
//' CDF of the Baumgartner-Weiss-Schindler test under the null.
//'
//' @description
//'
//' Computes the CDF of the Baumgartner-Weiss-Schindler test statistic under the
//' null hypothesis of equal distributions.
//'
//' @details
//'
//' Given value \eqn{b}, computes the CDF of the BWS statistic under
//' the null, denoted as \eqn{\Psi(b)}{Psi(b)} by 
//' Baumgartner \emph{et al.}  The CDF is computed from 
//' equation (2.5) via numerical quadrature.
//'
//' The expression for the CDF contains the integral
//' \deqn{\int_0^1 \frac{1}{\sqrt{r^3 (1-r)}} \mathrm{exp}\left(\frac{rb}{8} - \frac{\pi^2 (4j+1)^2}{8rb}\right) \mathrm{dr}}
//' By making the change of variables \eqn{x = 2r - 1}, this can
//' be re-expressed as an integral of the form
//' \deqn{\int_{-1}^1 \frac{1}{\sqrt{1-x^2}} f(x) \mathrm{dx},}
//' for some function \eqn{f(x)} involving \eqn{b} and \eqn{j}. 
//' This integral can be approximated
//' via Gaussian quadrature using Chebyshev nodes (of the first kind), which
//' is the approach we take here.
//'
//' @param b a vector of BWS test statistics.
//' @param maxj the maximum value of j to take in the approximate computation
//' of the CDF via equation (2.5). Baumgartner \emph{et. al.} claim that a
//' value of 3 is sufficient.
//' @param lower_tail boolean, when \code{TRUE} returns \eqn{\Psi}{Psi}, otherwise
//' compute the upper tail, \eqn{1-\Psi}{1 - Psi}, which is more useful for hypothesis tests.
//'
//' @return A vector of the CDF of \eqn{b}, \eqn{\Psi(b)}{Psi(b)}.
//' @seealso \code{\link{bws_stat}}, \code{\link{bws_test}}
//' @examples
//'
//' # do it 500 times
//' set.seed(123)
//' bvals <- replicate(500, bws_stat(rnorm(50),rnorm(50)))
//' pvals <- bws_cdf(bvals)
//' # these should be uniform!
//' \dontrun{ 
//'   plot(ecdf(pvals)) 
//' }
//' 
//' # compare to Table 1 of Baumgartner et al.
//' bvals <- c(1.933,2.493,3.076,3.880,4.500,5.990)
//' tab1v <- c(0.9,0.95,0.975,0.990,0.995,0.999)
//' pvals <- bws_cdf(bvals,lower_tail=TRUE)
//' show(data.frame(B=bvals,BWS_psi=tab1v,our_psi=pvals))
//'
//' @template etc
//' @template ref-bws
//' @rdname bws_cdf
//' @export
// [[Rcpp::export]]
NumericVector bws_cdf(NumericVector b,int maxj=5,bool lower_tail=true) {
	NumericVector retv(b.length());

	double frontpart;
	double bval;
	double interm;
	double fjp1;
	// Chebyshev quadrature of the first kind...
	// c.f. https://en.wikipedia.org/wiki/Chebyshev%E2%80%93Gauss_quadrature
	int bign=101;
	IntegerVector iseq = seq_len(bign);
	NumericVector nodes_plus_1 = 1.0 + cos(M_PI * (2.0 * as<NumericVector>(iseq) - 1.0) / (2*bign));
	NumericVector weights = rep(M_PI / ((double) bign),bign);

	NumericVector prod1;
	NumericVector part2;
	NumericVector summus;

	for (int iii=0;iii < b.length();iii++) {
		bval = b[iii];
		frontpart = sqrt(M_PI / 2.0) / bval;
		prod1 = (2.0 / (nodes_plus_1)) * exp((bval/16.0) * nodes_plus_1);
		part2 = (M_PI*M_PI) / ((4.0 * bval) * nodes_plus_1);

		for (int jjj=0;jjj < maxj;jjj++) {
			fjp1 = (1.0 + 4.0 * (double)jjj);
			interm = frontpart * gamrat((double)jjj) * fjp1;
			summus = prod1 * exp(-part2 * pow(fjp1,2));
			retv[iii] += interm * sum(summus * weights);
		}
		// probably a sugar way to do this:
		retv[iii] = MAX(retv[iii],0.0);
		retv[iii] = MIN(retv[iii],1.0);
	}
	if (! lower_tail) {
		retv = 1.0 - retv;
	}
	return retv;
}

//' @title
//' Compute Murakami's test statistic.
//'
//' @description
//'
//' Compute one of the modified Baumgartner-Weiss-Schindler test statistics proposed
//' by Murakami, or Neuhauser.
//'
//' @details
//'
//' Given vectors \eqn{X} and \eqn{Y}, computes \eqn{B_{jX}} and \eqn{B_{jY}} 
//' for some \eqn{j} as described by Murakami and by Neuhauser, returning either their 
//' their average or their average distance.
//' The test statistics approximate the weighted square norm of the
//' difference in CDFs of the two distributions. 
//'
//' The test statistic is based only on the ranks of the input. If the same
//' monotonic transform is applied to both vectors, the result should be unchanged.
//'
//' The various \sQuote{flavor}s of test statistic are:
//' \describe{
//' \item{0}{The statistic of Baumgartner-Weiss-Schindler.}
//' \item{1}{Murakami's \eqn{B_1} statistic, from his 2006 paper.}
//' \item{2}{Neuhauser's difference statistic, denoted by Murakami as \eqn{B_2} in his 
//' 2012 paper.}
//' \item{3}{Murakami's \eqn{B_3} statistic, from his 2012 paper.}
//' \item{4}{Murakami's \eqn{B_4} statistic, from his 2012 paper.}
//' }
//'
//' @param x a vector.
//' @param y a vector.
//' @param flavor which \sQuote{flavor} of test statistic. 
//' @param P a matrix, as output by \code{\link{partitions::setparts}}, consisting of
//' 1s and 2s. Each column is a separate test, the rows correspond to the ordered elements
//' in the grouped set, with no possibility of ties. The 1s and 2s denote which of the two
//' samples the observation belongs to.
//'
//' @return The BWS test statistic, \eqn{B_j}. For \code{murakami_stat_parts}, a vector of
//' the test statistics.
//' @note \code{NA} and \code{NaN} not yet dealt with.
//' @note this is NYI!
//' @seealso \code{\link{bws_stat}}.
//' @examples
//'
//' set.seed(1234)
//' x <- runif(1000)
//' y <- runif(100)
//' bval <- murakami_stat(x,y,1)
//'
//' @template etc
//' @template ref-bws
//' @template ref-modtests
//' @rdname murakami_stat
//' @export
// [[Rcpp::export]]
NumericVector murakami_stat_parts(IntegerMatrix parts,int flavor=0) {
	NumericVector B1(parts.ncol());
	NumericVector B2(parts.ncol());
	NumericVector Bstat(parts.ncol());
	// ack! fill this in!

	switch(flavor) {
		case 0 : 

			break;
		case 1 :

			break;

		case 2 :

			break;

		default : 
			stop("unknown flavor; try value in [0,4]"); 
			break;
	}
	return Bstat;
}
//' @rdname murakami_stat
//' @export
// [[Rcpp::export]]
double murakami_stat(NumericVector x,NumericVector y,int flavor=0) {
	// put into the form to be consumed by the other function...
	NumericVector sortx = clone(x); std::sort(sortx.begin(), sortx.end());
	NumericVector sorty = clone(y); std::sort(sorty.begin(), sorty.end());
	IntegerVector G = full_rank<NumericVector, double>(sorty, sortx);
	double m = (double)y.size();
	double n = (double)x.size();
	double mpn = m + n;
	IntegerMatrix parts(mpn,1);
	int iii;
	for (iii=0;iii<mpn;iii++) { parts(iii,0) = 2; }
	for (iii=0;iii<m;iii++) { 
		parts(G(iii)-1,0) = 1;
	}
	NumericVector Bstat = murakami_stat_parts(parts,flavor);
	double B = Bstat(0);
	return B;
}

//for vim modeline: (do not edit)
// vim:ts=2:sw=2:tw=129:fdm=marker:fmr=FOLDUP,UNFOLD:cms=//%s:tags=.c_tags;:syn=cpp:ft=cpp:mps+=<\:>:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
