
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
    Biometrics, Vol. 54, No. 3 (Sep., 1998), pp. 1129-1135, http://doai.io/10.2307/2533862
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
//' Perform the Baumgartner-Weiss-Schindler test.
//'
//' @description
//'
//' Compute the Baumgartner-Weiss-Schindler test statistic, the CDF under the
//' null, or perform the hypothesis test.
//'
//' @param x a vector.
//' @param y a vector.
//'
//' @return return the BWS test statistic, \eqn{b}, or the CDF of \eqn{b} under
//' the null.
//'
//' @examples
//'
//'  set.seed(1234)
//'  x <- rnorm(1000)
//'  y <- rnorm(100)
//'  bval <- bws_stat(x,y)
//'
//'  # do it 500 times
//'  set.seed(123)
//'  bvals <- replicate(500, bws_stat(rnorm(50),rnorm(50)))
//'  pvals <- bws_cdf(bvals)
//'  # these should be uniform!
//'  \dontrun{ 
//'    plot(ecdf(pvals)) 
//'  }
//'
//' @template etc
//' @template ref-bws
//' @rdname bws_test
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

//' @param b a vector of BWS test statistics.
//' @param maxj the maximum value of j to take in the approximate computation
//' of the CDF via equation (2.5). Baumgartner \emph{et. al.} claim that a
//' value of 3 is sufficient.
//' @rdname bws_test
//' @export
// [[Rcpp::export]]
NumericVector bws_cdf(NumericVector b,int maxj=5) {
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
	}
	return retv;
}

//for vim modeline: (do not edit)
// vim:ts=2:sw=2:tw=129:fdm=marker:fmr=FOLDUP,UNFOLD:cms=//%s:tags=.c_tags;:syn=cpp:ft=cpp:mps+=<\:>:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
