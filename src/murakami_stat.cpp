
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

#ifndef __DEF_MURAKAMI_STAT__
#define __DEF_MURAKAMI_STAT__

#include <math.h>

// preallocate the Binomial Coefficients, for efficiency

// this is R code used to generate C code. [ducks]

//MAXORD <- 32
//refv <- matrix(0,nrow=MAXORD,ncol=MAXORD)
//for (iii in seq(1,nrow(refv))) {
	 //refv[iii,1] = 1; refv[iii,iii] = 1; 
	 //if (iii > 2) { for (jjj in seq(2,iii-1)) { refv[iii,jjj] = refv[iii-1,jjj-1] + refv[iii-1,jjj]; } }
 //} 
//cat(sprintf('#define MAX_ORD %d\nconst int bincoef[%d][%d] = {',ncol(refv)-1,ncol(refv),ncol(refv)),
		//paste0('\n{ ',lapply(seq_len(nrow(refv)),function(rn) { paste0(sprintf('%9s',as.character(refv[rn,])),collapse=', ') }),'},'),
		//'};\n\n',file='/tmp/binc.txt')

#define MAX_ORD 31
const int bincoef[32][32] = { 
{         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,         2,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,         3,         3,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,         4,         6,         4,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,         5,        10,        10,         5,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,         6,        15,        20,        15,         6,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,         7,        21,        35,        35,        21,         7,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,         8,        28,        56,        70,        56,        28,         8,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,         9,        36,        84,       126,       126,        84,        36,         9,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        10,        45,       120,       210,       252,       210,       120,        45,        10,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        11,        55,       165,       330,       462,       462,       330,       165,        55,        11,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        12,        66,       220,       495,       792,       924,       792,       495,       220,        66,        12,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        13,        78,       286,       715,      1287,      1716,      1716,      1287,       715,       286,        78,        13,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        14,        91,       364,      1001,      2002,      3003,      3432,      3003,      2002,      1001,       364,        91,        14,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        15,       105,       455,      1365,      3003,      5005,      6435,      6435,      5005,      3003,      1365,       455,       105,        15,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        16,       120,       560,      1820,      4368,      8008,     11440,     12870,     11440,      8008,      4368,      1820,       560,       120,        16,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        17,       136,       680,      2380,      6188,     12376,     19448,     24310,     24310,     19448,     12376,      6188,      2380,       680,       136,        17,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        18,       153,       816,      3060,      8568,     18564,     31824,     43758,     48620,     43758,     31824,     18564,      8568,      3060,       816,       153,        18,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        19,       171,       969,      3876,     11628,     27132,     50388,     75582,     92378,     92378,     75582,     50388,     27132,     11628,      3876,       969,       171,        19,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        20,       190,      1140,      4845,     15504,     38760,     77520,    125970,    167960,    184756,    167960,    125970,     77520,     38760,     15504,      4845,      1140,       190,        20,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        21,       210,      1330,      5985,     20349,     54264,    116280,    203490,    293930,    352716,    352716,    293930,    203490,    116280,     54264,     20349,      5985,      1330,       210,        21,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        22,       231,      1540,      7315,     26334,     74613,    170544,    319770,    497420,    646646,    705432,    646646,    497420,    319770,    170544,     74613,     26334,      7315,      1540,       231,        22,         1,         0,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        23,       253,      1771,      8855,     33649,    100947,    245157,    490314,    817190,   1144066,   1352078,   1352078,   1144066,    817190,    490314,    245157,    100947,     33649,      8855,      1771,       253,        23,         1,         0,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        24,       276,      2024,     10626,     42504,    134596,    346104,    735471,   1307504,   1961256,   2496144,   2704156,   2496144,   1961256,   1307504,    735471,    346104,    134596,     42504,     10626,      2024,       276,        24,         1,         0,         0,         0,         0,         0,         0,         0}, 
{         1,        25,       300,      2300,     12650,     53130,    177100,    480700,   1081575,   2042975,   3268760,   4457400,   5200300,   5200300,   4457400,   3268760,   2042975,   1081575,    480700,    177100,     53130,     12650,      2300,       300,        25,         1,         0,         0,         0,         0,         0,         0}, 
{         1,        26,       325,      2600,     14950,     65780,    230230,    657800,   1562275,   3124550,   5311735,   7726160,   9657700,  10400600,   9657700,   7726160,   5311735,   3124550,   1562275,    657800,    230230,     65780,     14950,      2600,       325,        26,         1,         0,         0,         0,         0,         0}, 
{         1,        27,       351,      2925,     17550,     80730,    296010,    888030,   2220075,   4686825,   8436285,  13037895,  17383860,  20058300,  20058300,  17383860,  13037895,   8436285,   4686825,   2220075,    888030,    296010,     80730,     17550,      2925,       351,        27,         1,         0,         0,         0,         0}, 
{         1,        28,       378,      3276,     20475,     98280,    376740,   1184040,   3108105,   6906900,  13123110,  21474180,  30421755,  37442160,  40116600,  37442160,  30421755,  21474180,  13123110,   6906900,   3108105,   1184040,    376740,     98280,     20475,      3276,       378,        28,         1,         0,         0,         0}, 
{         1,        29,       406,      3654,     23751,    118755,    475020,   1560780,   4292145,  10015005,  20030010,  34597290,  51895935,  67863915,  77558760,  77558760,  67863915,  51895935,  34597290,  20030010,  10015005,   4292145,   1560780,    475020,    118755,     23751,      3654,       406,        29,         1,         0,         0}, 
{         1,        30,       435,      4060,     27405,    142506,    593775,   2035800,   5852925,  14307150,  30045015,  54627300,  86493225, 119759850, 145422675, 155117520, 145422675, 119759850,  86493225,  54627300,  30045015,  14307150,   5852925,   2035800,    593775,    142506,     27405,      4060,       435,        30,         1,         0}, 
{         1,        31,       465,      4495,     31465,    169911,    736281,   2629575,   7888725,  20160075,  44352165,  84672315, 141120525, 206253075, 265182525, 300540195, 300540195, 265182525, 206253075, 141120525,  84672315,  44352165,  20160075,   7888725,   2629575,    736281,    169911,     31465,      4495,       465,        31,         1} };


// 2FIX: deal with NA/NAN here?
#define MAX(a,b) ((a>b)? (a):(b))
#define MIN(a,b) ((a<b)? (a):(b))

#endif /* __DEF_MURAKAMI_STAT__ */

#include <Rcpp.h>
using namespace Rcpp;

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
//' \item{5}{Murakami's \eqn{B_5} statistic, from his 2012 paper, with a log weighting.}
//' }
//'
//' @param x a vector.
//' @param y a vector.
//' @param flavor which \sQuote{flavor} of test statistic. 
//' @param Parts a matrix, as output by \code{\link{setparts}}, consisting of
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
//' \dontrun{
//' if (require(partitions)) {
//'   nx <- 6
//'   ny <- 5
//'   # monte carlo
//'   set.seed(1234)
//'   repli <- replicate(3000,murakami_stat(rnorm(nx),rnorm(ny),0L))
//'   # under the null, perform the permutation test:
//'   P <- partitions::setparts(c(nx,ny))
//'   if (nx == ny) { allem <- murakami_stat_parts(cbind(P,3-P),0L) 
//'   } else { allem <- murakami_stat_parts(P,0L) }
//'   plot(ecdf(allem)) 
//'   lines(ecdf(repli),col='red') 
//' }
//' }
//'
//' @template etc
//' @template ref-bws
//' @template ref-modtests
//' @rdname murakami_stat
//' @export
// [[Rcpp::export]]
NumericVector murakami_stat_parts(IntegerMatrix Parts,int flavor=0) {
	int nc=Parts.ncol();
	int N=Parts.nrow();
	int nx,ny;
	int iii,jjj;
	int xcount,ycount;
	double evx,evy,vvx,vvy,Np1,nxp1,nxp2,nyp1,nyp2;
	double nonce,npart,dpart,bplus;
	NumericVector B1(nc);
	NumericVector B2(nc);
	NumericVector Bstat(nc);

	Np1  = N + 1;

	for (jjj=0;jjj<nc;jjj++) {
		// prealloc just to make sure:
		B1(jjj) = 0.0;
		B2(jjj) = 0.0;

		// compute the number in x and y, essentially.
		nx = 0;
		for (iii=0;iii<N;iii++) {
			if (Parts(iii,jjj) == 1) { nx++; }
		}
		ny = N - nx;

		nxp1 = nx + 1;
		nyp1 = ny + 1;

		switch(flavor) {
			case 0 :
			case 2 :
				evx = N / nx;
				evy = N / ny;
				vvx = ny * N;
				vvy = nx * N;
				break ;
			case 1 :
			case 3 :
			case 4 :
			case 5 :
				nxp2 = nx + 2;
				nyp2 = ny + 2;
				evx = Np1 / nxp1;
				evy = Np1 / nyp1;
				vvx = ny * Np1 / nxp2;
				vvy = nx * Np1 / nyp2;
				break ;
			default : 
				stop("unknown flavor; try value in [0,1,2,3,4,5]"); 
				break;
		}

		xcount = 0;
		ycount = 0;

		// subtle hint: templating could be your friend here..
		for (iii=0;iii<N;iii++) {
			if (Parts(iii,jjj) == 1) {
				xcount++;
				nonce = xcount / (nxp1);
				npart = ((iii+1) - evx * xcount);
				dpart = (nonce * (1.0 - nonce) * vvx);
				switch(flavor) {
					case 0:
						bplus = npart * npart / dpart;
						break;
					case 1:
						bplus = (1.0 / nx) * npart * npart / dpart;
						break;
					case 2:
						bplus = npart * abs(npart) / dpart;
						break;
					case 3:
						bplus = (1.0 / nx) * npart * npart / (dpart * dpart);
						break;
					case 4:
						bplus = (1.0 / nx) * abs(npart) / (dpart * dpart);
						break;
					case 5:
						bplus = (1.0 / nx) * npart * npart / log(dpart);
						break;
				}
				B1(jjj) += bplus;
			} else {
				ycount++;
				nonce = ycount / (nyp1);
				npart = ((iii+1) - evy * ycount);
				dpart = (nonce * (1.0 - nonce) * vvy);
				switch(flavor) {
					case 0:
						bplus = npart * npart / dpart;
						break;
					case 1:
						bplus = (1.0 / ny) * npart * npart / dpart;
						break;
					case 2:
						bplus = npart * abs(npart) / dpart;
						break;
					case 3:
						bplus = (1.0 / ny) * npart * npart / (dpart * dpart);
						break;
					case 4:
						bplus = (1.0 / ny) * abs(npart) / (dpart * dpart);
						break;
					case 5:
						bplus = (1.0 / ny) * npart * npart / log(dpart);
						break;
				}
				B2(jjj) += bplus;
			}
		}
	}

	// combine them. usually the average.
	switch(flavor) {
		case 2 : 
			Bstat = 0.5 * (B2 - B1);
			break;
		default :
			Bstat = 0.5 * (B1 + B2);
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
	int n = x.size();
	int N = n + y.size();
	IntegerMatrix parts(N,1);
	int iii;
	for (iii=0;iii<N;iii++) { parts(iii,0) = 2; }
	for (iii=0;iii<n;iii++) { 
		parts(G(iii)-1,0) = 1;
	}
	NumericVector Bstat = murakami_stat_parts(parts,flavor);
	double B = Bstat(0);
	return B;
}

template <int flavor>
NumericVector murakami_allv(size_t nx, size_t ny) {
	size_t N = nx + ny;
	int nc = bincoef[N][n1];
	int iii,jjj;
	int xcount,ycount;
	double evx,evy,vvx,vvy,Np1,nxp1,nxp2,nyp1,nyp2;
	double nonce,npart,dpart,bplus;
	// preallocate
	NumericVector B1(nc);
	NumericVector B2(nc);
	NumericVector Bstat(nc);

	// this will hold the permutations.
	NumericVector xrank(nx);
	for (iii=0;iii<nx;iii++) {
		xrank(iii) = 1 + iii;
	}

	Np1  = N + 1;
	nxp1 = nx + 1;
	nyp1 = ny + 1;

	if ((flavor == 0) || (flavor == 2)) {
		evx = N / nx;
		evy = N / ny;
		vvx = ny * N;
		vvy = nx * N;
	}
	if ((flavor == 1) || (flavor == 3) || (flavor == 4) || (flavor == 5)) {
		nxp2 = nx + 2;
		nyp2 = ny + 2;
		evx = Np1 / nxp1;
		evy = Np1 / nyp1;
		vvx = ny * Np1 / nxp2;
		vvy = nx * Np1 / nyp2;
	}
	for (jjj=0;jjj<nc;jjj++) {
		if (jjj!=0) {
			// increment.
			iii = nx - 1;
			while ((iii > 0) && (xperm(iii) == N - nx + iii + 1)) {
				iii--;
			}
			xperm(iii)++;
			for (;iii < nx - 1;iii++) {
				xperm(iii+1) = xperm(iii) + 1;
			}
		}
		// now use xperm...

	}

}



//for vim modeline: (do not edit)
// vim:ts=2:sw=2:tw=129:fdm=marker:fmr=FOLDUP,UNFOLD:cms=//%s:tags=.c_tags;:syn=cpp:ft=cpp:mps+=<\:>:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
