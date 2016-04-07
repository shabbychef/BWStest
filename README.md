

# BWStest

[![Build Status](https://travis-ci.org/shabbychef/BWStest.png)](https://travis-ci.org/shabbychef/BWStest)
[![codecov.io](http://codecov.io/github/shabbychef/BWStest/coverage.svg?branch=master)](http://codecov.io/github/shabbychef/BWStest?branch=master)
![RCpp](https://img.shields.io/badge/RCpp-inside-blue.svg)

Performs the [Baumgartner-Weiß-Schindler 2-sample test](http://doai.io/10.2307/2533862) of equal probability
distributions. 

-- Steven E. Pav, shabbychef@gmail.com

## Installation

This package can be installed 
from github:


```r
# get snapshot from github (may be buggy)
if (require(devtools)) {
    install_github("shabbychef/BWStest")
}
```

# Basic Usage

The front end for the hypothesis test is the function `bws_test`. At the moment, this only
supports the classical Baumgartner-Weiß-Schindler test, returning a `htest` object:


```r
require(BWStest)
set.seed(123)
# under the null:
x <- rnorm(100)
y <- rnorm(100)
bval <- bws_test(x, y)
show(bval)
```

```
## 
## 	two-sample BWS test
## 
## data:  x vs. y
## b = 2, p-value = 0.1
```

```r
# under the alternative:
z <- rnorm(100, mean = 1)
bval <- bws_test(x, z)
show(bval)
```

```
## 
## 	two-sample BWS test
## 
## data:  x vs. z
## b = 30, p-value <2e-16
```

## Checking the null

  _Doverai No Proverai_ (Trust, but verify.) -- Russian proverb.

Here we perform 5000 simulations of the BWS test under the null hypothesis, then
compute the CDF of the test statistic. If the code is correct, the resultant p-values
should be uniform. So I q-q plot under the uniform law:


```r
require(BWStest)

# now compute a bunch under the null:
set.seed(1234)
bvals <- replicate(5000, bws_stat(rnorm(100), rnorm(100)))
# compute the approximate p-values under the null:
pvals <- bws_cdf(bvals)

require(ggplot2)
ph <- ggplot(data.frame(pv = pvals), aes(sample = pv)) + 
    stat_qq(distribution = stats::qunif)
print(ph)
```

<img src="github_extra/figure/babysteps-1.png" title="plot of chunk babysteps" alt="plot of chunk babysteps" width="500px" height="400px" />

Looks good to me!

## Under the alternative

Here we replicate figure 2A of Baumgartner _et al._. We draw two samples from the normal distribution,
both with unit standard deviation, letting _a_ be the difference in means. 
We check the empirical rejection rate at the 0.05 level for a few different tests (the two sample Cramer Von Mises test is not readily
available for modern version of R, apparently). 
As in Baumgartner, we find that the lowly t-test
is the most powerful in this case, with the BWS and Wilcoxon displaying similar power, then the KS
test the least powerful. Note that the Kolmogorov Smirnov test does not appear to have nominal
coverage under the null, probably due to the small sample size.


```r
n.sim <- 10000
avals <- seq(0, 3.2, length.out = 17)
alpha <- 0.05
mnsize <- 10
set.seed(1234)
simul <- sapply(avals, function(a) {
    rejs <- replicate(n.sim, {
        x <- rnorm(mnsize, mean = 0)
        y <- rnorm(mnsize, mean = a)
        bws <- bws_cdf(bws_stat(x, y), lower_tail = FALSE) <= 
            alpha
        ttv <- t.test(x, y, alternative = "two.sided")$p.value <= 
            alpha
        ksv <- ks.test(x, y, alternative = "two.sided")$p.value <= 
            alpha
        wcx <- wilcox.test(x, y, alternative = "two.sided")$p.value <= 
            alpha
        c(bws, ttv, ksv, wcx)
    })
    rejrate <- rowMeans(rejs)
    names(rejrate) <- c("BWS test", "t test", "Kolmogorov Smirnov test", 
        "Wilcoxon test")
    rejrate
}, simplify = "matrix")

Arejrates <- data.frame(t(simul))
Arejrates$a <- avals
```

```r
library(tidyr)
plotdf <- tidyr::gather(Arejrates, "test", "rejection_rate", 
    -a)
library(ggplot2)
ph <- ggplot(plotdf, aes(x = a, y = rejection_rate, 
    group = test, colour = test)) + geom_line() + geom_point() + 
    labs(x = "a, difference in means", y = "rejection rate")
print(ph)
```

<img src="github_extra/figure/fig_two_A-1.png" title="plot of chunk fig_two_A" alt="plot of chunk fig_two_A" width="700px" height="500px" />

Here we replicate figure 2B of Baumgartner _et al._. We draw two samples from the normal distribution,
both with zero mean, one with unit standard deviation, the other with standard deviation of _sigma_.
We compute the empirical rejection rate at the 0.05 level, dropping the t-test
since it is not relevant for this formulation.
As in Baumgartner, 
we find the BWS test is the most powerful, followed by KS test, then Wilcoxon, which is essentially
useless in this simulation.


```r
n.sim <- 10000
svals <- seq(1, 45, length.out = 10)
alpha <- 0.05
mnsize <- 10
set.seed(1234)
simul <- sapply(svals, function(s) {
    rejs <- replicate(n.sim, {
        x <- rnorm(mnsize, mean = 0, sd = 1)
        y <- rnorm(mnsize, mean = 0, sd = s)
        bws <- bws_cdf(bws_stat(x, y), lower_tail = FALSE) <= 
            alpha
        ksv <- ks.test(x, y, alternative = "two.sided")$p.value <= 
            alpha
        wcx <- wilcox.test(x, y, alternative = "two.sided")$p.value <= 
            alpha
        c(bws, ksv, wcx)
    })
    rejrate <- rowMeans(rejs)
    names(rejrate) <- c("BWS test", "Kolmogorov Smirnov test", 
        "Wilcoxon test")
    rejrate
}, simplify = "matrix")

Brejrates <- data.frame(t(simul))
Brejrates$sigma <- svals
```

```r
library(tidyr)
plotdf <- tidyr::gather(Brejrates, "test", "rejection_rate", 
    -sigma)
library(ggplot2)
ph <- ggplot(plotdf, aes(x = sigma, y = rejection_rate, 
    group = test, colour = test)) + geom_line() + geom_point() + 
    labs(x = "sigma, ratio of standard deviations", 
        y = "rejection rate")
print(ph)
```

<img src="github_extra/figure/fig_two_B-1.png" title="plot of chunk fig_two_B" alt="plot of chunk fig_two_B" width="700px" height="500px" />

Here we replicate figure 3A of Baumgartner _et al._. We draw two samples from the exponential distribution,
letting _l_ be the ratio of the rate parameters of the two populations.
We compute the empirical rejection rate at the 0.05 level.
As in Baumgartner, 
we find the BWS test is the most powerful, followed by Wilcoxon, then KS test.


```r
n.sim <- 10000
lvals <- seq(1, 12)
alpha <- 0.05
mnsize <- 10
set.seed(1234)
simul <- sapply(lvals, function(l) {
    rejs <- replicate(n.sim, {
        x <- rexp(mnsize, rate = 1)
        y <- rexp(mnsize, rate = l)
        bws <- bws_cdf(bws_stat(x, y), lower_tail = FALSE) <= 
            alpha
        ksv <- ks.test(x, y, alternative = "two.sided")$p.value <= 
            alpha
        wcx <- wilcox.test(x, y, alternative = "two.sided")$p.value <= 
            alpha
        c(bws, ksv, wcx)
    })
    rejrate <- rowMeans(rejs)
    names(rejrate) <- c("BWS test", "Kolmogorov Smirnov test", 
        "Wilcoxon test")
    rejrate
}, simplify = "matrix")

Crejrates <- data.frame(t(simul))
Crejrates$lratio <- lvals
```

```r
library(tidyr)
plotdf <- tidyr::gather(Crejrates, "test", "rejection_rate", 
    -lratio)
library(ggplot2)
ph <- ggplot(plotdf, aes(x = lratio, y = rejection_rate, 
    group = test, colour = test)) + geom_line() + geom_point() + 
    labs(x = "l, ratio of rate parameters", y = "rejection rate")
print(ph)
```

<img src="github_extra/figure/fig_three_A-1.png" title="plot of chunk fig_three_A" alt="plot of chunk fig_three_A" width="700px" height="500px" />

Here we replicate figure 3B of Baumgartner _et al._. We draw two samples, one from the normal distribution
with zero mean and varianced one-twelth, the other uniformly on -0.5 to 0.5. We take equal sample sizes
from these two populations, then vary the sample size, checking the
empirical rejection rate at the 0.05 level.
As in Baumgartner, 
we find the BWS test is the most powerful, followed by the KS test, and the Wilcoxon is useless. 
The power found here seems to be much higher than that found by Baumgartner, with near 100% rejection
rate for a sample size of around 600, while Baumgartner finds those kinds of rejection rates at around
a sample size of 1200. I theorize that Baumgartner _et al._ are plotting the _sum_ of the sample sizes
on the _x_ axis.


```r
n.sim <- 10000
mvals <- seq(25, 1275, by = 125)
alpha <- 0.05
set.seed(1234)
simul <- sapply(mvals, function(m) {
    rejs <- replicate(n.sim, {
        x <- rnorm(m, mean = 0, sd = 1/sqrt(12))
        y <- runif(m, min = -0.5, max = 0.5)
        bws <- bws_cdf(bws_stat(x, y), lower_tail = FALSE) <= 
            alpha
        ksv <- ks.test(x, y, alternative = "two.sided")$p.value <= 
            alpha
        wcx <- wilcox.test(x, y, alternative = "two.sided")$p.value <= 
            alpha
        c(bws, ksv, wcx)
    })
    rejrate <- rowMeans(rejs)
    names(rejrate) <- c("BWS test", "Kolmogorov Smirnov test", 
        "Wilcoxon test")
    rejrate
}, simplify = "matrix")

Drejrates <- data.frame(t(simul))
Drejrates$ssize <- mvals
```

```r
library(tidyr)
plotdf <- tidyr::gather(Drejrates, "test", "rejection_rate", 
    -ssize)
library(ggplot2)
ph <- ggplot(plotdf, aes(x = ssize, y = rejection_rate, 
    group = test, colour = test)) + geom_line() + geom_point() + 
    labs(x = "m=n, sample size", y = "rejection rate")
print(ph)
```

<img src="github_extra/figure/fig_three_B-1.png" title="plot of chunk fig_three_B" alt="plot of chunk fig_three_B" width="700px" height="500px" />

## Future work

Eventually the package will support the modified tests proposed by 
[Neuhauser](http://doai.io/10.1007/BF02762032) and 
[Murakami](http://doai.io/10.1080/00949655.2010.551516).

