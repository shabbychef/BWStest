

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


```r
require(BWStest)
x <- rnorm(100)
y <- rnorm(100)
bval <- bws_stat(x, y)

# now compute a bunch under the null:
bvals <- replicate(5000, bws_stat(rnorm(100), rnorm(100)))
# compute the approximate p-values under the null:
pvals <- bws_cdf(bvals)

require(ggplot2)
ph <- ggplot(data.frame(pv = pvals), aes(sample = pv)) + 
    stat_qq(distribution = stats::qunif)
print(ph)
```

<img src="github_extra/figure/babysteps-1.png" title="plot of chunk babysteps" alt="plot of chunk babysteps" width="700px" height="600px" />

Looks good to me!

## Future work

Eventually the package will support the modified tests proposed by 
[Neuhauser](http://doai.io/10.1007/BF02762032) and 
[Murakami](http://doai.io/10.1080/00949655.2010.551516).

