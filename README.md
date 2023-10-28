# BNPGrangerTestR

<!-- badges: start -->
<!-- badges: end -->

An R package that implements the (BNP) Granger causality test described in 
Gutiérrez et al. (2023). See the 
[documentation](https://igutierrezm.github.io/BNPGrangerTestR/index.html) 
for details and the 
[getting started](https://igutierrezm.github.io/BNPGrangerTestR/articles/getting_started.html) 
vignette for a example.

## Installation

You can install BNPGrangerTestR from source as follows:

```r
remotes::install_github("igutierrezm/BNPGrangerTestR")
```

This will install the R package BNPGrangerTestR, but not the Julia packages on
which it depends. In order to install these Julia packages, run

```r
BNPGrangerTestR::setup()
```

You only need to do this once.

BNPGrangerTestR requires Julia v1.6.1+. You can download it 
[here](https://julialang.org/downloads/), 
and then install it following these 
[instructions](https://julialang.org/downloads/platform/).
