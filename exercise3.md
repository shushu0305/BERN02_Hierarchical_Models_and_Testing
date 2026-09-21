# exercise3
Shuyi Liu

## Hierarchical Models and Testing

The aim of this analysis is to investigate whether the descriptive
social norm intervention has an effect on the probability of hotel
guests reusing their towels, while accounting for heterogeneity between
studies.

### Data preparation

``` r
library(readr)
library(tidyr)
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
towel_data <- read_delim(
  "towelData.csv",
  delim = ";",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
) %>%
  pivot_wider(
    names_from = Towel.Reuse,
    values_from = Count
  ) %>%
  mutate(
    Total = Yes + No
  )
```

Prepare variables for modelling. Use Control as the reference category.

``` r
towel_data <- towel_data %>%
  mutate(
    Group = factor(
      Group,
      levels = c("Control", "Social Norm")
    ),
    Source = factor(Source),
    Reuse_rate = Yes / Total
  )
# Check the structure of the prepared dataset
glimpse(towel_data)
```

    Rows: 14
    Columns: 9
    $ Source     <fct> Goldstein.2008.Ex1, Goldstein.2008.Ex1, Goldstein.2008.Ex2,…
    $ AuthorName <chr> "Goldstein et al.", "Goldstein et al.", "Goldstein et al.",…
    $ Experiment <dbl> 1, 1, 2, 2, 2, 2, 3, 3, 1, 1, 1, 1, 2, 2
    $ Year       <dbl> 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2010, 2010,…
    $ Group      <fct> Control, Social Norm, Control, Social Norm, Control, Social…
    $ Yes        <dbl> 74, 98, 103, 587, 77, 406, 82, 278, 21, 21, 123, 472, 28, 1…
    $ No         <dbl> 137, 124, 174, 731, 58, 249, 105, 277, 4, 3, 24, 104, 2, 31
    $ Total      <dbl> 211, 222, 277, 1318, 135, 655, 187, 555, 25, 24, 147, 576, …
    $ Reuse_rate <dbl> 0.3507109, 0.4414414, 0.3718412, 0.4453718, 0.5703704, 0.61…

``` r
towel_data %>%
  select(
    Source,
    Group,
    Yes,
    No,
    Total,
    Reuse_rate
  )
```

    # A tibble: 14 × 6
       Source             Group         Yes    No Total Reuse_rate
       <fct>              <fct>       <dbl> <dbl> <dbl>      <dbl>
     1 Goldstein.2008.Ex1 Control        74   137   211      0.351
     2 Goldstein.2008.Ex1 Social Norm    98   124   222      0.441
     3 Goldstein.2008.Ex2 Control       103   174   277      0.372
     4 Goldstein.2008.Ex2 Social Norm   587   731  1318      0.445
     5 Schulz2008.Ex2     Control        77    58   135      0.570
     6 Schulz2008.Ex2     Social Norm   406   249   655      0.620
     7 Schulz2008.Ex3     Control        82   105   187      0.439
     8 Schulz2008.Ex3     Social Norm   278   277   555      0.501
     9 Mair2010           Control        21     4    25      0.84 
    10 Mair2010           Social Norm    21     3    24      0.875
    11 Bohner2014.Ex1     Control       123    24   147      0.837
    12 Bohner2014.Ex1     Social Norm   472   104   576      0.819
    13 Bohner2014.Ex2     Control        28     2    30      0.933
    14 Bohner2014.Ex2     Social Norm   101    31   132      0.765

We can find the differences in baseline towel reuse rates between
experiments from the graph. In addition, the difference between the
Social Norm and Control conditions was not identical across studies.
These patterns suggest that observations from different studies should
not be treated as completely homogeneous and motivate the use of a
hierarchical model.

``` r
library(ggplot2)
ggplot(
  towel_data,
  aes(
    x = Group,
    y = Reuse_rate,
    group = Source
  )
) +
  geom_point(size = 2) +
  geom_line() +
  facet_wrap(~ Source) +
  labs(
    x = "Experimental condition",
    y = "Observed towel reuse proportion",
    title = "Towel reuse across studies"
  ) +
  theme_minimal()
```

![](exercise3_files/figure-commonmark/unnamed-chunk-3-1.png)

### Model choice

The response was modelled using a binomial distribution because the data
represent the number of towel reuses out of a known total number of
guests. A logit link was used to relate the towel reuse probability to
the intervention and study effects. The logit transformation ensures
that fitted probabilities remain between 0 and 1.

So finally we have the model:

$$Y_{ij} \sim \text{Binomial}(n_{ij}, p_{ij})$$

$$\text{logit}(p_{ij})=
\beta_0
+
u_{0j}
+
\beta_1
\text{Group}_{ij}$$

$$u_{0j} \sim N(0,\sigma_0^2)$$

``` r
library(brms)
```

    Loading required package: Rcpp

    Loading 'brms' package (version 2.23.0). Useful instructions
    can be found by typing help('brms'). A more detailed introduction
    to the package is available through vignette('brms_overview').


    Attaching package: 'brms'

    The following object is masked from 'package:stats':

        ar

``` r
# Random intercept and random slope model.
# baseline towel reuse are allowed to vary between studies.
formula_m1 <- bf(
  Yes | trials(Total) ~
    Group +
    (1 | Source)
)
# Inspect model parameters
get_prior(
  formula_m1,
  data = towel_data,
  family = binomial(link = "logit")
)
```

                    prior     class            coef  group resp dpar nlpar lb ub
                   (flat)         b                                             
                   (flat)         b GroupSocialNorm                             
     student_t(3, 0, 2.5) Intercept                                             
     student_t(3, 0, 2.5)        sd                                         0   
     student_t(3, 0, 2.5)        sd                 Source                  0   
     student_t(3, 0, 2.5)        sd       Intercept Source                  0   
     tag       source
              default
         (vectorized)
              default
              default
         (vectorized)
         (vectorized)

``` r
# Fit Model 1
fit_m1 <- brm(
  Yes | trials(Total) ~
    Group + (1 | Source),
  data = towel_data,
  family = binomial(link = "logit"),
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123,
  control = list(adapt_delta = 0.95)
)
```

    Compiling Stan program...

    Trying to compile a simple C file

    Running /usr/lib/R/bin/R CMD SHLIB foo.c
    using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04.3) 11.4.0’
    gcc -I"/usr/share/R/include" -DNDEBUG   -I"/home/shu/R/x86_64-pc-linux-gnu-library/4.5/Rcpp/include/"  -I"/home/shu/R/x86_64-pc-linux-gnu-library/4.5/RcppEigen/include/"  -I"/home/shu/R/x86_64-pc-linux-gnu-library/4.5/RcppEigen/include/unsupported"  -I"/home/shu/R/x86_64-pc-linux-gnu-library/4.5/BH/include" -I"/home/shu/R/x86_64-pc-linux-gnu-library/4.5/StanHeaders/include/src/"  -I"/home/shu/R/x86_64-pc-linux-gnu-library/4.5/StanHeaders/include/"  -I"/home/shu/R/x86_64-pc-linux-gnu-library/4.5/RcppParallel/include/" -DRCPP_PARALLEL_USE_TBB=1 -DTBB_INTERFACE_NEW -I/home/shu/R/x86_64-pc-linux-gnu-library/4.5/RcppParallel/include -I"/home/shu/R/x86_64-pc-linux-gnu-library/4.5/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -D_HAS_AUTO_PTR_ETC=0  -include '/home/shu/R/x86_64-pc-linux-gnu-library/4.5/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1       -fpic  -g -O2 -ffile-prefix-map=/build/r-base-QoVTUP/r-base-4.5.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2  -c foo.c -o foo.o
    In file included from <command-line>:
    /home/shu/R/x86_64-pc-linux-gnu-library/4.5/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:3:10: fatal error: stdexcept: No such file or directory
        3 | #include <stdexcept>
          |          ^~~~~~~~~~~
    compilation terminated.
    make: *** [/usr/lib/R/etc/Makeconf:202: foo.o] Error 1

    Start sampling

Posterior parameter summary

``` r
# Population-level effects:
# overall intercept and intervention effect
fixef(fit_m1, probs = c(0.05, 0.95))
```

                     Estimate  Est.Error          Q5       Q95
    Intercept       0.4508972 0.45132534 -0.27062833 1.1561137
    GroupSocialNorm 0.2113360 0.07792375  0.08493409 0.3397426

``` r
# Group-level standard deviations:
# between-study heterogeneity
VarCorr(fit_m1, probs = c(0.05, 0.95))
```

    $Source
    $Source$sd
              Estimate Est.Error        Q5      Q95
    Intercept 1.115664 0.4139999 0.6435096 1.897218

### Hypothesis testing

The population-level coefficient for the Social Norm condition,
$\beta_{\text1}$, represents the effect of the intervention relative to
the Control condition on the log-odds of towel reuse. The hypotheses
were formulated as

$H_0: \beta_{\text1} ≤ 0$

$H_1: \beta_{\text1} > 0$

where $H_{\text1}$ represents an increase in towel reuse under the
Social Norm intervention.

``` r
test_m1 <-hypothesis(
  fit_m1,
  "GroupSocialNorm > 0"
)
test_m1$hypothesis
```

                 Hypothesis Estimate  Est.Error   CI.Lower  CI.Upper Evid.Ratio
    1 (GroupSocialNorm) > 0 0.211336 0.07792375 0.08493409 0.3397426   332.3333
      Post.Prob Star
    1     0.997    *

The Bayesian analysis provided strong support for a positive
intervention effect. The estimated intervention coefficient was positive
$\beta_1$=0.21, 90% CrI \[0.08, 0.34\], and the posterior probability
that the intervention increased towel reuse was 99.7%.
