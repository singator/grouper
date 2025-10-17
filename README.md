
# grouper

<!-- badges: start -->
<!-- badges: end -->

The goal of grouper is to make it easier for an instructor to divide students
into groups.

## Installation

You can install the development version of grouper from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("singator/grouper")
```

But to be honest, the package on CRAN is already the most updated on. 
Please refer to the [vignettes](https://cran.r-project.org/package=grouper) for examples and documentation on the models in the package.

For best performance, it is best to get a license for [Gurobi optimiser](https://www.gurobi.com/downloads/).

## Using Gurobi on Linux

Something to take note: Rstudio does not seem to run the .bash_profile or .bashrc scripts. It may be necessary to set the 
environment variables explicitly in /etc/R/Renviron and /etc/R/Renviron.site

Shiny servers, on the other hand, seem to read environment variables from .bash_profile, so no extra modifications to Renviron needed.
When running on shiny servers, it is also necessary to copy the files into the app directory. It does not seem to work with the use of 
shiny::runApp().

## Contact

For more details, please contact Vik Gopal <vik.gopal@nus.edu.sg> or 
Kevin Lam <lamfy@u.nus.edu>.
