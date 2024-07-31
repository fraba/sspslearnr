# sspslearnr: Interactive Tutorials for Quantitative Social Science

This package contains
[`learnr`](https://rstudio.github.io/learnr/index.html) tutorials
forked from [mattblackwell/qsslearnr](https://github.com/mattblackwell/qsslearnr).

``` r
remotes::install_github("kosukeimai/qss-package", build_vignettes = TRUE)
remotes::install_github("rstudio/learnr")
remotes::install_github("rstudio-education/gradethis")
remotes::install_github("fraba/sspslearnr")
```

Then you can start the tutorials in one of two ways. First, in RStudio 1.3 or later, you will find the QSS tutorials listed in the "Tutorial" tab in the top-right pane (by default). Find a tutorial and click "Run Tutorial" to get started. Second, you can run any tutorial from the R console by typing the following line: 

``` r
learnr::run_tutorial("week-01", package = "sspslearnr")
```

This should bring up a tutorial in your default web browser. You can see the full list of tutorials by running:

``` r
learnr::run_tutorial(package = "sspslearnr")
```

## Note on the Tidyverse version of the tutorials

Althought there are 11 baseR tutorials in total, only 7 (00 - 06) of such tutorials are in tidyverse since all of the rest (07 - 10) contain mostly conceptual questions. 

## Submission Reports

At the end of each tutorial, students can download submission reports that describe what questions and exercises they attempted. Students can then upload these PDFs with their names to a learning management system like Gradescope or Canvas. 

## Other QSS tutorials

The material in these tutorials largely follows the original swirl course, [`qss-swirl`](https://github.com/kosukeimai/qss-swirl). 
