# sc (Sykdomspulsen Core) <a href="https://docs.sykdomspulsen.no/sc"><img src="man/figures/logo.png" align="right" width="120" /></a>

## Overview 

[sc](https://docs.sykdomspulsen.no/sc) ("Sykdomspulsen Core") is a free and open-source framework for real-time analysis and disease surveillance.

Read the introduction vignette [here](https://docs.sykdomspulsen.no/sc/articles/sc.html) or run `help(package="sc")`.

## splverse

<a href="https://docs.sykdomspulsen.no/packages"><img src="https://docs.sykdomspulsen.no/packages/splverse.png" align="right" width="120" /></a>

The [splverse](https://docs.sykdomspulsen.no/packages) is a set of R packages developed to help solve problems that frequently occur when performing infectious disease surveillance.

If you want to install the dev versions (or access packages that haven't been released on CRAN), run `usethis::edit_r_profile()` to edit your `.Rprofile`. 

Then write in:

```
options(
  repos = structure(c(
    SPLVERSE  = "https://docs.sykdomspulsen.no/drat/",
    CRAN      = "https://cran.rstudio.com"
  ))
)
```

Save the file and restart R.

You can now install [splverse](https://docs.sykdomspulsen.no/packages) packages from our [drat repository](https://docs.sykdomspulsen.no/drat).

```
install.packages("attrib")
```
