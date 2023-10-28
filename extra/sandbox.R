setup()
library(BNPGrangerTestR)
library(devtools)
library(pkgdown)

object <-
  example_dataset |>
  bnpgrangertest(
    iter = 10L,
    warmup = 5L,
    grid_npoints = 50L,
    hmax = 3L
  )

object |>
  summarize_pdf() |>
  head()

document(); build_site()
