Nvar <- 2
Nobs <- 100
Y <- rnorm(Nvar * Nobs) |> matrix(ncol = Nvar)
for (t in 2:Nobs) {
  Y[t, 1] <- 0.5 * Y[t - 1, 1] + 0.5 * Y[t - 1, 2] + rnorm(1)
  Y[t, 2] <- 0.2 * Y[t - 1, 2] + rnorm(1)
}
example_dataset <- Y |> apply(2, \(x) (x - mean(x)) / sd(x))
usethis::use_data(example_dataset, overwrite = TRUE)
