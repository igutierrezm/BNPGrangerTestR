Nvar <- 2
Nobs <- 200
Y <- rnorm(Nvar * Nobs) |> matrix(ncol = Nvar)
for (t in 2:Nobs) {
  di <- runif(1) < 0.3
  ci <- ifelse(di, +0.5, -0.5)
  a11i <- ifelse(di, +0.7, -0.7)
  a22i <- ifelse(di, +0.7, -0.7)
  a21i <- ifelse(di, +0.5, -0.5)
  a12i <- 0.0
  Y[t, 1] <- 0.5 + a11i * Y[t - 1, 1] + a12i * Y[t - 1, 2] + sqrt(0.5) * rnorm(1)
  Y[t, 2] <- 0.5 + a21i * Y[t - 1, 1] + a22i * Y[t - 1, 2] + sqrt(0.5) * rnorm(1)
}

example_dataset <-
  Y |>
  apply(2, \(x) (x - mean(x)) / sd(x))

usethis::use_data(example_dataset, overwrite = TRUE)
