setup()

{
  Nvar <- 3
  Nobs <- 100
  Y <- rnorm(Nobs * Nvar) |> matrix(ncol = Nvar)
  out <- bnpgrangertest(Y, iter = 2L, warmup = 1L)
}
