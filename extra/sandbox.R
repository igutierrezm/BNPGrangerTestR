setup()

{
  Nvar <- 3
  Nobs <- 100
  Y <- rnorm(Nobs * Nvar) |> matrix(ncol = Nvar)
  out <- bnpgrangertest(Y, iter = 11L, warmup = 1L)
  summarize_gamma(out)
}
