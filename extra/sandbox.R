setup()

{
  N <- 3
  T <- 100
  Y <- rnorm(N * T) |> matrix(ncol = N)
  out <- bnpgrangertest(Y, iter = 2L, warmup = 1L)
}
