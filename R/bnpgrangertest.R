#' Perform the Granger causality test described in Gutiérrez et al. (2023).
#'
#' @param Y A \eqn{T \times N} response matrix with \eqn{T} periods.
#' @param p The number of lags in the model (defaul = 1L).
#' @param z0 The hyperparameter \eqn{\zeta_{0}} (default 1.0).
#' @param q0 The hyperparameter \eqn{q_{0} := \sigma_{0}^{2}} (default 1.0).
#' @param v0 The hyperparameter \eqn{v_{0}} (default 1.0).
#' @param S0 The hyperparameter \eqn{S_{0}} (default \eqn{I_{N}}).
#' @param a0p The hyperparameter \eqn{a_{0p}} (default 1.0).
#' @param b0p The hyperparameter \eqn{b_{0p}} (default 1.0).
#' @param a0s The hyperparameter \eqn{a_{0s}} (default 8.0).
#' @param b0s The hyperparameter \eqn{b_{0s}} (default 4.0).
#' @param thin The period for saving samples (default 1L).
#' @param iter The total number of mcmc iterations (default 4000L).
#' @param warmup The number of warmup mcmc iterations (default 2000L).
#' @param seed The seed for random number generation (default 1L).
#' @param hmax The maximum horizon for computing
#' both the irfs and the posterior predictive pdfs.
#' @param grid_npoints The number of points y0 for computing each posterior
#' predictive pdf. The final grid for the ith variable is conformed by
#' `grid_npoints` equispaced points from `grid_lb[i]` to `grid_ub[i]`,
#' see the arguments `grid_lb` and `grid_ub`.
#' @param grid_lbs A vector of lower bounds, see `grid_npoints`.
#' @param grid_ubs A vector of upper bounds, see `grid_npoints`.
#' @return An object of class 'bnpgrangertest'.
#' @seealso
#' [summarize_gamma()] for a summary of the \eqn{\gamma}'s,
#' [summarize_pdf()] for a summary of the posterior predictive pdf's.
#' [summarize_irf()] for a summary of the IRFs.
#' @export
bnpgrangertest <- function(
    Y,
    p = 1L,
    z0 = 1.0,
    q0 = 1.0,
    a0p = 1.0,
    b0p = 1.0,
    a0s = 8.0,
    b0s = 4.0,
    v0 = ncol(Y) + 1L,
    S0 = diag(ncol(Y)),
    warmup = 2000L,
    iter = 4000L,
    thin = 1L,
    hmax = 1L,
    seed = 1L,
    grid_npoints = 50L,
    grid_lbs = apply(Y, 2, min),
    grid_ubs = apply(Y, 2, max)
) {
  BV <- JuliaConnectoR::juliaImport("BNPVAR")
  RD <- JuliaConnectoR::juliaImport("Random")
  RD$`seed!`(seed)
  out_jl <-
    BV$fit(
      Y,
      p = p,
      z0 = z0,
      q0 = q0,
      v0 = v0,
      S0 = S0,
      a0p = a0p,
      b0p = b0p,
      a0s = a0s,
      b0s = b0s,
      warmup = warmup,
      iter = iter,
      thin = thin,
      hmax = hmax,
      grid_npoints = grid_npoints,
      grid_lbs = grid_lbs,
      grid_ubs = grid_ubs
    )
  out <-
    list(
      gamma = out_jl$gamma |> dplyr::as_tibble(),
      irf = out_jl$irf |> dplyr::as_tibble(),
      pdf = out_jl$pdf |> dplyr::as_tibble()
    ) |>
    magrittr::set_class("bnpgrangertest")
  return(out)
}

#' Summarize the posterior distribution of gamma.
#'
#' @param object An instance of class `bnpgrangertest`.
#' @return A tibble with the posterior probability of each hypothesis
#' (one per row), and the following variables:
#' \itemize{
#'   \item `cause_id`: the causing variable in the relationship.
#'   \item `effect_id`: the affected variable in the relationship.
#'   \item `prob`: The posterior probability of the causal relationship.
#' }
#' @importFrom rlang := .data
#' @export
summarize_gamma <- function(object) {
  out <-
    object$gamma |>
    dplyr::summarise(
      prob = mean(.data[["value"]]),
      .by = c("cause_id", "effect_id") |> dplyr::any_of()
    )
  return(out)
}

#' Summarize the posterior distribution of the IRF.
#'
#' @param object An instance of class `bnpgrangertest`.
#' @return A tibble with the posterior mean of each requested IRF,
#' and the following variables:
#' \itemize{
#'   \item `horizon`: the IRF horizon.
#'   \item `cause_id`: the causing variable in the relationship.
#'   \item `effect_id`: the affected variable in the relationship.
#'   \item `irf`: The posterior mean of the IRF.
#' }
#' @importFrom rlang := .data
#' @export
summarize_irf <- function(object) {
  out <-
    object$irf |>
    dplyr::summarise(
      irf = mean(.data[["value"]]),
      .by = c("horizon", "cause_id", "effect_id") |> dplyr::any_of()
    )
  return(out)
}

#' Summarize the posterior predictive distribution.
#'
#' @param object An instance of class `bnpgrangertest`.
#' @return A tibble with each posterior predictive pdf,
#' and the following variables:
#' \itemize{
#'   \item `horizon`: the IRF horizon.
#'   \item `cause_id`: the causing variable in the relationship.
#'   \item `effect_id`: the affected variable in the relationship.
#'   \item `y`: the grid point.
#'   \item `pdf`: The posterior predictive pdf.
#' }
#' @importFrom rlang := .data
#' @export
summarize_pdf <- function(object) {
  out <-
    object$pdf |>
    dplyr::summarise(
      pdf = mean(.data[["value"]]),
      .by = c("horizon", "var_id", "y") |> dplyr::any_of()
    )
  return(out)
}

extract_chain_gamma <- function(x) {
  return(x$gamma)
}

extract_chain_irf <- function(x) {
  return(x$irf)
}

extract_chain_pdf <- function(x) {
  return(x$pdf)
}

#' Check Julia setup.
#'
#' Check that Julia (v1.0.0+) can be started and install  ANOVADDPTest.jl.
#' For more information about the setup and discovery of Julia, see
#' the JuliaConnectoR's package documentation, section "Setup".
#' @export
setup <- function() {
  message("Checking that Julia (version >= 1.0) can be started...")
  if (!JuliaConnectoR::juliaSetupOk()) stop("Julia could not be started.")
  c("AbstractGSBPs.jl", "BayesVAR.jl", "BNPVAR.jl") |>
    lapply(function(x) {
      paste("Installing", x, "...") |> message()
      code <-
        'url = "https://github.com/igutierrezm/%s";' |>
        sprintf(x) |>
        paste0('import Pkg;') |>
        paste0('Pkg.add(url = url; io = devnull);') |>
        JuliaConnectoR::juliaEval() |>
        suppressMessages()
    })
  return(invisible(NULL))
}
