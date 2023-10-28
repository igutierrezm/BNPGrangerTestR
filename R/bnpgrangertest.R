#' Perform the Granger causality test described in Gutiérrez et al. (2023).
#'
#' @param Y A \eqn{T \times N} response matrix with \eqn{T} periods.
#' @param p The number of lags in the model.
#' @param z0 The hyperparameter \eqn{\zeta_{0}}.
#' @param q0 The hyperparameter \eqn{q_{0} := \sigma_{0}^{2}}.
#' @param v0 The hyperparameter \eqn{v_{0}}.
#' @param S0 The hyperparameter \eqn{S_{0}}.
#' @param a0p The hyperparameter \eqn{a_{0p}}.
#' @param b0p The hyperparameter \eqn{b_{0p}}.
#' @param a0s The hyperparameter \eqn{a_{0s}}.
#' @param b0s The hyperparameter \eqn{b_{0s}}.
#' @param thin The period for saving samples.
#' @param iter The total number of mcmc iterations.
#' @param warmup The number of warmup mcmc iterations.
#' @param seed The seed for random number generation.
#' @param hmax The maximum horizon for computing
#' both the IRFs and the posterior predictive pdfs.
#' @param grid_npoints The number of points y0 for computing each posterior
#' predictive pdf. The final grid for the ith variable is conformed by
#' `grid_npoints` equispaced points from \code{grid_lbs[i]} to
#' \code{grid_ubs[i]},  see the arguments \code{grid_lb} and \code{grid_ub}.
#' @param grid_lbs A vector of lower bounds, see \code{grid_npoints}.
#' @param grid_ubs A vector of upper bounds, see \code{grid_npoints}.
#' @return An object of class \code{bnpgrangertest}.
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
    grid_lbs = apply(Y, 2, \(x) mean(x) - 3 * stats::sd(x)),
    grid_ubs = apply(Y, 2, \(x) mean(x) + 3 * stats::sd(x))
) {
  # Import some julia pkgs
  BV <- JuliaConnectoR::juliaImport("BNPVAR")
  RD <- JuliaConnectoR::juliaImport("Random")

  # Set a RNG seed
  RD$`seed!`(seed)

  # Fit the model
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

  # Get the varnames
  varnames <- colnames(Y)
  if (is.null(varnames)) varnames <- paste0("y", 1:ncol(Y))
  tbl_var <- dplyr::tibble(var_id = 1:length(varnames), var = varnames)

  # Return a list of chains and the varnames
  out <-
    list(
      gamma = out_jl$gamma |> dplyr::as_tibble(),
      irf = out_jl$irf |> dplyr::as_tibble(),
      pdf = out_jl$pdf |> dplyr::as_tibble(),
      var = tbl_var
    ) |>
    magrittr::set_class("bnpgrangertest")
  return(out)
}

#' Plot the posterior distribution of gamma as a tile plot.
#'
#' @param object An instance of class \code{bnpgrangertest}.
#' @return A \code{ggplot} object that can be further
#' customized with the \code{ggplot2} package.
#' @importFrom rlang := .data
#' @export
plot_gamma <- function(object) {
  out <-
    object |>
    summarize_gamma() |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["cause_var"]],
        y = .data[["effect_var"]],
        label = round(.data[["prob"]], 3),
        fill = .data[["prob"]]
      )
    ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
      ggplot2::aes(
        color = ifelse(.data[["prob"]] > mean(.data[["prob"]]), "1", "2")
      )
    ) +
    ggplot2::scale_fill_distiller(
      type = "seq",
      direction = 1,
      palette = "Greys"
    ) +
    ggplot2::scale_colour_manual(values = c("white", "black")) +
    ggplot2::theme_classic() +
    ggplot2::guides(color = "none") +
    ggplot2::labs(
      x = "causing variable",
      y = "affected variable"
    )
  return(out)
}

#' Plot the posterior distribution of the IRFs as a collection of line plots.
#'
#' @param object An instance of class \code{bnpgrangertest}.
#' @return A \code{ggplot} object that can be further
#' customized with the \code{ggplot2} package.
#' @importFrom rlang := .data
#' @export
plot_irf <- function(object) {
  out <-
    object |>
    summarize_irf() |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["horizon"]],
        y = .data[["irf"]],
        ymin = .data[["lb"]],
        ymax = .data[["ub"]],
      )
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_ribbon(alpha = 0.1) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(.data[["cause_var"]]),
      rows = ggplot2::vars(.data[["effect_var"]])
    ) +
    ggplot2::scale_x_discrete(
      limits =
        summarize_irf(object)[["horizon"]] |>
        (\(x) seq(1, max(x), by = 1))() |>
        factor()
    ) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "horizon by causing variable",
      y = "irf by affected variable"
    )
  return(out)
}

#' Plot the posterior predictive pdf as a collection of density plots.
#'
#' @param object An instance of class \code{bnpgrangertest}.
#' @return A \code{ggplot} object that can be further
#' customized with the \code{ggplot2} package.
#' @importFrom rlang := .data
#' @export
plot_pdf <- function(object) {
  out <-
    object |>
    summarize_pdf() |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["y"]],
        y = .data[["pdf"]]
      )
    ) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(
      cols = ggplot2::vars(.data[["horizon"]]),
      rows = ggplot2::vars(.data[["var"]])
    ) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "y by horizon",
      y = "pdf by variable"
    )
  return(out)
}

#' Summarize the posterior distribution of \eqn{\gamma}.
#'
#' @param object An instance of class \code{bnpgrangertest}.
#' @return A tibble with the posterior probability of \eqn{\gamma},
#' and the following variables:
#' \itemize{
#'   \item \code{cause_var}: the causing variable in the relationship.
#'   \item \code{effect_var}: the affected variable in the relationship.
#'   \item \code{prob}: The posterior probability of the causal relationship.
#' }
#' @importFrom rlang := .data
#' @export
summarize_gamma <- function(object) {
  out <-
    object |>
    extract_chain_gamma() |>
    dplyr::summarise(
      prob = mean(.data[["value"]]),
      .by = c("cause_var", "effect_var") |> dplyr::any_of()
    )
  return(out)
}

#' Summarize the posterior distribution of the IRFs.
#'
#' @param object An instance of class \code{bnpgrangertest}.
#' @param qlb The quantile used for computing the lower bound of the CI.
#' @param qub The quantile used for computing the lower bound of the CI.
#' @return A tibble with the posterior mean of each requested IRF,
#' and the following variables:
#' \itemize{
#'   \item \code{horizon}: the IRF horizon.
#'   \item `cause_id`: the causing variable in the relationship.
#'   \item `effect_id`: the affected variable in the relationship.
#'   \item `irf`: The posterior mean of the IRF.
#' }
#' @importFrom rlang := .data
#' @export
summarize_irf <- function(object, qlb = 0.1, qub = 0.9) {
  out <-
    object |>
    extract_chain_irf() |>
    dplyr::summarise(
      irf = mean(.data[["value"]]),
      lb = stats::quantile(.data[["value"]], qlb),
      ub = stats::quantile(.data[["value"]], qub),
      .by = c("horizon", "cause_var", "effect_var") |> dplyr::any_of()
    )
  return(out)
}

#' Summarize the posterior predictive distribution.
#'
#' @param object An instance of class \code{bnpgrangertest}.
#' @return A tibble with each posterior predictive pdf,
#' and the following variables:
#' \itemize{
#'   \item \code{horizon}: the IRF horizon.
#'   \item \code{cause_var}: the causing variable in the relationship.
#'   \item \code{effect_var}: the affected variable in the relationship.
#'   \item \code{y}: the grid point.
#'   \item \code{pdf}: The posterior predictive pdf.
#' }
#' @importFrom rlang := .data
#' @export
summarize_pdf <- function(object) {
  out <-
    object |>
    extract_chain_pdf() |>
    dplyr::summarise(
      pdf = mean(.data[["value"]]),
      .by = c("horizon", "var", "y") |> dplyr::any_of()
    )
  return(out)
}

#' Extract the full MCMC chain associated to \eqn{\gamma}.
#'
#' @param object An instance of class \code{bnpgrangertest}.
#' @return A tibble with the chain of \eqn{\gamma} and the following variables:
#' \itemize{
#'   \item \code{iter}: The MCMC iteration.
#'   \item \code{cause_var}: The causing variable in the relationship.
#'   \item \code{effect_var}: The affected variable in the relationship.
#'   \item \code{value}: The value of the target variable in the current iteration.
#' }
#' @importFrom rlang := .data
#' @export
extract_chain_gamma <- function(object) {
  out <-
    object$gamma |>
    dplyr::inner_join(
      extract_tbl_var(object),
      by = c("cause_id" = "var_id")
    ) |>
    dplyr::inner_join(
      extract_tbl_var(object),
      by = c("effect_id" = "var_id")
    ) |>
    dplyr::rename(
      cause_var = .data[["var.x"]],
      effect_var = .data[["var.y"]]
    ) |>
    dplyr::select(
      c("iter", "cause_var", "effect_var", "value") |> dplyr::any_of()
    )
  return(out)
}

#' Extract the full MCMC chain associated to the IRFs.
#'
#' @param object An instance of class \code{bnpgrangertest}.
#' @return A tibble with the chain of the IRFs and the following variables:
#' \itemize{
#'   \item \code{iter}: The MCMC iteration.
#'   \item \code{horizon}: The IRF horizon.
#'   \item \code{cause_var}: The causing variable in the relationship.
#'   \item \code{effect_var}: The affected variable in the relationship.
#'   \item \code{value}: The value of the target variable in the current iteration.
#' }
#' @importFrom rlang := .data
#' @export
extract_chain_irf <- function(object) {
  out <-
    object$irf |>
    dplyr::inner_join(
    extract_tbl_var(object),
    by = c("cause_id" = "var_id")
  ) |>
    dplyr::inner_join(
      extract_tbl_var(object),
      by = c("effect_id" = "var_id")
    ) |>
    dplyr::rename(
      cause_var = .data[["var.x"]],
      effect_var = .data[["var.y"]]
    ) |>
    dplyr::select(
      c("iter", "horizon", "cause_var", "effect_var", "value") |>
        dplyr::all_of()
    )
  return(out)
}

#' Extract the full MCMC chain associated to the (1d) posterior predictive pdfs.
#'
#' @param object An instance of class \code{bnpgrangertest}.
#' @return A tibble with the chain of the (1d) posterior predictive pdfs
#' and the following variables:
#' \itemize{
#'   \item \code{iter}: The MCMC iteration.
#'   \item \code{horizon}: The IRF horizon.
#'   \item \code{var}: The variable for which the predictive pdf is computed.
#'   \item \code{value}: The value of the target variable in the current iteration.
#' }
#' @importFrom rlang := .data
#' @export
extract_chain_pdf <- function(object) {
  out <-
    object$pdf |>
    dplyr::inner_join(
      extract_tbl_var(object)
    ) |>
    dplyr::select(
      c("iter", "horizon", "var", "y", "value") |>
        dplyr::all_of()
    )
  return(out)
}

extract_tbl_var <- function(object) {
  return(object$var)
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
