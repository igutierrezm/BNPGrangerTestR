#' Check Julia setup
#'
#' Checks that Julia (v1.0.0+) can be started and install  ANOVADDPTest.jl.
#' For more information about the setup and discovery of Julia, see
#' the JuliaConnectoR's package documentation, section "Setup".
#' @importFrom JuliaConnectoR juliaEval
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

# foo <- function(
#     Y,
#     p = 1L,
#     z0 = 1.0,
#     q0 = 1.0,
#     v0 = ncol(Y) + 1L,
#     S0 = diag(ncol(Y)),
#     warmup = 2000L,
#     iter = 4000L,
#     thin = 1L,
#     hmax = 1L,
#     ...
# ) {
#   BV <- JuliaConnectoR::juliaImport("BNPVAR")
#   out_jl <-
#     BV$fit(
#       Y,
#       p = p,
#       z0 = z0,
#       q0 = q0,
#       v0 = v0,
#       S0 = S0,
#       warmup = warmup,
#       iter = iter,
#       thin = thin,
#       hmax = hmax
#     )
#   out <-
#     list(
#       gamma = out_jl$gamma |> dplyr::as_tibble(),
#       irf = out_jl$irf |> dplyr::as_tibble()
#     )
#   return(out)
# }
