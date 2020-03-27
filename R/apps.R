#' Explorador dos Perfis de Fluxo de Caixa das UGs
#'
#' @param dados_com_indicadores
#'
#' @export
explorar <- function(dados_preparados_mas_sem_indicadores) {
  requireNamespace("shiny")
  requireNamespace("reactable")
  requireNamespace("highcharter")
  requireNamespace("shinyWidgets")
  requireNamespace("purrr")
  requireNamespace("magrittr")
  requireNamespace("xts")

  appDir <- system.file("apps/explorador", package = "empocamento")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `empocamento`.", call. = FALSE)
  }

  .GlobalEnv$dados_preparados_mas_sem_indicadores <- dados_preparados_mas_sem_indicadores
  on.exit(rm(dados_preparados_mas_sem_indicadores, envir = .GlobalEnv))

  shiny::runApp(paste0(appDir, "/app.R"))
}
