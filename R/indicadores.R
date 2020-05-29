
#' calc_disponibilidade_estritamente_crescente
calc_disponibilidade_estritamente_crescente <- function(disponibilidade_liquida, NO_DIA_COMPLETO_dmy) {
  dias_no_periodo = length(NO_DIA_COMPLETO_dmy)
  proporcao_de_disponibilidade_liquida_negativa <- mean(disponibilidade_liquida < 0)
  disponibilidade_mais_recente <- disponibilidade_liquida[which.max(NO_DIA_COMPLETO_dmy)]
  p1 <- mean(diff(disponibilidade_liquida)  > 0)
  p2 <- mean(abs(diff(disponibilidade_liquida)[diff(disponibilidade_liquida) < 0]) < (sd(disponibilidade_liquida) + 0.001)/100)
  resp <- ifelse(is.nan(p1), 0, p1) + ifelse(is.nan(p2), 0, p2)
  resp <- resp * 365/(dias_no_periodo)
  resp
}

#' calc_integral_sobre_media_dos_gastos
calc_integral_sobre_media_dos_gastos <- function(disponibilidade_liquida, pagamento_diario) {
  dias_no_periodo = length(disponibilidade_liquida)
  integral <- mean(disponibilidade_liquida)
  soma_dos_gastos <- sum(pagamento_diario)
  soma_dos_gastos <- ifelse(abs(soma_dos_gastos) < 1, 1, soma_dos_gastos)
  resp <- integral/soma_dos_gastos
  resp <- resp * 365/length(dias_no_periodo)/1e8
  resp
}

#' calc_valor_nominal
calc_valor_nominal <- function(disponibilidade_liquida) {
  mean(disponibilidade_liquida)
}

#' calc_valor_nominal_conservador
calc_valor_nominal_conservador <- function(disponibilidade_liquida, pagamentos_diarios) {
  mean(disponibilidade_liquida) - mean(pagamentos_diarios)*30
}

#' calc_tempo
calc_tempo <- function(disponibilidades_liquida) {
  sum(disponibilidades_liquida > 0)/length(disponibilidades_liquida)
}

#' calc_ipdl
calc_ipdl <- function(disponibilidade_liquida, lag_disponibilidade_liquida) {

  disponibilidade_liquida <- trunc(disponibilidade_liquida)
  dif <- disponibilidade_liquida - lag_disponibilidade_liquida
  # dif < 0 significa débito.
  # sempre vai ter pelo menos 1 NA.
  debitos <- mean(abs(dif[dif < 0]), na.rm = TRUE)
  debitos <- ifelse(is.nan(debitos) || debitos < 0, 0, debitos)

  mean(disponibilidade_liquida -  debitos*0.5 > 0)
}

#' calc_iadl
calc_iadl <- function(disponibilidade_liquida, order_by) {
  dias_no_periodo = length(disponibilidade_liquida)
  lag_disponibilidade_liquida <- dplyr::lag(disponibilidade_liquida, 1, order_by = order_by)
  disp_positiva <- disponibilidade_liquida[disponibilidade_liquida>0]

  if (length(disp_positiva) == 0)
    disp_positiva <- 0

  disp_positiva_media <- mean(disp_positiva)

  dif <- disponibilidade_liquida - lag_disponibilidade_liquida
  # dif < 0 significa débito.
  # sempre vai ter pelo menos 1 NA.
  debitos <- sum(abs(dif[dif < 0]), na.rm = TRUE)
  # numero menor que 1 fica ruim pq pode explodir tudo
  debitos <- ifelse(debitos < 1, 1, debitos)

  resp <- disp_positiva_media/debitos
  resp <- resp * 365/length(dias_no_periodo)/10e5
  resp
}

#' calcula_descritores
calcula_descritores <- function(series_temporais) {
  series_temporais %>%
    dplyr::summarise(
      n = dplyr::n(),
      integral_sobre_media_dos_gastos = calc_integral_sobre_media_dos_gastos(
        disponibilidade_liquida = disponibilidade_liquida,
        pagamento_diario = pagamento_diario
      ),
      disponibilidade_estritamente_crescente = calc_disponibilidade_estritamente_crescente(
        disponibilidade_liquida = disponibilidade_liquida,
        NO_DIA_COMPLETO_dmy = NO_DIA_COMPLETO_dmy
      ),
      iadl = calc_iadl(disponibilidade_liquida, NO_DIA_COMPLETO_dmy),
      ipdl = calc_ipdl(disponibilidade_liquida, dplyr::lag(disponibilidade_liquida, default = 0)),
      valor_nominal = calc_valor_nominal(disponibilidade_liquida),
      valor_nominal_conservador = calc_valor_nominal_conservador(disponibilidade_liquida, pagamento_diario),
      indicador_tempo = calc_tempo(disponibilidade_liquida)
    )
}

#' Adiciona Descritores de Perfis de Fluxo de Caixa
#'
#' @param dados_preparados dados preparados pela função \code{\link{prepara}}.
#' @param n_dias integer. Número de dias de histórico para considerar no cálculo dos descritores. O padrão é 365 dias.
#'
#' @return
#' @export
#'
#' @examples
#' library(empocamento)
#' series_mj <- movimentacoes_diarias_mj %>%
#'   prepara() %>%
#'   adiciona_descritores()
adiciona_descritores <- function(dados_preparados, n_dias = 365) {
  requireNamespace("workflows")

  dados_preparados %>%
    dplyr::mutate(
      series_temporais_crop = purrr::map(series_temporais, ~ {
        .x %>% dplyr::filter(NO_DIA_COMPLETO_dmy >= (max(NO_DIA_COMPLETO_dmy) - lubridate::days(n_dias)))
      }),
      descritores = purrr::map(series_temporais_crop, calcula_descritores)
    ) %>%
    dplyr::select(-series_temporais_crop) %>%
    tidyr::unnest(descritores) %>%
    dplyr::mutate(
      suspeita_de_empocamento = workflows:::predict.workflow(modelo_suspeita_de_empocamento_v01, ., type = "prob")$.pred_Empoçamento
    )
}




