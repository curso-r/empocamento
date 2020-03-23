#' gera_series_diarias_para_os_tres_itens
#'
#' @param dados
#'
gera_series_diarias_para_os_tres_itens <- function(dados) {
  dados %>%
    dplyr::select(
      NO_ORGAO_MAXI,
      NO_ITEM_INFORMACAO,
      ID_ANO_LANC,
      NO_DIA_COMPLETO,
      NO_UG,
      NO_ORGAO,
      NO_FONTE_RECURSO,
      SALDORITEMINFORMAODIALANAMENT
    ) %>%
    dplyr::mutate(
      flag_saldo_anual = stringr::str_detect(NO_DIA_COMPLETO , "-09/00/"),
      NO_DIA_COMPLETO_dmy = lubridate::dmy(dplyr::if_else(!flag_saldo_anual, NO_DIA_COMPLETO, paste0("01/01/", ID_ANO_LANC))),
      NO_ITEM_INFORMACAO = dplyr::case_when(
        NO_ITEM_INFORMACAO %in% "LIMITES DE SAQUE (OFSS, DIVIDA, BACEN E PREV)" ~ "saldo_diario",
        NO_ITEM_INFORMACAO %in% "VALORES LIQUIDADOS A PAGAR (EXERCICIO + RP)" ~ "obrigacoes_a_pagar_diario",
        NO_ITEM_INFORMACAO %in% "PAGAMENTOS TOTAIS (EXERCICIO E RAP)" ~ "pagamento_diario"
      )
    ) %>%
    dplyr::group_by(
      NO_ORGAO_MAXI,
      NO_ITEM_INFORMACAO,
      NO_UG,
      NO_ORGAO,
      NO_FONTE_RECURSO
    ) %>%
    dplyr::filter(
      # retira as datas com -09/00/YYYY se o YYYY for maior do que o ano da data mais antiga daquela NO_UG/NO_FONTE
      (NO_DIA_COMPLETO_dmy %in% min(NO_DIA_COMPLETO_dmy, na.rm = TRUE)) | !flag_saldo_anual
    ) %>%
    dplyr::arrange(NO_DIA_COMPLETO_dmy) %>%
    dplyr::mutate(
      SALDORITEMINFORMAODIALANAMENT_acumulado = cumsum(SALDORITEMINFORMAODIALANAMENT)
    ) %>%
    padr::pad(group = c("NO_ORGAO_MAXI", "NO_ITEM_INFORMACAO", "NO_UG", "NO_ORGAO", "NO_FONTE_RECURSO"), by = "NO_DIA_COMPLETO_dmy", break_above = 10, interval = "day") %>%
    tidyr::fill(SALDORITEMINFORMAODIALANAMENT_acumulado) %>%
    dplyr::mutate(
      paded = !is.na(SALDORITEMINFORMAODIALANAMENT),
      ID_ANO_LANC = lubridate::year(NO_DIA_COMPLETO_dmy),
      SALDORITEMINFORMAODIALANAMENT = dplyr::coalesce(SALDORITEMINFORMAODIALANAMENT, 0),
      flag_saldo_anual = dplyr::coalesce(flag_saldo_anual, FALSE),
      NO_DIA_COMPLETO = dplyr::if_else(is.na(NO_DIA_COMPLETO), format(NO_DIA_COMPLETO_dmy, "%d/%m/%Y"), NO_DIA_COMPLETO)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::nest(dados = c(-NO_ITEM_INFORMACAO)) %>%
    dplyr::mutate(
      dados = dados %>% magrittr::set_names(NO_ITEM_INFORMACAO)
    )
}



#' gera_disponibilidades_liquidas_diarias
#'
#' @param lista_dos_tres_itens
#'
gera_disponibilidades_liquidas_diarias <- function(lista_dos_tres_itens) {

  browser()
  lista_dos_tres_itens$dados$saldo_diario %>%
    dplyr::rename(
      saldo_diario = SALDORITEMINFORMAODIALANAMENT,
      saldo_diario_acumulado = SALDORITEMINFORMAODIALANAMENT_acumulado
    ) %>%
    dplyr::left_join(
      lista_dos_tres_itens$dados$obrigacoes_a_pagar_diario %>%
        dplyr::select(
          ID_ANO_LANC,
          NO_DIA_COMPLETO,
          NO_UG, NO_ORGAO,
          NO_FONTE_RECURSO,
          NO_DIA_COMPLETO_dmy,
          obrigacoes_a_pagar_diario = SALDORITEMINFORMAODIALANAMENT,
          obrigacoes_a_pagar_diario_acumulado = SALDORITEMINFORMAODIALANAMENT_acumulado
        ),
      by = c("ID_ANO_LANC", "NO_DIA_COMPLETO", "NO_UG", "NO_ORGAO", "NO_FONTE_RECURSO", "NO_DIA_COMPLETO_dmy")
    ) %>%
    dplyr::group_by(NO_UG, NO_ORGAO, NO_FONTE_RECURSO) %>%
    dplyr::arrange(NO_DIA_COMPLETO_dmy) %>%
    tidyr::fill(obrigacoes_a_pagar_diario_acumulado) %>%
    dplyr::mutate(
      obrigacoes_a_pagar_diario_acumulado = dplyr::coalesce(obrigacoes_a_pagar_diario_acumulado, 0),
      obrigacoes_a_pagar_diario = dplyr::coalesce(obrigacoes_a_pagar_diario, 0)
    ) %>%
    dplyr::left_join(
      lista_dos_tres_itens$dados$pagamento_diario %>%
        dplyr::select(
          ID_ANO_LANC,
          NO_DIA_COMPLETO,
          NO_UG, NO_ORGAO,
          NO_FONTE_RECURSO,
          NO_DIA_COMPLETO_dmy,
          pagamento_diario = SALDORITEMINFORMAODIALANAMENT,
          pagamento_diario_acumulado = SALDORITEMINFORMAODIALANAMENT_acumulado
        ),
      by = c("ID_ANO_LANC", "NO_DIA_COMPLETO", "NO_UG", "NO_ORGAO", "NO_FONTE_RECURSO", "NO_DIA_COMPLETO_dmy")
    ) %>%
    dplyr::group_by(NO_UG, NO_ORGAO, NO_FONTE_RECURSO) %>%
    dplyr::arrange(NO_DIA_COMPLETO_dmy) %>%
    tidyr::fill(pagamento_diario_acumulado) %>%
    dplyr::mutate(
      pagamento_diario_acumulado = dplyr::coalesce(pagamento_diario_acumulado, 0),
      pagamento_diario = dplyr::coalesce(pagamento_diario, 0)
    ) %>%
    dplyr::mutate(
      disponibilidade_liquida = saldo_diario_acumulado - obrigacoes_a_pagar_diario_acumulado
    )
}

#' gera_tabela_aninhada
#'
#' @param dados_com_disponibilidade_liquida
#'
gera_tabela_aninhada <- function(dados_com_disponibilidade_liquida) {
  dados_com_disponibilidade_liquida %>%
    dplyr::group_by(
      NO_ORGAO_MAXI,
      NO_UG,
      NO_ORGAO,
      NO_FONTE_RECURSO
    ) %>%
    tidyr::nest_legacy(.key = "series_temporais")
}

#' gera_id
#'
#' @param dados_aninhados
#'
gera_id <- function(dados_aninhados) {
  dados_aninhados %>%
    dplyr::mutate(
      id = paste0(NO_ORGAO, NO_UG, NO_FONTE_RECURSO),
      id = purrr::map_chr(id, digest::sha1)
    ) %>%
    dplyr::select(id, dplyr::everything())
}

#' Prepara Perfis Históricos de Fluxo de Caixa
#'
#' @param dados_limsaque_pagto_obrig data.frame extraído do SIAF com os três itens de limite de saque, obricações a pagar e pagamentos.
#'
#' @return data.frame com 5 colunas de identificação e 1 coluna de data.frames aninhados:
#'
#' \item{id}{}
#' \item{NO_ORGAO_MAXI}{}
#' \item{NO_UG}{}
#' \item{NO_ORGAO}{}
#' \item{NO_FONTE_RECURSO}{}
#' \item{series_temporais}{}
#'
#' data.frame aninhado em series_temporais:
#'
#' \item{ID_ANO_LANC}{}
#' \item{NO_DIA_COMPLETO}{}
#' \item{saldo_diario}{}
#' \item{flag_saldo_anual}{}
#' \item{NO_DIA_COMPLETO_dmy}{}
#' \item{saldo_diario_acumulado}{}
#' \item{paded}{}
#' \item{obrigacoes_a_pagar_diario}{}
#' \item{obrigacoes_a_pagar_diario_acumulado}{}
#' \item{pagamento_diario}{}
#' \item{pagamento_diario_acumulado}{}
#' \item{disponibilidade_liquida}{}
#' @export
#'
#' @examples
#' library(empocamento)
#' series_mj <- prepara(movimentacoes_diarias_mj)
prepara <- function(dados_limsaque_pagto_obrig) {
  dados_limsaque_pagto_obrig %>%
    gera_series_diarias_para_os_tres_itens() %>%
    gera_disponibilidades_liquidas_diarias() %>%
    gera_tabela_aninhada() %>%
    gera_id()
}


#' Filtra Séries Temporais Por Período
#'
#' @param dados_preparados data.frame com os dados já preparados pela função `preparar()`.
#' @param periodo vetor de tamanho 2 com o início e o fim do período.
#'
#' @export
#'
#' @example
#' series_mj <- empocamento::movimentacoes_diarias_mj %>%
#'   prepara() %>%
#'   filtra_series_temporais_por_periodo(c("2019-01-01", "2019-11-01"))
filtra_series_temporais_por_periodo <- function(dados_preparados, periodo) {
  if(class(periodo) %in% "character") periodo <- as.Date(periodo, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y", "%d/%m/%Y"))
  dados_preparados %>%
    dplyr::mutate(series_temporais = purrr::map(series_temporais, ~ {
      .x %>% dplyr::filter(NO_DIA_COMPLETO_dmy %>% between(periodo[1], periodo[2]))
    }))
}
