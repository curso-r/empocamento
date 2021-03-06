% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataprep.R
\name{filtra_series_temporais_por_periodo}
\alias{filtra_series_temporais_por_periodo}
\title{Filtra Séries Temporais Por Período}
\usage{
filtra_series_temporais_por_periodo(dados_preparados, periodo)
}
\arguments{
\item{dados_preparados}{data.frame com os dados já preparados pela função `preparar()`.}

\item{periodo}{vetor de tamanho 2 com o início e o fim do período.}
}
\description{
Filtra Séries Temporais Por Período
}
\examples{
library(empocamento)
series_mj <- movimentacoes_diarias_mj \%>\%
  prepara() \%>\%
  filtra_series_temporais_por_periodo(c("2019-01-01", "2019-11-01"))
}
