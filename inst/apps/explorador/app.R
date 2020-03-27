indicadores_disponiveis <- ""
dia_mais_recente <- dados_preparados_mas_sem_indicadores$series_temporais %>% purrr::map(~max(.x$NO_DIA_COMPLETO_dmy)) %>% reduce(max)
um_ano_atras <- dia_mais_recente - lubridate::days(365)

ui <- fluidPage(
  useShinydashboard(),
  tags$script(src = "logneg.js"),
  h1("Explorador dos Perfis de Fluxo de Caixa"),
  tabsetPanel(
    tabPanel(
      "Painel principal",
      fluidRow(
        column(2, offset = 6, shiny::dateRangeInput("periodo", "Período", start = um_ano_atras, end = dia_mais_recente,format = "dd/mm/yyyy")),
        column(
          width = 4,
          fluidRow(
            column(
              offset = 1,
              width = 4,
              shiny::selectInput(
                "indice_x",
                "Eixo X",
                choices = indicadores_disponiveis,
                selected = indicadores_disponiveis[1]
              )
            ),
            column(
              width = 1,
              checkboxInput("type_x", "log", value = FALSE)
            ),
            column(
              width = 4,
              shiny::selectInput(
                "indice_y",
                "Eixo Y",
                choices = indicadores_disponiveis,
                selected = indicadores_disponiveis[2]
              )
            ),
            column(
              width = 1,
              checkboxInput("type_y", "log", value = FALSE)
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 8,
          reactable::reactableOutput("tabela"),
          highchartOutput("st")
        ),
        column(
          width = 4,
          highchartOutput("dispersao"),
          reactable::reactableOutput("info")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  anotacoes <- reactiveVal()
  id_selecionado <- reactiveVal()
  atualizou <- reactiveVal(0)

  # se algo mudar, tudo tem que atualizar
  observe({
    input$selected_row
    atualizou(runif(1))
    input$dispersao_click$x
  })

  observeEvent(input$selected_row, {
    id <- dados() %>% slice(input$selected_row)
    id_selecionado(id$id)
  })

  observeEvent(input$dispersao_click, {
    id <- dados() %>%
      dplyr::mutate(
        x := !!rlang::sym(input$indice_x),
        y := !!rlang::sym(input$indice_y)
      ) %>%
      filter(near(x, input$dispersao_click$x, .Machine$double.eps^0.3), near(y, input$dispersao_click$y, .Machine$double.eps^0.3)) %>%
      slice(1)
    id_selecionado(id$id)
  })

  dados <- reactive({
    validate(
      need(input$periodo, "período não selecionado.")
    )

    dados_preparados_mas_sem_indicadores %>%
      filtra_series_temporais_por_periodo(periodo = input$periodo) %>%
      empocamento::adiciona_descritores()
  })

  observe({
    indicadores_disponiveis <- dados() %>% dplyr::select(-(id:n)) %>% names
    indicadores_disponiveis <- set_names(indicadores_disponiveis, str_replace_all(indicadores_disponiveis, "_", " "))
    shiny::updateSelectInput(session, "indice_x", choices = indicadores_disponiveis, selected = indicadores_disponiveis[1])
    shiny::updateSelectInput(session, "indice_y", choices = indicadores_disponiveis, selected = indicadores_disponiveis[2])
  })

  output$tabela <- reactable::renderReactable({
    aff <- atualizou()
    dados() %>%
      select(
        NO_UG,
        NO_ORGAO,
        NO_FONTE_RECURSO,
        integral_sobre_media_dos_gastos,
        disponibilidade_estritamente_crescente,
        iadl,
        valor_nominal,
        valor_nominal_conservador,
        indicador_tempo,
        suspeita_de_empocamento
      ) %>%
      reactable::reactable(
        selectionId = "selected_row",
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        onClick = "select",
        highlight = TRUE,
        compact = TRUE,
        selection = "single",
        filterable = TRUE,
        wrap = FALSE,
        defaultColDef = colDef(format = colFormat(digits = 3)),
        defaultPageSize = 10,
        rowStyle = reactable::JS("function(a, b) {if(a.row.NO_UG != '(Não rotulado)') return {backgroundColor: '#ccddff'}}"),
        columns = list(
          NO_UG = colDef(html = TRUE, align = "center")
        )
      )
  })

  output$st <- renderHighchart({
    validate(
      need(id_selecionado(), "linha não selecionada.")
    )
    selec <- dados() %>% filter(id %in% id_selecionado()) %$% series_temporais[[1]]
    disponibilidade_liquida <- selec %$% xts::xts(round(disponibilidade_liquida, 8), NO_DIA_COMPLETO_dmy)
    obrigacoes_a_pagar_diario <- selec %$% xts::xts(round(obrigacoes_a_pagar_diario, 8), NO_DIA_COMPLETO_dmy)
    pagamento_diario <- selec %$% xts::xts(round(pagamento_diario, 8), NO_DIA_COMPLETO_dmy)
    saldo_diario <- selec %$% xts::xts(round(saldo_diario, 8), NO_DIA_COMPLETO_dmy)

    highchart(type = "chart") %>%
      hc_xAxis(type = "datetime") %>%
      hc_add_series(disponibilidade_liquida, type = "area", name = "Disponibilidade Líquida") %>%
      hc_add_series(obrigacoes_a_pagar_diario, type = "line", name = "Obrigações a Pagar") %>%
      hc_add_series(pagamento_diario, type = "line", name = "Pagamentos") %>%
      hc_add_series(saldo_diario, type = "line", name = "Saldo Diário") %>%
      hc_plotOptions(area = list(fillOpaticy = 0.3))
  })

  output$dispersao <- renderHighchart({
    dados() %>%
      dplyr::mutate(
        x := !!rlang::sym(input$indice_x),
        y := !!rlang::sym(input$indice_y)
      ) %>%
      select(x, y, NO_UG, NO_FONTE_RECURSO) %>%
      mutate(x = round(x, 4), y = round(y, 4)) %>%
      highcharter::hchart(
        type = "scatter"
      ) %>%
      hc_add_event_point(event = "click") %>%
      hc_yAxis(type = ifelse(input$type_y, "logarithmic", "linear"),
               allowNegativeLog = TRUE) %>%
      hc_xAxis(type = ifelse(input$type_x, "logarithmic", "linear"),
               allowNegativeLog = TRUE) %>%
      hc_tooltip(
        headerFormat = '<span style="color:{point.color}">●</span> Clique para expandir<br/>',
        pointFormat = 'X <b>{point.x}</b><br/>Y <b>{point.y}</b><br/>UG <b>{point.NO_UG}</b><br/>FONTE <b>{point.NO_FONTE_RECURSO}</b>')
  })

  output$info <- reactable::renderReactable({
    validate(
      need(id_selecionado(), "linha não selecionada.")
    )
    aff <- atualizou()
    dados() %>%
      filter(id %in% id_selecionado()) %>%
      select(
        NO_UG,
        NO_ORGAO,
        NO_FONTE_RECURSO,
        integral_sobre_media_dos_gastos,
        disponibilidade_estritamente_crescente,
        iadl,
        valor_nominal,
        valor_nominal_conservador,
        indicador_tempo,
        suspeita_de_empocamento
      ) %>%
      gather("variável", "valor", everything()) %>%
      reactable::reactable(
        resizable = TRUE,
        showPageSizeOptions = FALSE,
        highlight = TRUE,
        compact = TRUE,
        wrap = FALSE,
        defaultColDef = colDef(format = colFormat(digits = 3)),
        defaultPageSize = 10,
        rowStyle = reactable::JS("function(a, b) {if(a.row.valor != '(Não rotulado)') return {backgroundColor: '#ccddff'}}")
      )
  })
}

shinyApp(ui, server)
