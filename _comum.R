# Funções e dados compartilhados pelas páginas .qmd (carregado com source()).
# Lê só as saídas agregadas do script_integrado.R em dados/.

library(dplyr)
library(tidyr)
library(readr)
library(gt)

ler_dado <- function(arquivo) {
  caminho <- file.path("dados", arquivo)
  if (!file.exists(caminho)) return(NULL)
  readr::read_csv2(caminho, show_col_types = FALSE,
                   col_types = readr::cols(SE = "c", .default = readr::col_guess()))
}

serie       <- ler_dado("serie_semanal.csv")
correlacoes <- ler_dado("correlacoes.csv")
referencias <- ler_dado("referencias.csv")

formatar_se <- function(se) paste0(substr(se, 1, 2), "/", substr(se, 3, 6))
formatar_data <- function(x) format(as.Date(x), "%d/%m/%Y")
formatar_num <- function(x, casas = 1) {
  formatC(x, format = "f", digits = casas, decimal.mark = ",", big.mark = ".")
}

rotulos_variaveis <- c(
  temp_media       = "Temperatura média (°C)",
  temp_min         = "Temperatura mínima da semana (°C)",
  temp_max         = "Temperatura máxima da semana (°C)",
  umidade_media    = "Umidade relativa média (%)",
  precipitacao_mm  = "Precipitação na semana (mm)",
  pm2_5            = "MP2,5 médio (µg/m³)",
  pm10             = "MP10 médio (µg/m³)",
  ozonio           = "Ozônio médio (µg/m³)",
  co               = "CO médio (µg/m³)",
  sg_pct           = "% de SG nos atendimentos (sentinela)",
  positividade_pct = "Positividade viral das amostras (sentinela)"
)

rotulos_desfechos <- c(
  sg_pct           = "% de SG nos atendimentos (sentinela)",
  positividade_pct = "Positividade viral das amostras (sentinela)",
  srag_taxa_100mil = "Internações por SRAG por 100 mil hab."
)

estilo_tabela <- function(tabela) {
  tabela |>
    gt::opt_row_striping() |>
    gt::tab_options(
      table.font.size = "14px",
      column_labels.background.color = "#0057A3",
      column_labels.font.weight = "bold",
      table.border.top.color = "#0057A3",
      table.border.bottom.color = "#0057A3",
      row_group.font.weight = "bold"
    )
}

aviso_sem_dado <- function() {
  gt::gt(data.frame(Aviso = "Dado ainda não gerado - rode o script_integrado.R."))
}

# Tabela de correlações: linhas = variável, colunas = defasagem, cor = rho
# (azul negativo, vermelho positivo), negrito = p < 0,05.
tabela_correlacao <- function(dados, rotulos, rotulo_coluna) {
  if (is.null(dados) || nrow(dados) == 0) return(aviso_sem_dado())

  base <- dados |>
    filter(variavel %in% names(rotulos)) |>
    mutate(
      Variável  = factor(rotulos[variavel], levels = rotulos),
      defasagem = rotulo_coluna(defasagem)
    ) |>
    arrange(Variável)

  rho_largo <- base |>
    select(Variável, defasagem, rho) |>
    pivot_wider(names_from = defasagem, values_from = rho)
  p_largo <- base |>
    select(Variável, defasagem, valor_p) |>
    pivot_wider(names_from = defasagem, values_from = valor_p)

  colunas <- setdiff(names(rho_largo), "Variável")

  tabela <- rho_largo |>
    mutate(Variável = as.character(Variável)) |>
    gt::gt(rowname_col = "Variável") |>
    gt::fmt_number(columns = all_of(colunas), decimals = 2, dec_mark = ",") |>
    gt::sub_missing(missing_text = "—") |>
    gt::cols_align(align = "center", columns = all_of(colunas)) |>
    gt::data_color(columns = all_of(colunas), method = "numeric",
                   palette = c("#1565c0", "#ffffff", "#c62828"),
                   domain = c(-1, 1), na_color = "#f5f5f5") |>
    estilo_tabela()

  for (coluna in colunas) {
    linhas <- which(!is.na(p_largo[[coluna]]) & p_largo[[coluna]] < 0.05)
    if (length(linhas) > 0) {
      tabela <- tabela |>
        gt::tab_style(style = gt::cell_text(weight = "bold"),
                      locations = gt::cells_body(columns = all_of(coluna), rows = linhas))
    }
  }
  tabela
}
