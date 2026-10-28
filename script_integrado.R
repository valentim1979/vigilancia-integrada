# ==========================================================================================
#  VIGILÂNCIA INTEGRADA DE VÍRUS RESPIRATÓRIOS x CLIMA - 15ª RS (MARINGÁ/PR)
# ==========================================================================================
#
# Junta, por semana epidemiológica (SE), três fontes:
#   1. Vigilância Sentinela de SG (2 UPAs: Zona Sul/Maringá e Sarandi)
#        - % de atendimentos por SG: Ficha de Agregado Semanal (Sivep-Gripe, .dbf)
#        - positividade viral das amostras (5/semana/unidade): indicador_12.csv,
#          publicado pelo repositório vigilancia-sentinela
#   2. Vigilância Universal de SRAG (internados), por município de RESIDÊNCIA:
#        - cache da API dados.gov.br baixado pelo repositório virus-respiratorios
#          (SRAG_API_<ano>.csv)
#   3. Clima e qualidade do ar (Open-Meteo), no centroide IBGE de cada município,
#      com média ponderada pela população em cada recorte.
#
# Recortes:
#   - "Maringá + Sarandi": SRAG de residentes dos 2 municípios das UPAs
#     sentinelas (o mais comparável com a sentinela).
#   - "15ª RS": SRAG de residentes dos 30 municípios.
#   Nos dois, a sentinela é a mesma (só existem as 2 UPAs).
#
# Saídas (só dados agregados - nenhuma linha por paciente):
#   dados/municipios_15rs.csv     códigos IBGE, população e centroides
#   dados/serie_semanal.csv       sentinela + SRAG + clima por recorte e SE
#   dados/virus_semanal.csv       vírus por SE na sentinela e no SRAG
#   dados/correlacoes.csv         correlações (Spearman) com defasagem
#   img/linha_tempo_*.png, img/virus_comparacao.png
#
# [Inferência] As correlações são EXPLORATÓRIAS (uma temporada, ~40 SE, sem
# controle de sazonalidade). Não servem para afirmar causa.
# ==========================================================================================


# ---- 0. PACOTES -------------------------------------------------------------------------

pacman::p_load(
  foreign,    # leitura de DBF
  httr,       # requisições às APIs
  jsonlite,
  readr,
  dplyr,
  tidyr,
  purrr,
  MMWRweek,   # semana epidemiológica
  ggplot2,
  here
)


# ---- 1. CONFIGURAÇÃO --------------------------------------------------------------------

readRenviron("~/.Renviron")

# Export da Ficha de Agregado Semanal da sentinela (mesmo usado no
# script_clima_casos.R do repositório vigilancia-sentinela).
arquivo_agregado <- Sys.getenv("CAMINHO_SG_AGREGADO_DBF")

# Pasta com os CSVs da API de SRAG (dbf_sivep/ do repositório virus-respiratorios).
#   Mac: CAMINHO_SRAG_CACHE=/Users/valentimsalajunior/Documents/vigilancia-epidemiologica/dbf_sivep
pasta_srag <- Sys.getenv("CAMINHO_SRAG_CACHE")

# Chave do Open-Meteo: OPCIONAL (vazia = API gratuita, uso não comercial).
chave_open_meteo <- Sys.getenv("OPEN_METEO_API_KEY")

faltando <- c(CAMINHO_SG_AGREGADO_DBF = arquivo_agregado,
              CAMINHO_SRAG_CACHE      = pasta_srag)
faltando <- names(faltando)[faltando == ""]
if (length(faltando) > 0) {
  stop("Defina no ~/.Renviron: ", paste(faltando, collapse = ", "),
       " (veja os comentarios no topo da secao 1).")
}

ANO_ANALISE <- 2026

# Positividade viral da sentinela (agregado por unidade/SE/categoria).
URL_SENTINELA_VIRAL <- paste0("https://raw.githubusercontent.com/valentim1979/",
                              "vigilancia-sentinela/main/dados/indicador_12.csv")

# SRAG: últimas SE antes da data do download ficam marcadas como
# preliminares (atraso de digitação/encerramento) e fora das correlações.
# [Inferência] 3 semanas é uma escolha conservadora; o repositório de SRAG
# usa nowcasting para estimar esse atraso.
SEMANAS_PRELIMINARES <- 3

defasagem_max_clima <- 3   # clima até 3 SE antes do desfecho
defasagem_max_vig   <- 4   # SG da sentinela até 4 SE antes do SRAG
minimo_semanas      <- 8   # abaixo disso, não calcula correlação

# Municípios da 15ª RS e população estimada 2025 (mesma tabela do BLOCO 2 do
# SCRIPT_Unificado.R do repositório virus-respiratorios).
municipios_15rs <- tibble::tribble(
  ~cod_ibge_6, ~populacao,
  "410115",    3357,   "410210",  26203,  "410220",   4046,  "410590",  23313,
  "410730",    6517,   "410780",   4805,  "410790",  11522,  "410810",   2711,
  "411000",    5693,   "411090",   4530,  "411110",   6228,  "411160",   2685,
  "411360",    4707,   "411410",  34521,  "411420",  38313,  "411480",  44749,
  "411520",  429660,   "411630",   4057,  "411640",   3669,  "411690",  27142,
  "411740",    3193,   "411750",  48695,  "411810",   9549,  "412040",   4304,
  "412340",   11730,   "412360",   1745,  "412450",   6463,  "412530",   5170,
  "412625",  128106,   "412830",   2106
)

recortes <- list(
  "Maringá + Sarandi" = c("411520", "412625"),
  "15ª RS"            = municipios_15rs$cod_ibge_6
)

unidades_sentinela <- c("6986609", "7023049")   # UPA Zona Sul, UPA Sarandi

dir.create(here::here("dados"), showWarnings = FALSE)
dir.create(here::here("img"),   showWarnings = FALSE)
dir.create(here::here("cache"), showWarnings = FALSE)


# ---- 2. FUNÇÕES AUXILIARES --------------------------------------------------------------

# Rótulo "SSAAAA" (semana + ano), igual ao do Sivep-Gripe.
rotulo_se <- function(datas) {
  se <- MMWRweek::MMWRweek(datas)
  paste0(sprintf("%02d", se$MMWRweek), se$MMWRyear)
}

# Requisição GET com até 3 tentativas; devolve o JSON convertido.
consultar_json <- function(url, parametros = list()) {
  resposta <- httr::RETRY("GET", url, query = parametros,
                          times = 3, pause_base = 5, quiet = TRUE)
  conteudo <- httr::content(resposta, as = "text", encoding = "UTF-8")
  if (httr::http_error(resposta)) {
    stop("A API respondeu ", httr::status_code(resposta), " em ", url, ": ",
         substr(conteudo, 1, 300))
  }
  jsonlite::fromJSON(conteudo)
}

url_open_meteo <- function(servico, caminho) {
  prefixo <- if (identical(chave_open_meteo, "")) "" else "customer-"
  paste0("https://", prefixo, servico, ".open-meteo.com", caminho)
}

parametros_open_meteo <- function(lista) {
  if (!identical(chave_open_meteo, "")) lista$apikey <- chave_open_meteo
  lista
}

# Correlação de Spearman que devolve NA (em vez de erro) com poucos dados.
correlacionar <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < minimo_semanas || sd(x[ok]) == 0 || sd(y[ok]) == 0) {
    return(tibble(n_semanas = sum(ok), rho = NA_real_, valor_p = NA_real_))
  }
  teste <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))
  tibble(n_semanas = sum(ok),
         rho       = round(unname(teste$estimate), 3),
         valor_p   = signif(teste$p.value, 3))
}


# ---- 3. CALENDÁRIO DE SEMANAS EPIDEMIOLÓGICAS -------------------------------------------

# Só SE completas (domingo a sábado) até ontem.
inicio_ano <- MMWRweek::MMWRweek2Date(ANO_ANALISE, 1, 1)

calendario <- tibble(data = seq(inicio_ano, Sys.Date() - 1, by = "day")) %>%
  mutate(SE = rotulo_se(data)) %>%
  group_by(SE) %>%
  summarise(inicio_se = min(data), dias = n(), .groups = "drop") %>%
  filter(dias == 7, substr(SE, 3, 6) == as.character(ANO_ANALISE)) %>%
  select(SE, inicio_se) %>%
  arrange(inicio_se)


# ---- 4. MUNICÍPIOS: CÓDIGO IBGE, NOME E CENTROIDE ---------------------------------------

# Feito uma vez e guardado em dados/municipios_15rs.csv (não muda).
arquivo_municipios <- here::here("dados", "municipios_15rs.csv")

if (file.exists(arquivo_municipios)) {
  municipios <- readr::read_csv2(arquivo_municipios,
                                 col_types = readr::cols(.default = "c",
                                                         populacao = "d",
                                                         lat = "d", lon = "d"))
} else {
  ibge_pr <- consultar_json(
    "https://servicodados.ibge.gov.br/api/v1/localidades/estados/41/municipios"
  ) %>%
    transmute(cod_ibge_7 = as.character(id), municipio = nome,
              cod_ibge_6 = substr(cod_ibge_7, 1, 6))

  municipios <- municipios_15rs %>%
    left_join(ibge_pr, by = "cod_ibge_6")

  if (anyNA(municipios$cod_ibge_7)) stop("Município sem código IBGE de 7 dígitos.")

  centroides <- purrr::map_dfr(municipios$cod_ibge_7, function(codigo) {
    meta <- consultar_json(paste0(
      "https://servicodados.ibge.gov.br/api/v3/malhas/municipios/", codigo, "/metadados"
    ))
    tibble(cod_ibge_7 = codigo,
           lon = meta$centroide$longitude[1],
           lat = meta$centroide$latitude[1])
  })

  municipios <- municipios %>%
    left_join(centroides, by = "cod_ibge_7") %>%
    select(cod_ibge_6, cod_ibge_7, municipio, populacao, lat, lon)

  readr::write_csv2(municipios, arquivo_municipios)
}


# ---- 5. SENTINELA: % DE SG NOS ATENDIMENTOS ---------------------------------------------

# Só as colunas de contagem (OBSERVA, texto livre, fica de fora).
sentinela_sg <- foreign::read.dbf(arquivo_agregado, as.is = TRUE) %>%
  select(COD_UNID, SE = SE_ATEND, SG_TOTAL, TC_TOTAL) %>%
  filter(COD_UNID %in% unidades_sentinela) %>%
  group_by(SE) %>%
  summarise(
    sg_unidades   = n_distinct(COD_UNID),
    sg_atend_sg   = sum(SG_TOTAL, na.rm = TRUE),
    sg_atend_tot  = sum(TC_TOTAL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(sg_pct = round(100 * sg_atend_sg / sg_atend_tot, 2))


# ---- 6. SENTINELA: VÍRUS NAS AMOSTRAS ---------------------------------------------------

# Categorias do Indicador 12 da NT9 (classificação final do caso, por SE de
# início de sintomas). Influenza A e B somadas para comparar com o SRAG.
sentinela_viral_bruto <- readr::read_csv2(
  URL_SENTINELA_VIRAL, show_col_types = FALSE,
  col_types = readr::cols(COD_UNID = "c", SE_PRISINT = "c",
                          categoria_viral = "c", n_casos = "i")
)

sentinela_virus <- sentinela_viral_bruto %>%
  mutate(virus = case_when(
    categoria_viral %in% c("Influenza A", "Influenza B") ~ "Influenza",
    categoria_viral == "COVID-19"                        ~ "COVID-19",
    categoria_viral == "Outro vírus respiratório"        ~ "Outro vírus respiratório",
    TRUE                                                 ~ "Não especificado"
  )) %>%
  group_by(SE = SE_PRISINT, virus) %>%
  summarise(n = sum(n_casos), .groups = "drop") %>%
  mutate(vigilancia = "Sentinela (amostras de SG)")

sentinela_positividade <- sentinela_virus %>%
  group_by(SE) %>%
  summarise(
    amostras       = sum(n),
    amostras_virus = sum(n[virus != "Não especificado"]),
    .groups = "drop"
  ) %>%
  mutate(positividade_pct = round(100 * amostras_virus / amostras, 2))


# ---- 7. SRAG: INTERNADOS POR RESIDÊNCIA -------------------------------------------------

arquivo_srag <- file.path(pasta_srag, paste0("SRAG_API_", ANO_ANALISE, ".csv"))
if (!file.exists(arquivo_srag)) stop("Não encontrei ", arquivo_srag)
data_extracao_srag <- as.Date(file.mtime(arquivo_srag))

# O CSV é nacional (centenas de MB): lê em blocos, só 5 colunas, e guarda só
# residentes da 15ª RS. Cache em cache/ (fora do git), refeito se o CSV mudar.
cache_srag <- here::here("cache", paste0("srag_15rs_", ANO_ANALISE, ".rds"))

if (file.exists(cache_srag) && file.mtime(cache_srag) >= file.mtime(arquivo_srag)) {
  srag_registros <- readRDS(cache_srag)
} else {
  message("Lendo ", basename(arquivo_srag), " (pode levar alguns minutos)...")
  srag_registros <- readr::read_delim_chunked(
    arquivo_srag, delim = ";",
    callback   = readr::DataFrameCallback$new(function(bloco, pos) {
      bloco[!is.na(bloco$CO_MUN_RES) & bloco$CO_MUN_RES %in% municipios$cod_ibge_6, ]
    }),
    chunk_size = 100000,
    locale     = readr::locale(encoding = "latin1"),
    col_types  = readr::cols_only(DT_SIN_PRI = "c", CO_MUN_RES = "c", UTI = "c",
                                  EVOLUCAO = "c", CLASSI_FIN = "c"),
    progress   = FALSE
  )
  saveRDS(srag_registros, cache_srag)
}

srag <- srag_registros %>%
  mutate(
    data_sintomas = as.Date(substr(DT_SIN_PRI, 1, 10)),
    SE            = rotulo_se(data_sintomas),
    virus = case_when(
      CLASSI_FIN == "1" ~ "Influenza",
      CLASSI_FIN == "5" ~ "COVID-19",
      CLASSI_FIN == "2" ~ "Outro vírus respiratório",
      CLASSI_FIN == "3" ~ "Outro agente etiológico",
      CLASSI_FIN == "4" ~ "Não especificado",
      TRUE              ~ "Em investigação"
    )
  ) %>%
  filter(!is.na(data_sintomas), substr(SE, 3, 6) == as.character(ANO_ANALISE))

# Semanas depois do download: sem dado (NA), não zero.
# Semanas perto do download: preliminares.
calendario_srag <- calendario %>%
  mutate(
    srag_disponivel  = inicio_se + 6 <= data_extracao_srag,
    srag_preliminar  = srag_disponivel &
                       inicio_se + 6 > data_extracao_srag - 7 * SEMANAS_PRELIMINARES
  )

srag_semanal <- purrr::imap_dfr(recortes, function(codigos, nome_recorte) {
  pop <- sum(municipios$populacao[municipios$cod_ibge_6 %in% codigos])
  srag %>%
    filter(CO_MUN_RES %in% codigos) %>%
    group_by(SE) %>%
    summarise(
      srag_casos   = n(),
      srag_uti     = sum(UTI == "1", na.rm = TRUE),
      srag_obitos  = sum(EVOLUCAO == "2", na.rm = TRUE),
      srag_virus   = sum(virus %in% c("Influenza", "COVID-19", "Outro vírus respiratório")),
      .groups = "drop"
    ) %>%
    right_join(calendario_srag, by = "SE") %>%
    mutate(
      across(c(srag_casos, srag_uti, srag_obitos, srag_virus),
             ~ if_else(srag_disponivel, tidyr::replace_na(.x, 0L), NA_integer_)),
      srag_taxa_100mil = round(1e5 * srag_casos / pop, 2),
      recorte = nome_recorte
    ) %>%
    select(recorte, SE, srag_casos, srag_taxa_100mil, srag_uti, srag_obitos,
           srag_virus, srag_preliminar)
})

srag_virus_semanal <- srag %>%
  mutate(recorte = "15ª RS") %>%
  bind_rows(srag %>% filter(CO_MUN_RES %in% recortes[["Maringá + Sarandi"]]) %>%
              mutate(recorte = "Maringá + Sarandi")) %>%
  count(recorte, SE, virus, name = "n") %>%
  mutate(vigilancia = "SRAG (internados)")


# ---- 8. CLIMA E QUALIDADE DO AR ---------------------------------------------------------

# Desde algumas SE antes do ano (para as defasagens) até ontem.
clima_inicio <- inicio_ano - 7 * defasagem_max_clima
clima_fim    <- Sys.Date() - 1

buscar_clima <- function(lat, lon) {
  json <- consultar_json(
    url_open_meteo("archive-api", "/v1/archive"),
    parametros_open_meteo(list(
      latitude = lat, longitude = lon,
      start_date = format(clima_inicio), end_date = format(clima_fim),
      daily = paste(c("temperature_2m_mean", "temperature_2m_min", "temperature_2m_max",
                      "relative_humidity_2m_mean", "precipitation_sum"), collapse = ","),
      timezone = "America/Sao_Paulo"
    ))
  )
  as_tibble(json$daily) %>% rename(data = time) %>% mutate(data = as.Date(data))
}

buscar_ar <- function(lat, lon) {
  json <- consultar_json(
    url_open_meteo("air-quality-api", "/v1/air-quality"),
    parametros_open_meteo(list(
      latitude = lat, longitude = lon,
      start_date = format(clima_inicio), end_date = format(clima_fim),
      hourly = "pm2_5,pm10,ozone,carbon_monoxide",
      timezone = "America/Sao_Paulo"
    ))
  )
  as_tibble(json$hourly) %>%
    mutate(data = as.Date(substr(time, 1, 10))) %>%
    group_by(data) %>%
    summarise(across(c(pm2_5, pm10, ozone, carbon_monoxide), ~ mean(.x, na.rm = TRUE)),
              .groups = "drop")
}

# 2 chamadas por município (60 no total; o limite gratuito é 10.000/dia).
message("Buscando clima e qualidade do ar de ", nrow(municipios), " municípios...")
clima_diario_mun <- municipios %>%
  select(cod_ibge_6, populacao, lat, lon) %>%
  mutate(dados = purrr::pmap(list(lat, lon), function(lat, lon) {
    resultado <- left_join(buscar_clima(lat, lon), buscar_ar(lat, lon), by = "data")
    Sys.sleep(0.5)
    resultado
  })) %>%
  select(cod_ibge_6, populacao, dados) %>%
  tidyr::unnest(dados)

variaveis_diarias <- c("temperature_2m_mean", "temperature_2m_min", "temperature_2m_max",
                       "relative_humidity_2m_mean", "precipitation_sum",
                       "pm2_5", "pm10", "ozone", "carbon_monoxide")

# Média do recorte ponderada pela população de cada município.
clima_semanal <- purrr::imap_dfr(recortes, function(codigos, nome_recorte) {
  clima_diario_mun %>%
    filter(cod_ibge_6 %in% codigos) %>%
    group_by(data) %>%
    summarise(across(all_of(variaveis_diarias),
                     ~ weighted.mean(.x, populacao, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(SE = rotulo_se(data)) %>%
    group_by(SE) %>%
    summarise(
      inicio_se       = min(data),
      dias            = n(),
      temp_media      = mean(temperature_2m_mean),
      temp_min        = min(temperature_2m_min),
      temp_max        = max(temperature_2m_max),
      umidade_media   = mean(relative_humidity_2m_mean),
      precipitacao_mm = sum(precipitation_sum),
      pm2_5           = mean(pm2_5),
      pm10            = mean(pm10),
      ozonio          = mean(ozone),
      co              = mean(carbon_monoxide),
      .groups = "drop"
    ) %>%
    filter(dias == 7) %>%
    select(-dias) %>%
    mutate(recorte = nome_recorte, across(where(is.double), ~ round(.x, 2)))
})

variaveis_clima <- c("temp_media", "temp_min", "temp_max", "umidade_media",
                     "precipitacao_mm", "pm2_5", "pm10", "ozonio", "co")


# ---- 9. SÉRIE SEMANAL INTEGRADA ---------------------------------------------------------

serie_semanal <- tidyr::expand_grid(recorte = names(recortes), calendario) %>%
  left_join(sentinela_sg, by = "SE") %>%
  left_join(sentinela_positividade, by = "SE") %>%
  left_join(srag_semanal, by = c("recorte", "SE")) %>%
  left_join(select(clima_semanal, -inicio_se), by = c("recorte", "SE")) %>%
  mutate(recorte = factor(recorte, levels = names(recortes))) %>%
  arrange(recorte, inicio_se)

virus_semanal <- bind_rows(
  tidyr::expand_grid(recorte = names(recortes), sentinela_virus),
  srag_virus_semanal
) %>%
  inner_join(calendario, by = "SE") %>%
  left_join(select(calendario_srag, SE, srag_preliminar), by = "SE") %>%
  mutate(preliminar = vigilancia == "SRAG (internados)" & srag_preliminar) %>%
  select(recorte, vigilancia, SE, inicio_se, virus, n, preliminar) %>%
  arrange(recorte, vigilancia, inicio_se, virus)


# ---- 10. CORRELAÇÕES --------------------------------------------------------------------

desfechos <- c(
  sg_pct           = "% de SG nos atendimentos (sentinela)",
  positividade_pct = "Positividade viral das amostras (sentinela)",
  srag_taxa_100mil = "Internações por SRAG por 100 mil hab."
)

# Tira das correlações as SE preliminares do SRAG.
serie_analise <- serie_semanal %>%
  mutate(srag_taxa_100mil = if_else(srag_preliminar %in% TRUE, NA_real_, srag_taxa_100mil))

# (a) Clima de k semanas antes x cada desfecho. A defasagem é aplicada na
# série completa de clima (inclui as SE do fim do ano anterior).
correlacao_clima <- purrr::map_dfr(names(recortes), function(nome_recorte) {
  clima_rec <- clima_semanal %>% filter(recorte == nome_recorte) %>% arrange(inicio_se)
  desf_rec  <- serie_analise %>% filter(recorte == nome_recorte) %>%
    select(SE, all_of(names(desfechos)))

  combinacoes <- tidyr::expand_grid(desfecho = names(desfechos),
                                    variavel = variaveis_clima,
                                    defasagem = 0:defasagem_max_clima)

  purrr::pmap_dfr(combinacoes, function(desfecho, variavel, defasagem) {
    dados <- tibble(SE = clima_rec$SE,
                    clima = dplyr::lag(clima_rec[[variavel]], defasagem)) %>%
      inner_join(desf_rec, by = "SE")
    tibble(desfecho = desfecho, variavel = variavel, defasagem = defasagem,
           correlacionar(dados$clima, dados[[desfecho]]))
  }) %>%
    mutate(recorte = nome_recorte, tipo = "Clima x desfecho")
})

# (b) Sentinela de k semanas antes x SRAG: a sentinela antecipa o SRAG?
correlacao_vigilancias <- purrr::map_dfr(names(recortes), function(nome_recorte) {
  rec <- serie_analise %>% filter(recorte == nome_recorte) %>% arrange(inicio_se)
  combinacoes <- tidyr::expand_grid(variavel = c("sg_pct", "positividade_pct"),
                                    defasagem = 0:defasagem_max_vig)

  purrr::pmap_dfr(combinacoes, function(variavel, defasagem) {
    tibble(variavel = variavel, defasagem = defasagem,
           correlacionar(dplyr::lag(rec[[variavel]], defasagem), rec$srag_taxa_100mil))
  }) %>%
    mutate(recorte = nome_recorte, tipo = "Sentinela x SRAG",
           desfecho = "srag_taxa_100mil")
})

correlacoes <- bind_rows(correlacao_clima, correlacao_vigilancias) %>%
  select(recorte, tipo, desfecho, variavel, defasagem, n_semanas, rho, valor_p)


# ---- 11. EXPORTAÇÃO ---------------------------------------------------------------------

readr::write_csv2(serie_semanal, here::here("dados", "serie_semanal.csv"))
readr::write_csv2(virus_semanal, here::here("dados", "virus_semanal.csv"))
readr::write_csv2(correlacoes,   here::here("dados", "correlacoes.csv"))

# Datas de referência, lidas pelas páginas.
readr::write_csv2(
  tibble(
    gerado_em          = Sys.Date(),
    extracao_srag      = data_extracao_srag,
    # Última SE com dado, pela ordem do calendário (não alfabética).
    ultima_se_sg       = dplyr::last(calendario$SE[calendario$SE %in% sentinela_sg$SE]),
    ultima_se_viral    = dplyr::last(calendario$SE[calendario$SE %in% sentinela_positividade$SE]),
    semanas_prelim     = SEMANAS_PRELIMINARES
  ),
  here::here("dados", "referencias.csv")
)


# ---- 12. GRÁFICOS -----------------------------------------------------------------------

cores_virus <- c(
  "Influenza"                = "#0057A3",
  "COVID-19"                 = "#c62828",
  "Outro vírus respiratório" = "#2e7d32",
  "Outro agente etiológico"  = "#f9a825",
  "Não especificado"         = "#9e9e9e",
  "Em investigação"          = "#d6d6d6"
)

rodape <- paste0("Fontes: Sivep-Gripe (Ficha de Agregado Semanal; Indicador 12 da NT9; ",
                 "SRAG via dados.gov.br, baixado em ", format(data_extracao_srag, "%d/%m/%Y"),
                 "); Open-Meteo (CC-BY 4.0). Gerado em ", format(Sys.Date(), "%d/%m/%Y"), ".")

paineis <- c(
  sg_pct           = "% de SG nos atendimentos\n(UPAs sentinelas)",
  positividade_pct = "Positividade viral das\namostras sentinela (%)",
  srag_taxa_100mil = "Internações por SRAG\npor 100 mil hab.",
  temp_media       = "Temperatura média (°C)",
  umidade_media    = "Umidade relativa média (%)",
  pm2_5            = "MP2,5 médio (µg/m³)"
)

purrr::walk(names(recortes), function(nome_recorte) {
  dados <- serie_semanal %>%
    filter(recorte == nome_recorte) %>%
    select(inicio_se, srag_preliminar, all_of(names(paineis))) %>%
    pivot_longer(all_of(names(paineis)), names_to = "variavel", values_to = "valor") %>%
    filter(!is.na(valor)) %>%
    mutate(variavel = factor(variavel, levels = names(paineis), labels = paineis))

  painel_srag <- paineis[["srag_taxa_100mil"]]

  grafico <- ggplot(dados, aes(x = inicio_se, y = valor)) +
    geom_col(data = filter(dados, variavel == painel_srag),
             aes(fill = srag_preliminar), width = 6) +
    geom_line(data = filter(dados, variavel != painel_srag),
              colour = "#0057A3", linewidth = 0.7) +
    facet_wrap(~ variavel, ncol = 1, scales = "free_y", strip.position = "left") +
    scale_fill_manual(values = c(`FALSE` = "#0057A3", `TRUE` = "#9ec3e6"),
                      labels = c(`FALSE` = "SRAG", `TRUE` = "SRAG - SE preliminar"),
                      name = NULL) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = paste0("Vigilâncias e clima por semana epidemiológica - ", nome_recorte),
         x = "Início da semana epidemiológica", y = NULL, caption = rodape) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top", strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, hjust = 1),
          plot.caption = element_text(size = 7.5))

  nome_arquivo <- if (nome_recorte == "15ª RS") "linha_tempo_15rs.png" else "linha_tempo_maringa_sarandi.png"
  ggsave(here::here("img", nome_arquivo), grafico,
         width = 10, height = 11, dpi = 150, bg = "white")
})

grafico_virus <- virus_semanal %>%
  filter(recorte == "15ª RS") %>%
  mutate(
    virus = factor(virus, levels = names(cores_virus)),
    vigilancia = factor(vigilancia, levels = c("Sentinela (amostras de SG)",
                                               "SRAG (internados)"),
                        labels = c("Sentinela - amostras de SG das 2 UPAs (casos)",
                                   "SRAG - internados residentes na 15ª RS (casos)"))
  ) %>%
  ggplot(aes(x = inicio_se, y = n, fill = virus)) +
  geom_col(width = 6) +
  facet_wrap(~ vigilancia, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = cores_virus, name = NULL, drop = FALSE) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "SE de início dos sintomas", y = "Casos", caption = rodape) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", plot.caption = element_text(size = 7.5)) +
  guides(fill = guide_legend(nrow = 2))

ggsave(here::here("img", "virus_comparacao.png"), grafico_virus,
       width = 10, height = 7, dpi = 150, bg = "white")

message("Pronto. SRAG baixado em ", format(data_extracao_srag, "%d/%m/%Y"), "; ",
        nrow(calendario), " SE completas em ", ANO_ANALISE, ".")
