# Vigilância Integrada de Vírus Respiratórios x Clima — 15ª RS

Painel estático (Quarto + GitHub Pages) que compara, por semana epidemiológica, as duas vigilâncias de vírus respiratórios da 15ª Regional de Saúde de Maringá/PR e o clima da região:

- **Vigilância Sentinela de SG** — 2 UPAs (Zona Sul/Maringá e Sarandi): % de atendimentos por SG e positividade viral das amostras (5 por unidade por semana). Repositório: [vigilancia-sentinela](https://github.com/valentim1979/vigilancia-sentinela).
- **Vigilância Universal de SRAG** — internados residentes na 15ª RS. Repositório: [virus-respiratorios](https://github.com/valentim1979/virus-respiratorios).
- **Clima e qualidade do ar** — Open-Meteo, nos centroides IBGE dos 30 municípios.

Este repositório **não altera** os outros dois: só lê as saídas deles.

## ⚠️ Privacidade

Só dados **agregados** (contagens por SE e recorte) vão para `dados/` e `img/`. A leitura da base de SRAG gera um cache com uma linha por caso em `cache/`, que está no `.gitignore` e nunca deve ser versionado.

## Estrutura

```
.
├── script_integrado.R   # junta as 3 fontes e gera dados/ e img/
├── _comum.R             # funções usadas pelas páginas
├── index.qmd            # visão geral + linha do tempo
├── vigilancias.qmd      # sentinela x SRAG (antecipação, vírus)
├── clima.qmd            # correlações com clima e qualidade do ar
├── metodo.qmd           # fontes, definições e limitações
├── dados/               # CSVs agregados (versionados)
├── img/                 # gráficos PNG (versionados)
├── cache/               # registros de SRAG filtrados (NÃO versionado)
├── docs/                # site gerado pelo quarto render (GitHub Pages)
└── publicar.sh          # script R + quarto render + git push
```

## Configuração (uma vez por computador)

No `~/.Renviron`:

```
CAMINHO_SG_AGREGADO_DBF=/Users/valentimsalajunior/Documents/DBF_Vig_Sentinela/SG2026_agregado.dbf
CAMINHO_SRAG_CACHE=/Users/valentimsalajunior/Documents/vigilancia-epidemiologica/dbf_sivep
# Opcional - só com plano pago do Open-Meteo:
# OPEN_METEO_API_KEY=...
```

## Como atualizar

1. Atualize as fontes: export novo da Ficha de Agregado Semanal; painel sentinela publicado; painel de SRAG com a base da API baixada.
2. Rode `./publicar.sh` (ou, à mão: `Rscript script_integrado.R` → `quarto render` → commit e push).

Na primeira publicação: GitHub → **Settings > Pages** → Deploy from a branch → `main`, pasta `/docs`.

## Fontes

- SIVEP-Gripe (Ficha de Agregado Semanal; SRAG hospitalizado via dados.gov.br).
- Open-Meteo — Historical Weather API e Air Quality API (CC-BY 4.0; uso gratuito não comercial, até 10.000 chamadas/dia — o script faz 60).
- IBGE — API de localidades e de malhas (centroides).
