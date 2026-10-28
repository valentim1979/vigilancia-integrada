#!/bin/bash
# Publica o painel integrado (Sentinela + SRAG + clima) no GitHub Pages.
# Uso: rode a partir da raiz do repositório.
#
# Antes de rodar, para ter dados novos:
#   - Sentinela: export novo da Ficha de Agregado Semanal (CAMINHO_SG_AGREGADO_DBF)
#     e painel vigilancia-sentinela publicado (indicador_12.csv no GitHub).
#   - SRAG: o painel virus-respiratorios precisa ter baixado a base da API
#     (CSV em CAMINHO_SRAG_CACHE).

set -e  # para no primeiro erro, em vez de publicar dado incompleto

echo ">>> 1/3 - Juntando vigilâncias e clima (script_integrado.R)..."
Rscript script_integrado.R

echo ">>> 2/3 - Renderizando o site Quarto..."
quarto render

echo ">>> 3/3 - Publicando no GitHub..."
git add .
git commit -m "Atualiza painel integrado - $(date +'%Y-%m-%d %H:%M')" || echo "Nada novo para commitar."
git push origin main

echo ">>> Pronto."
