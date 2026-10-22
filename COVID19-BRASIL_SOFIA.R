
# Impacto do COVID-19 no Resultado Eleitoral de 2022
# Autor: Sofia Aiax Nepomuceno
# Data: março de 2026
# =============================================================================
# INSTRUÇÕES DE USO:
# 1. Coloque todos os arquivos de dados na mesma pasta deste script
# 2. Desative qualquer VPN antes de rodar — o script baixa dados
#    automaticamente do SIDRA (IBGE) e requer acesso à internet
# 3. Execute o script completo com Ctrl+A → Ctrl+Enter
#    Os arquivos ZIP serão descompactados automaticamente
# =============================================================================
# ARQUIVOS NECESSÁRIOS:
# - votacao_candidato_munzona_2022.zip
# - votacao_candidato_munzona_2018.zip
# - municipio_tse_ibge (1).zip
# - caso_full.csv.gz (não precisa descompactar)
# =============================================================================

# Define automaticamente o diretório de trabalho como a pasta do script
if (!is.null(sys.frames()[[1]]$ofile)) {
  setwd(dirname(sys.frames()[[1]]$ofile))
} else {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

# ── DESCOMPACTAR ARQUIVOS ─────────────────────────────────────────────────────

# Descompacta ZIPs eleitorais caso ainda não tenham sido descompactados
zips_eleitorais <- list.files(pattern = "votacao_candidato_munzona.*\\.zip$",
                              full.names = TRUE)
for (z in zips_eleitorais) {
  pasta <- tools::file_path_sans_ext(basename(z))
  if (!dir.exists(pasta)) unzip(z)
}

# Descompacta ZIP do dicionário TSE x IBGE
zip_dic <- list.files(pattern = "municipio_tse_ibge.*\\.zip$", full.names = TRUE)
if (length(zip_dic) > 0 && !any(dir.exists(tools::file_path_sans_ext(basename(zip_dic))))) {
  unzip(zip_dic[1])
}
# ── 1. PACOTES ────────────────────────────────────────────────────────────────

pacotes <- c("tidyverse", "data.table", "janitor", "geobr",
             "sidrar", "modelsummary", "sandwich", "car",
             "ggthemes", "scales", "officer", "R.utils")

instalar <- pacotes[!pacotes %in% installed.packages()[, "Package"]]
if (length(instalar) > 0) install.packages(instalar)

library(tidyverse)
library(data.table)
library(janitor)
library(geobr)
library(sidrar)
library(modelsummary)
library(sandwich)
library(car)
library(scales)

# ── 2. LEITURA DOS DADOS ELEITORAIS ───────────────────────────────────────────

# Busca todos os CSVs de 2022 e 2018 recursivamente — funciona em qualquer PC
# independente do nome da pasta ou do sistema operacional
arquivos_2022 <- list.files(pattern = "votacao_candidato_munzona_2022.*\\.csv$",
                            recursive = TRUE, full.names = TRUE)

tse_2022 <- rbindlist(lapply(arquivos_2022, function(x) {
  fread(
    x,
    encoding = "Latin-1",
    sep = ";",
    showProgress = FALSE,
    select = c("CD_MUNICIPIO", "NM_CANDIDATO",
               "DS_CARGO", "NR_TURNO", "SG_UF", "QT_VOTOS_NOMINAIS")
  )[DS_CARGO == "Presidente" & NR_TURNO == 2 & SG_UF != "ZZ"]
}), fill = TRUE)

arquivos_2018 <- list.files(pattern = "votacao_candidato_munzona_2018.*\\.csv$",
                            recursive = TRUE, full.names = TRUE)

tse_2018 <- rbindlist(lapply(arquivos_2018, function(x) {
  fread(
    x,
    encoding = "Latin-1",
    sep = ";",
    showProgress = FALSE,
    select = c("CD_MUNICIPIO", "NM_CANDIDATO",
               "DS_CARGO", "NR_TURNO", "SG_UF", "QT_VOTOS_NOMINAIS")
  )[DS_CARGO == "Presidente" & NR_TURNO == 2 & SG_UF != "ZZ"]
}), fill = TRUE)

# ── 3. VERIFICAÇÃO PRÉ-FILTRO — 2022 ──────────────────────────────────────────

sum(is.na(tse_2022))                                    # Valores nulos
sum(duplicated(tse_2022))                               # Duplicatas (zonas eleitorais — normais)
n_distinct(tse_2022$CD_MUNICIPIO)                       # Municípios únicos (esperado: ~5.570)
n_distinct(tse_2022$SG_UF)                             # Estados (esperado: 27 + ZZ)
unique(tse_2022$SG_UF) |> sort()                       # Identificar ZZ (exterior)
unique(tse_2022$NR_TURNO)                              # Turnos (esperado: 1 e 2)
unique(tse_2022$ANO_ELEICAO)                           # Ano (esperado: só 2022)
unique(tse_2022$DS_CARGO)                              # Cargos disponíveis
sum(tse_2022$QT_VOTOS_NOMINAIS <= 0)                   # Votos zerados (só deputados)
sum(tse_2022$QT_VOTOS_NOMINAIS)                        # Total de votos (~120M após filtro)

# ── 4. VERIFICAÇÃO PRÉ-FILTRO — 2018 ──────────────────────────────────────────

sum(is.na(tse_2018))
sum(duplicated(tse_2018))
n_distinct(tse_2018$CD_MUNICIPIO)
n_distinct(tse_2018$SG_UF)
unique(tse_2018$SG_UF) |> sort()
unique(tse_2018$NR_TURNO)
unique(tse_2018$ANO_ELEICAO)
unique(tse_2018$DS_CARGO)
sum(tse_2018$QT_VOTOS_NOMINAIS <= 0)
sum(tse_2018$QT_VOTOS_NOMINAIS)

# ── 5. FILTRAR E AGREGAR ───────────────────────────────────────────────────────

pres_2022 <- tse_2022 |>
  filter(DS_CARGO == "Presidente", NR_TURNO == 2, SG_UF != "ZZ") |>
  group_by(CD_MUNICIPIO, NM_CANDIDATO) |>
  summarise(votos = sum(QT_VOTOS_NOMINAIS), .groups = "drop")

pres_2018 <- tse_2018 |>
  filter(DS_CARGO == "Presidente", NR_TURNO == 2, SG_UF != "ZZ") |>
  group_by(CD_MUNICIPIO, NM_CANDIDATO) |>
  summarise(votos = sum(QT_VOTOS_NOMINAIS), .groups = "drop")

unique(pres_2022$NM_CANDIDATO)
unique(pres_2018$NM_CANDIDATO)

# ── 6. CALCULAR PERCENTUAIS E VARIAÇÃO DE VOTO ────────────────────────────────

bolso_2022 <- pres_2022 |>
  pivot_wider(names_from = NM_CANDIDATO, values_from = votos) |>
  clean_names() |>
  mutate(
    total_2022         = jair_messias_bolsonaro + luiz_inacio_lula_da_silva,
    pct_bolsonaro_2022 = jair_messias_bolsonaro / total_2022
  ) |>
  select(cd_municipio, pct_bolsonaro_2022, total_2022)

bolso_2018 <- pres_2018 |>
  pivot_wider(names_from = NM_CANDIDATO, values_from = votos) |>
  clean_names() |>
  mutate(
    total_2018         = jair_messias_bolsonaro + fernando_haddad,
    pct_bolsonaro_2018 = jair_messias_bolsonaro / total_2018
  ) |>
  select(cd_municipio, pct_bolsonaro_2018, total_2018)

eleicoes <- bolso_2022 |>
  left_join(bolso_2018, by = "cd_municipio") |>
  mutate(variacao_bolsonaro = pct_bolsonaro_2022 - pct_bolsonaro_2018)

head(eleicoes)
summary(eleicoes)

# ── 7. LEITURA DOS DADOS DE COVID ─────────────────────────────────────────────

# Busca o arquivo .gz recursivamente — funciona em qualquer PC
arquivo_covid <- list.files(pattern = "caso_full\\.csv$",
                            recursive = TRUE, full.names = TRUE)

covid <- fread(arquivo_covid[1], showProgress = FALSE)

glimpse(covid)
names(covid)

# ── 8. VERIFICAÇÃO PRÉ-FILTRO — COVID ─────────────────────────────────────────

sum(is.na(covid))
unique(covid$place_type)
range(covid$date)
n_distinct(covid$city_ibge_code[covid$place_type == "city"])
sum(covid$is_last)
sum(covid$last_available_deaths < 0, na.rm = TRUE)
colSums(is.na(covid)) |> sort(decreasing = TRUE)

# ── 9. FILTRAR E PREPARAR COVID ───────────────────────────────────────────────

covid_municipios <- covid |>
  filter(place_type == "city", is_last == TRUE) |>
  mutate(
    obitos_per_capita = last_available_deaths    / estimated_population * 100000,
    casos_per_capita  = last_available_confirmed / estimated_population * 100000
  ) |>
  select(
    city_ibge_code,
    city,
    state,
    last_available_deaths,
    last_available_confirmed,
    obitos_per_capita,
    casos_per_capita,
    estimated_population
  )

# ── 10. REMOVER REGISTROS INVÁLIDOS ───────────────────────────────────────────

covid_municipios <- covid_municipios |>
  filter(!is.na(city_ibge_code))

n_distinct(covid_municipios$city_ibge_code)
sum(is.na(covid_municipios$city_ibge_code))


# ── 11. LEITURA DO DICIONÁRIO TSE x IBGE ──────────────────────────────────────

# O dicionário traduz códigos municipais do TSE para o IBGE
# Necessário para cruzar dados eleitorais com dados de COVID
arquivo_dic <- list.files(pattern = "municipio_tse_ibge.*\\.csv$",
                          recursive = TRUE, full.names = TRUE)

if (length(arquivo_dic) == 0) stop("Arquivo do dicionário não encontrado. Verifique se o ZIP foi descompactado e os arquivos estão na pasta correta.")

dicionario <- data.table::fread(arquivo_dic[1], encoding = "Latin-1", sep = ";")

# ── 12. VERIFICAÇÃO DO DICIONÁRIO ─────────────────────────────────────────────

sum(is.na(dicionario))
n_distinct(dicionario$CD_MUNICIPIO_TSE)
n_distinct(dicionario$CD_MUNICIPIO_IBGE)
sum(duplicated(dicionario$CD_MUNICIPIO_TSE))
sum(duplicated(dicionario$CD_MUNICIPIO_IBGE))

# ── 13. MERGE DAS TRÊS BASES ──────────────────────────────────────────────────

eleicoes_ibge <- eleicoes |>
  left_join(
    dicionario |> select(CD_MUNICIPIO_TSE, CD_MUNICIPIO_IBGE),
    by = c("cd_municipio" = "CD_MUNICIPIO_TSE")
  )

base_final <- eleicoes_ibge |>
  left_join(
    covid_municipios,
    by = c("CD_MUNICIPIO_IBGE" = "city_ibge_code")
  )

nrow(base_final)
sum(is.na(base_final$obitos_per_capita))
sum(is.na(base_final$CD_MUNICIPIO_IBGE))

# ── 14. DADOS DE CONTROLE — PIB PER CAPITA ────────────────────────────────────

pib_raw <- get_sidra(x = 5938, variable = 37,  period = "2020", geo = "City")
pop_raw <- get_sidra(x = 6579, variable = 9324, period = "2020", geo = "City")

# Limpar nomes das colunas (ESSENCIAL)
pib_raw <- janitor::clean_names(pib_raw)
pop_raw <- janitor::clean_names(pop_raw)

# Selecionar colunas de forma segura (sem acento/quebrar código)
pib <- pib_raw |>
  select(cod_ibge = municipio_codigo, pib_total = valor) |>
  mutate(
    cod_ibge  = as.integer(cod_ibge),
    pib_total = as.numeric(pib_total)
  )

pop <- pop_raw |>
  select(cod_ibge = municipio_codigo, populacao = valor) |>
  mutate(
    cod_ibge  = as.integer(cod_ibge),
    populacao = as.numeric(populacao)
  )

# Calcular PIB per capita
pib_pc <- pib |>
  left_join(pop, by = "cod_ibge") |>
  mutate(pib_per_capita = (pib_total * 1000) / populacao)

summary(pib_pc$pib_per_capita)

# ── 15. DADOS DE CONTROLE — TAXA DE URBANIZAÇÃO ───────────────────────────────

pop_total  <- get_sidra(x = 1378, variable = 93, period = "2010",
                        geo = "City", classific = "c1", category = list(c1 = 0))

pop_urbana <- get_sidra(x = 1378, variable = 93, period = "2010",
                        geo = "City", classific = "c1", category = list(c1 = 1))

# Limpar nomes das colunas (ESSENCIAL)
pop_total  <- janitor::clean_names(pop_total)
pop_urbana <- janitor::clean_names(pop_urbana)

# Seleção segura
pt <- pop_total |>
  select(cod_ibge = municipio_codigo, pop_total = valor) |>
  mutate(
    cod_ibge  = as.integer(cod_ibge),
    pop_total = as.numeric(pop_total)
  )

pu <- pop_urbana |>
  select(cod_ibge = municipio_codigo, pop_urbana = valor) |>
  mutate(
    cod_ibge   = as.integer(cod_ibge),
    pop_urbana = as.numeric(pop_urbana)
  )

# Calcular taxa de urbanização
urbanizacao <- pt |>
  left_join(pu, by = "cod_ibge") |>
  mutate(taxa_urbanizacao = pop_urbana / pop_total)

summary(urbanizacao$taxa_urbanizacao)

# ── 16. ADICIONAR CONTROLES À BASE FINAL ──────────────────────────────────────

base_modelo <- base_final |>
  left_join(pib_pc |> select(cod_ibge, pib_per_capita),
            by = c("CD_MUNICIPIO_IBGE" = "cod_ibge")) |>
  left_join(urbanizacao |> select(cod_ibge, taxa_urbanizacao),
            by = c("CD_MUNICIPIO_IBGE" = "cod_ibge")) |>
  mutate(
    regiao = case_when(
      state %in% c("AC","AM","AP","PA","RO","RR","TO") ~ "Norte",
      state %in% c("AL","BA","CE","MA","PB","PE","PI","RN","SE") ~ "Nordeste",
      state %in% c("DF","GO","MS","MT") ~ "Centro-Oeste",
      state %in% c("ES","MG","RJ","SP") ~ "Sudeste",
      state %in% c("PR","RS","SC") ~ "Sul"
    ),
    regiao = factor(regiao, levels = c("Sul", "Norte", "Nordeste",
                                       "Centro-Oeste", "Sudeste"))
  )

media_urb_estado <- base_modelo |>
  filter(!is.na(taxa_urbanizacao)) |>
  group_by(state) |>
  summarise(media_urb = mean(taxa_urbanizacao))

base_modelo <- base_modelo |>
  left_join(media_urb_estado, by = "state") |>
  mutate(taxa_urbanizacao = ifelse(is.na(taxa_urbanizacao),
                                   media_urb, taxa_urbanizacao)) |>
  select(-media_urb) |>
  mutate(obitos_per_100k = obitos_per_capita / 100)

sum(is.na(base_modelo$pib_per_capita))
sum(is.na(base_modelo$taxa_urbanizacao))

write.csv(base_modelo, "base_modelo.csv", row.names = FALSE)

# ── 17. ANÁLISE EXPLORATÓRIA ───────────────────────────────────────────────────

anotacoes <- base_modelo |>
  filter(city %in% c("São Paulo", "Rio de Janeiro",
                     "Manaus", "Brasília",
                     "Redenção da Serra", "Treviso",
                     "Normandia", "Jacareacanga"))

ggplot(base_modelo, aes(x = obitos_per_capita,
                        y = variacao_bolsonaro,
                        size = estimated_population)) +
  geom_point(alpha = 0.3, color = "#DB0011") +
  geom_smooth(method = "lm", se = TRUE,
              color = "#1A1A1A", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "#666666", linewidth = 0.4) +
  geom_point(data = anotacoes, color = "#1A1A1A",
             fill = "#DB0011", shape = 21, stroke = 0.8) +
  geom_text(data = anotacoes, aes(label = city),
            size = 2.5, color = "#1A1A1A",
            hjust = -0.15, vjust = 0.5) +
  scale_size_continuous(range = c(0.5, 6), labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title    = "Municípios mais afetados pelo COVID votaram menos no incumbente",
    subtitle = "Variação do percentual de votos (2018-2022) vs. óbitos por 100 mil habitantes",
    x        = "Óbitos por 100 mil habitantes (acumulado até mar/2022)",
    y        = "Variação no percentual de votos (p.p.)",
    size     = "População",
    caption  = "Fontes: TSE (2018, 2022) e Brasil.IO. Elaboração própria."
  ) +
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 11, face = "bold", color = "#1A1A1A"),
    plot.subtitle    = element_text(size = 9,  color = "#666666"),
    plot.caption     = element_text(size = 7,  color = "#999999"),
    axis.text        = element_text(size = 8,  color = "#444444"),
    axis.title       = element_text(size = 9,  color = "#444444"),
    legend.position  = "bottom"
  )

ggsave("grafico1_scatter.png", width = 10, height = 6, dpi = 300)

base_quartis <- base_modelo |>
  mutate(
    quartil = ntile(obitos_per_capita, 4),
    quartil = factor(quartil,
                     labels = c("1º quartil\n(menos afetados)",
                                "2º quartil", "3º quartil",
                                "4º quartil\n(mais afetados)"))
  ) |>
  group_by(quartil) |>
  summarise(
    media_variacao = mean(variacao_bolsonaro),
    se             = sd(variacao_bolsonaro) / sqrt(n()),
    ic_inf         = media_variacao - 1.96 * se,
    ic_sup         = media_variacao + 1.96 * se
  )

ggplot(base_quartis, aes(x = quartil, y = media_variacao)) +
  geom_col(fill = "#DB0011", alpha = 0.85, width = 0.6) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup),
                width = 0.15, color = "#1A1A1A", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "#666666", linewidth = 0.4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title    = "Quanto maior o impacto do COVID, maior a queda no voto no incumbente",
    subtitle = "Variação média do percentual de votos (2018-2022) por quartil de óbitos per capita",
    x        = "Quartil de óbitos por 100 mil habitantes",
    y        = "Variação média no percentual de votos (p.p.)",
    caption  = "Fontes: TSE (2018, 2022) e Brasil.IO. Elaboração própria.\nBarras de erro representam intervalo de confiança de 95%."
  ) +
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 11, face = "bold", color = "#1A1A1A"),
    plot.subtitle    = element_text(size = 9,  color = "#666666"),
    plot.caption     = element_text(size = 7,  color = "#999999"),
    axis.text        = element_text(size = 8,  color = "#444444"),
    axis.title       = element_text(size = 9,  color = "#444444")
  )

ggsave("grafico2_quartis.png", width = 10, height = 6, dpi = 300)

estados <- read_state(year = 2020, showProgress = FALSE)

variacao_estado <- base_modelo |>
  group_by(state) |>
  summarise(variacao_media = mean(variacao_bolsonaro))

mapa <- estados |>
  left_join(variacao_estado, by = c("abbrev_state" = "state"))

ggplot(mapa) +
  geom_sf(aes(fill = variacao_media), color = "#FFFFFF", linewidth = 0.3) +
  scale_fill_gradient2(
    low      = "#DB0011",
    mid      = "#F5F5F5",
    high     = "#003087",
    midpoint = 0,
    labels   = scales::percent,
    name     = "Variação média\nno voto (p.p.)"
  ) +
  labs(
    title    = "Bolsonaro perdeu votos no Sul e Sudeste, mas ganhou no Norte e Centro-Oeste",
    subtitle = "Variação média do percentual de votos por estado (2018-2022)",
    caption  = "Fontes: TSE (2018, 2022) e Brasil.IO. Elaboração própria."
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(size = 11, face = "bold", color = "#1A1A1A"),
    plot.subtitle = element_text(size = 9,  color = "#666666"),
    plot.caption  = element_text(size = 7,  color = "#999999"),
    legend.title  = element_text(size = 8),
    legend.text   = element_text(size = 7)
  )

ggsave("grafico3_mapa.png", width = 10, height = 7, dpi = 300)

# ── 18. MODELAGEM OLS ──────────────────────────────────────────────────────────

modelo1 <- lm(variacao_bolsonaro ~ obitos_per_100k,
              data = base_modelo)

modelo2 <- lm(variacao_bolsonaro ~ obitos_per_100k +
                log(pib_per_capita) + regiao,
              data = base_modelo)

modelo3 <- lm(variacao_bolsonaro ~ obitos_per_100k +
                log(pib_per_capita) + taxa_urbanizacao + regiao,
              data = base_modelo)

erros_robustos <- list(
  "Modelo 1" = sandwich::vcovHC(modelo1, type = "HC3"),
  "Modelo 2" = sandwich::vcovHC(modelo2, type = "HC3"),
  "Modelo 3" = sandwich::vcovHC(modelo3, type = "HC3")
)

# ── 19. VALIDAÇÃO DO MODELO ────────────────────────────────────────────────────

residuos <- residuals(modelo3)

ggplot(data.frame(residuos), aes(x = residuos)) +
  geom_histogram(bins = 50, fill = "#DB0011", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#1A1A1A") +
  labs(
    title    = "Distribuição dos resíduos — Modelo 3",
    subtitle = "Resíduos devem se distribuir aproximadamente como uma normal",
    x        = "Resíduos",
    y        = "Frequência",
    caption  = "Elaboração própria."
  ) +
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 11, face = "bold", color = "#1A1A1A"),
    plot.subtitle    = element_text(size = 9,  color = "#666666"),
    plot.caption     = element_text(size = 7,  color = "#999999")
  )

ncvTest(modelo3)
vif(modelo3)

regioes <- c("Sul", "Sudeste", "Nordeste", "Norte", "Centro-Oeste")

resultados_regiao <- map_df(regioes, function(r) {
  mod <- lm(variacao_bolsonaro ~ obitos_per_100k +
              log(pib_per_capita) + taxa_urbanizacao,
            data = base_modelo |> filter(regiao == r))
  
  coef_covid <- coef(mod)["obitos_per_100k"]
  pval       <- summary(mod)$coefficients["obitos_per_100k", 4]
  
  data.frame(
    regiao        = r,
    coeficiente   = round(coef_covid, 5),
    p_valor       = round(pval, 4),
    significativo = ifelse(pval < 0.05, "Sim", "Não")
  )
})

print(resultados_regiao)

modelo_casos <- lm(variacao_bolsonaro ~ casos_per_capita +
                     log(pib_per_capita) + taxa_urbanizacao + regiao,
                   data = base_modelo)

summary(modelo_casos)$coefficients["casos_per_capita", ]
summary(modelo3)$coefficients["obitos_per_100k", ]

# ── 20. TABELA DE RESULTADOS ───────────────────────────────────────────────────

# Salvar o arquivo
modelsummary(
  list("Modelo 1" = modelo1,
       "Modelo 2" = modelo2,
       "Modelo 3" = modelo3),
  vcov        = erros_robustos,
  stars       = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  coef_rename = c(
    "obitos_per_100k"      = "Óbitos (por 100 óbitos/100k hab.)",
    "log(pib_per_capita)"  = "log(PIB per capita)",
    "taxa_urbanizacao"     = "Taxa de urbanização",
    "regiaoNorte"          = "Região: Norte",
    "regiaoNordeste"       = "Região: Nordeste",
    "regiaoCentro-Oeste"   = "Região: Centro-Oeste",
    "regiaoSudeste"        = "Região: Sudeste"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title   = "Impacto do COVID-19 na variação do voto no incumbente (2018-2022)",
  notes   = "Erros padrão robustos (HC3) entre parênteses. Categoria de referência: Região Sul.",
  output  = "tabela_resultados.html"
)

# Caminho seguro
arquivo <- file.path(getwd(), "tabela_resultados.html")

# Abrir arquivo
browseURL(arquivo)


# =============================================================================
# FIM DO SCRIPT
# =============================================================================