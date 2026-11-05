# Variable dictionary for ELSOC_Long_2016_2023_1.00 ---------------------------
# Lists every variable (name, label, type, answer codes) and how much
# longitudinal information it carries: valid N per wave, number of waves with
# data, longest run of consecutive waves and respondents with 3+ valid obs.
# Output: output/tables/ELSOC_variable_dictionary_2016_2023.xlsx

if (!require("pacman")) install.packages("pacman")
rm(list = ls())
pacman::p_load(here, readxl, openxlsx)

load(here::here("input/data/original/ELSOC_Long_2016_2023_1.00.RData"))
df <- elsoc_long_2016_2023

ref_file <- here::here("input/data/original/Listado-de-Variables-ELSOC-2.xlsx")
ref_es <- as.data.frame(readxl::read_excel(ref_file, sheet = "Español"))
ref_en <- as.data.frame(readxl::read_excel(ref_file, sheet = "English"))

# Settings ---------------------------------------------------------------------
miss_codes <- c(-999, -888, -777, -666)   # NR, NS, technical error, incomplete
min_valid  <- 30                          # min valid cases for a wave to count as "available"
waves      <- c(`1` = 2016, `2` = 2017, `3` = 2018, `4` = 2019,
                `5` = 2021, `6` = 2022, `7` = 2023)

# Helpers ----------------------------------------------------------------------
is_valid <- function(x) {
  if (is.character(x)) return(!is.na(x) & trimws(x) != "" & !(trimws(x) %in% as.character(miss_codes)))
  !is.na(x) & !(as.numeric(x) %in% miss_codes)
}

max_run <- function(avail) {
  r <- rle(avail)
  if (!any(r$values)) return(0L)
  max(r$lengths[r$values])
}

value_labels <- function(x) {
  lab <- attr(x, "labels", exact = TRUE)
  if (is.null(lab)) return(NULL)
  lab[!(lab %in% miss_codes)]
}

infer_type <- function(x) {
  if (is.character(x)) return("Cadena")
  if (is.logical(x)) return("Logica")
  if (length(value_labels(x)) > 0) return("Categorica")
  "Numerica"
}

# Per-variable summary ---------------------------------------------------------
ola <- df$ola
id  <- df$idencuesta
n_by_wave <- as.vector(table(factor(ola, levels = names(waves))))

dic <- do.call(rbind, lapply(names(df), function(v) {
  x     <- df[[v]]
  valid <- is_valid(x)

  n_wave <- as.vector(tapply(valid, factor(ola, levels = names(waves)), sum))
  n_wave[is.na(n_wave)] <- 0
  avail  <- n_wave >= min_valid

  obs_id <- rowsum(as.integer(valid), id)[, 1]
  vl     <- value_labels(x)
  lab    <- attr(x, "label", exact = TRUE)

  out <- data.frame(
    variable          = v,
    label             = if (is.null(lab)) NA_character_ else as.character(lab),
    r_class           = class(x)[1],
    type_inferred     = infer_type(x),
    n_categories      = if (length(vl) > 0) length(vl) else NA_integer_,
    answer_codes      = if (length(vl) > 0) paste(unname(vl), collapse = "|") else NA_character_,
    answer_categories = if (length(vl) > 0) paste(names(vl), collapse = "|") else NA_character_,
    n_waves           = sum(avail),
    waves_available   = paste(waves[avail], collapse = ", "),
    max_consecutive   = max_run(avail),
    n_resp_1plus      = sum(obs_id >= 1),
    n_resp_3plus      = sum(obs_id >= 3),
    n_resp_5plus      = sum(obs_id >= 5),
    n_resp_all7       = sum(obs_id >= 7),
    stringsAsFactors  = FALSE
  )
  wave_cols <- as.data.frame(t(n_wave))
  names(wave_cols) <- paste0("n_", waves)
  cbind(out, wave_cols)
}))

# Merge with official variable list (by longitudinal code) ---------------------
ref <- data.frame(
  variable            = ref_es$codigo_longitudinal,
  label_en            = ref_en$label[match(ref_es$codigo_longitudinal, ref_en$longitudinal_code)],
  module              = ref_es$modulo,
  general_concept     = ref_es$concepto_general,
  specific_concept    = ref_es$Concepto_especifico,
  question_wording    = ref_es$fraseo_pregunta,
  item_wording        = ref_es$fraseo_item,
  type_listado        = ref_es$tipo_variable,
  notes_listado       = ref_es$Observaciones,
  stringsAsFactors    = FALSE
)
ref <- ref[!duplicated(ref$variable), ]

dic <- merge(dic, ref, by = "variable", all.x = TRUE, sort = FALSE)
dic <- dic[match(names(df), dic$variable), ]

dic$in_listado    <- ifelse(is.na(dic$module), "No", "Si")
dic$variable_type <- ifelse(is.na(dic$type_listado), dic$type_inferred, dic$type_listado)
dic$type_source   <- ifelse(is.na(dic$type_listado), "Inferido", "Listado")
dic$n             <- seq_len(nrow(dic))

wave_n_cols <- paste0("n_", waves)
dic <- dic[, c("n", "variable", "label", "label_en", "module", "general_concept",
               "specific_concept", "variable_type", "type_source", "n_categories",
               "answer_codes", "answer_categories", "n_waves", "waves_available",
               "max_consecutive", wave_n_cols, "n_resp_1plus", "n_resp_3plus",
               "n_resp_5plus", "n_resp_all7", "question_wording", "item_wording",
               "r_class", "in_listado", "notes_listado")]
rownames(dic) <- NULL

dic3 <- dic[dic$n_waves >= 3, ]

# Summary by module ------------------------------------------------------------
mod <- ifelse(is.na(dic$module), "(no en Listado)", dic$module)
summ <- do.call(rbind, lapply(split(dic, mod), function(d) data.frame(
  module         = ifelse(is.na(d$module[1]), "(no en Listado)", d$module[1]),
  vars_total     = nrow(d),
  vars_3plus     = sum(d$n_waves >= 3),
  vars_5plus     = sum(d$n_waves >= 5),
  vars_all7      = sum(d$n_waves == 7),
  vars_3plus_consecutive = sum(d$max_consecutive >= 3)
)))
summ <- summ[order(-summ$vars_3plus), ]
summ <- rbind(summ, data.frame(module = "TOTAL", t(colSums(summ[, -1]))))

notes <- data.frame(Nota = c(
  "Fuente: ELSOC_Long_2016_2023_1.00.RData (formato largo, 7 olas: 2016, 2017, 2018, 2019, 2021, 2022, 2023).",
  paste0("Casos validos: se excluyen NA y codigos de perdido ", paste(miss_codes, collapse = ", "),
         " (No responde, No sabe, error tecnico, encuesta incompleta)."),
  paste0("Una ola cuenta como 'disponible' si la variable tiene al menos ", min_valid, " casos validos en esa ola."),
  paste0("n_YYYY: numero de casos validos en cada ola (N total por ola: ",
         paste(paste0(waves, "=", n_by_wave), collapse = ", "), ")."),
  "n_waves: numero de olas disponibles. waves_available: anios de esas olas.",
  "max_consecutive: mayor numero de olas consecutivas disponibles (2019 y 2021 se consideran consecutivas; no hubo ola 2020).",
  "n_resp_Kplus: numero de participantes (idencuesta) con al menos K observaciones validas de la variable.",
  "variable_type: tipo segun Listado-de-Variables-ELSOC-2.xlsx; si la variable no esta en el Listado se infiere de los datos (type_source = 'Inferido').",
  "answer_codes / answer_categories: etiquetas de valor presentes en la base, sin codigos de perdido.",
  "Hoja 'Vars 3+ olas': subconjunto con n_waves >= 3."
))

# Write Excel ------------------------------------------------------------------
wb <- openxlsx::createWorkbook()
hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DCE6F1", border = "bottom")

add_sheet <- function(name, data) {
  openxlsx::addWorksheet(wb, name)
  openxlsx::writeData(wb, name, data, headerStyle = hs)
  openxlsx::freezePane(wb, name, firstActiveRow = 2, firstActiveCol = if ("variable" %in% names(data)) 3 else 1)
  openxlsx::addFilter(wb, name, rows = 1, cols = seq_along(data))
  openxlsx::setColWidths(wb, name, cols = seq_along(data), widths = "auto")
}
add_sheet("Vars 3+ olas", dic3)
add_sheet("Todas las variables", dic)
add_sheet("Resumen por modulo", summ)
add_sheet("Notas", notes)
for (s in c("Vars 3+ olas", "Todas las variables")) {
  openxlsx::setColWidths(wb, s, cols = which(names(dic) %in% c("label", "label_en", "answer_categories",
                                                               "question_wording", "item_wording", "notes_listado")),
                         widths = 45)
}

openxlsx::saveWorkbook(wb, here::here("output/tables/ELSOC_variable_dictionary_2016_2023.xlsx"), overwrite = TRUE)
