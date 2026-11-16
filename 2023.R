# ============================================================
# Energy Transition Discourse – Full R Pipeline
# Bundestag plenary speeches 2023 & 2024
# ============================================================

# ---------- Packages ----------
pkgs <- c("tidyverse", "xml2", "httr", "stringr")
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}
invisible(lapply(pkgs, install_if_missing))

# ---------- Folders ----------
dirs <- c("data/raw", "data/processed", "outputs/figures", "outputs/tables")
walk(dirs, ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))

# ---------- 1. Download one protocol ----------
download_protocol <- function(period, session) {
  url <- sprintf("https://dserver.bundestag.de/btp/%d/%d%03d.xml",
                 period, period, session)
  resp <- tryCatch(
    GET(url, timeout(20), user_agent("Mozilla/5.0")),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  raw <- content(resp, as = "raw")
  if (length(raw) < 10000) return(NULL)
  raw
}

# ---------- 2. Parse speeches ----------
parse_speeches <- function(xml_raw, period, session, year) {
  doc <- tryCatch(read_xml(xml_raw), error = function(e) NULL)
  if (is.null(doc)) return(tibble())
  
  reden <- xml_find_all(doc, ".//rede | .//*[local-name()='rede']")
  if (length(reden) == 0) return(tibble())
  
  map_dfr(reden, function(node) {
    paras <- xml_find_all(node, ".//p | .//*[local-name()='p']")
    text <- paste(xml_text(paras), collapse = " ") %>% str_squish()
    if (nchar(text) < 200) return(NULL)
    
    redner <- xml_find_first(node, ".//redner | .//*[local-name()='redner']")
    name <- faction <- ""
    if (length(redner) > 0 && !is.na(redner)) {
      n <- xml_find_first(redner, ".//name | .//*[local-name()='name'] | .//nachname")
      f <- xml_find_first(redner, ".//fraktion | .//*[local-name()='fraktion']")
      if (length(n) > 0 && !is.na(n)) name <- xml_text(n)
      if (length(f) > 0 && !is.na(f)) faction <- xml_text(f)
    }
    
    tibble(
      text = text,
      speaker_name = name,
      faction = faction,
      protocol_number = paste0(period, "/", session),
      year = year
    )
  })
}

# ---------- 3. Collect a year ----------
collect_year <- function(period, sessions, year) {
  results <- list()
  for (s in sessions) {
    xml <- download_protocol(period, s)
    if (!is.null(xml)) {
      sp <- parse_speeches(xml, period, s, year)
      if (nrow(sp) > 0) {
        results[[length(results) + 1]] <- sp
        message(sprintf("%d/%03d: %d speeches", period, s, nrow(sp)))
      }
    }
    Sys.sleep(0.3)
  }
  bind_rows(results)
}

# ---------- 4. Download both years ----------
message("Collecting 2023...")
df_2023 <- collect_year(20, 80:179, 2023)
write_csv(df_2023, "data/raw/speeches_2023.csv")

message("Collecting 2024...")
df_2024 <- collect_year(20, 180:214, 2024)
write_csv(df_2024, "data/raw/speeches_2024.csv")

# ---------- 5. Clean faction ----------
clean_faction <- function(f) {
  if (is.na(f) || str_trim(f) == "") return("Unknown")
  f <- str_replace_all(f, "[\n\u00a0]+", " ") %>% str_squish()
  up <- str_to_upper(f)
  if (str_detect(up, "GRÜNEN|GRUENEN")) return("BÜNDNIS 90/DIE GRÜNEN")
  if (str_detect(up, "LINKE")) return("Die Linke")
  if (str_detect(up, "FRAKTIONSLOS")) return("fraktionslos")
  f
}

# ---------- 6. High-precision filter ----------
energy_regex <- regex(
  paste(c("energiewende", "strompreis", "energiepreis", "wärmepumpe",
          "heizungsgesetz", "gebäudeenergiegesetz", "wasserstoff",
          "photovoltaik", "windenergie", "netzausbau",
          "erneuerbare[n]?\\s+energien", "klimaschutz",
          "energieversorgung", "energiesicherheit",
          "gaskraftwerk", "biogas", "ccs"),
        collapse = "|"),
  ignore_case = TRUE
)

apply_hp_filter <- function(df) {
  df %>%
    mutate(
      faction = map_chr(faction, clean_faction),
      energy_hits = str_count(str_to_lower(text), energy_regex)
    ) %>%
    filter(energy_hits >= 2)
}

hp_2023 <- apply_hp_filter(df_2023)
hp_2024 <- apply_hp_filter(df_2024)

write_csv(hp_2023, "data/processed/hp_2023.csv")
write_csv(hp_2024, "data/processed/hp_2024.csv")

message("High-precision 2023: ", nrow(hp_2023))
message("High-precision 2024: ", nrow(hp_2024))

# ---------- 7. Keyword shares (comparable percentages) ----------
key_terms <- c("wasserstoff", "heizungsgesetz", "strompreis",
               "energiepreis", "energiewende", "wärmepumpe")

share_table <- map_dfr(key_terms, function(term) {
  tibble(
    term = term,
    pct_2023 = mean(str_detect(str_to_lower(hp_2023$text), term), na.rm = TRUE) * 100,
    pct_2024 = mean(str_detect(str_to_lower(hp_2024$text), term), na.rm = TRUE) * 100
  )
})
print(share_table)
write_csv(share_table, "outputs/tables/keyword_shares.csv")

# ---------- 8. Bootstrap CIs ----------
boot_share <- function(df, term, n_boot = 1000) {
  contains <- str_detect(str_to_lower(df$text), fixed(term))
  reps <- replicate(n_boot, mean(sample(contains, replace = TRUE)) * 100)
  quantile(reps, c(0.025, 0.5, 0.975), na.rm = TRUE)
}

boot_table <- map_dfr(key_terms, function(term) {
  ci23 <- boot_share(hp_2023, term)
  ci24 <- boot_share(hp_2024, term)
  tibble(
    term = term,
    pct_2023 = ci23[2], low_2023 = ci23[1], high_2023 = ci23[3],
    pct_2024 = ci24[2], low_2024 = ci24[1], high_2024 = ci24[3]
  )
})
write_csv(boot_table, "outputs/tables/keyword_shares_bootstrap.csv")

# ---------- 9. Party-specific shares ----------
main_parties <- c("SPD", "CDU/CSU", "BÜNDNIS 90/DIE GRÜNEN", "FDP", "AfD")

party_term_share <- function(df, term) {
  df %>%
    filter(faction %in% main_parties) %>%
    group_by(faction) %>%
    summarise(
      n = n(),
      pct = mean(str_detect(str_to_lower(text), term), na.rm = TRUE) * 100,
      .groups = "drop"
    )
}

# Example for Heizungsgesetz
party_heizung <- bind_rows(
  party_term_share(hp_2023, "heizungsgesetz") %>% mutate(year = 2023),
  party_term_share(hp_2024, "heizungsgesetz") %>% mutate(year = 2024)
)
write_csv(party_heizung, "outputs/tables/heizungsgesetz_by_party.csv")

# ---------- 10. Comparison plot ----------
plot_data <- share_table %>%
  pivot_longer(starts_with("pct"), names_to = "year", values_to = "pct") %>%
  mutate(year = str_remove(year, "pct_"))

ggplot(plot_data, aes(term, pct, fill = year)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "Share of high-precision speeches containing each term",
    subtitle = "Identical collection and filter; percentages within each year",
    y = "Percentage of speeches",
    x = NULL,
    fill = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("outputs/figures/keyword_shares_2023_2024.png",
       width = 8, height = 5, dpi = 150)

# ---------- 11. Simple framing cue counts (optional) ----------
negative_cues <- c("gescheitert", "teuer", "ruin", "ideolog", "zwang",
                   "versagen", "unbezahlbar", "zerstör")
positive_cues <- c("notwendig", "erfolgreich", "sicher", "chance",
                   "fortschritt", "bezahlbar")

cue_rate <- function(df, cues) {
  pattern <- paste(cues, collapse = "|")
  mean(str_detect(str_to_lower(df$text), pattern), na.rm = TRUE) * 100
}

cue_summary <- tibble(
  year = c(2023, 2024),
  pct_negative_cues = c(cue_rate(hp_2023, negative_cues),
                        cue_rate(hp_2024, negative_cues)),
  pct_positive_cues = c(cue_rate(hp_2023, positive_cues),
                        cue_rate(hp_2024, positive_cues))
)
print(cue_summary)

message("Done. Results in outputs/")