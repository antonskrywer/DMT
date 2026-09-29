# analysis/pilot_kids_analysis.R
#
# Auswertung Mini-Pilot mit Kindern (Konfiguration: test_apps/DMT_pilot/app.R).
# Beantwortet zwei Fragen:
#   1. Timing: Wie lange brauchen Kinder fuer Intro, Uebung und Haupttest,
#      und wie viele Haupt-Trials passen damit in 15 min?
#   2. Itemniveau: Bei welchem Versuch wird geloest? Ziel: meist bei
#      Versuch 2-3, weder Decke (Versuch 1) noch Boden (nicht geloest) -
#      insgesamt und pro Schwierigkeits-Stratum.
#
# Benutzung:
#   1. Ergebnis-.rds-Dateien vom Server holen (Admin-Panel -> "all rds",
#      ZIP entpacken) und den Ordner unten bei `results_dir` eintragen.
#   2. Im Paket-Ordner ausfuehren:
#        & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" analysis/pilot_kids_analysis.R
#   3. Ausgabe in der Konsole; Tabellen zusaetzlich als CSV in `out_dir`.

results_dir <- "pilot_results"           # <- Ordner mit den .rds-Dateien
out_dir     <- "analysis/output"         # CSV-Ausgabe
budget_min  <- 15                        # Zeitbudget gesamt (min)

suppressMessages({
  devtools::load_all(".", quiet = TRUE)  # nutzt die Funktionen im Repo-Stand
  library(dplyr)
  library(tidyr)
})
logging::setLevel("WARN")

stopifnot(dir.exists(results_dir))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------
# Daten laden
# ---------------------------------------------------------------------
long   <- DMT_results_dir_to_long(results_dir, include_demo = TRUE)
phases <- DMT_phase_durations(results_dir)

# Stratum-Label (easy_easy / easy_hard / normal_easy / normal_hard) aus
# source + complexity_half: innerhalb jeder Quelle ist die Haelfte mit dem
# niedrigeren Complexity-Mittel "_easy" (wie resolve_stratum_allocation()).
stratum_lookup <- bind_rows(
  easy_stimuli_drum_matrix %>% mutate(source = "easy"),
  drum_matrix %>% mutate(source = "normal")
) %>%
  group_by(source, complexity_half = ComplexityHalves) %>%
  summarise(m = mean(Complexity), .groups = "drop_last") %>%
  mutate(stratum = paste0(source, "_", ifelse(m == min(m), "easy", "hard"))) %>%
  ungroup() %>%
  select(source, complexity_half, stratum)

main <- long %>%
  filter(!(demo %in% TRUE)) %>%
  left_join(stratum_lookup, by = c("source", "complexity_half"))

# ---------------------------------------------------------------------
# Trial-Ebene: bei welchem Versuch geloest? Wie lange hat der Trial gedauert?
# ---------------------------------------------------------------------
trials <- main %>%
  group_by(p_id, trial_no, stimulus_id, stratum, complexity) %>%
  arrange(attempt, .by_group = TRUE) %>%
  summarise(
    n_attempts     = n(),
    solved         = any(global_correct %in% TRUE),
    solved_at      = if (any(global_correct %in% TRUE)) min(attempt[global_correct %in% TRUE]) else NA_integer_,
    n_timeouts     = sum(timed_out %in% TRUE),
    trial_sec      = sum(rt_ms, feedback_rt_ms, na.rm = TRUE) / 1000,
    stim_plays     = sum(stim_plays, na.rm = TRUE),
    pattern_plays  = sum(pattern_plays, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Fallback-Score: benoetigte Hilfestufen (0 = sofort geloest,
    # 3 = beim 4. Versuch geloest, 4 = nicht geloest)
    help_needed = ifelse(solved, solved_at - 1L, 4L),
    outcome = factor(
      ifelse(solved, paste0("Versuch ", solved_at), "nicht geloest"),
      levels = c(paste0("Versuch ", 1:4), "nicht geloest")
    )
  )

# ---------------------------------------------------------------------
# 1. Timing
# ---------------------------------------------------------------------
cat("\n==================== 1. TIMING ====================\n")
cat(sprintf("Sessions: %i (davon vollstaendig: %i)\n", nrow(phases), sum(phases$complete %in% TRUE)))

cat("\nPhasendauern pro Kind (min):\n")
print(as.data.frame(mutate(phases, across(ends_with("_min"), ~ round(.x, 1)))))

phase_summary <- phases %>%
  summarise(across(ends_with("_min"), list(
    median = ~ median(.x, na.rm = TRUE),
    min    = ~ suppressWarnings(min(.x, na.rm = TRUE)),
    max    = ~ suppressWarnings(max(.x, na.rm = TRUE))
  ))) %>%
  pivot_longer(everything(), names_to = c("phase", "stat"), names_pattern = "(.*_min)_(.*)") %>%
  pivot_wider(names_from = stat, values_from = value)
cat("\nPhasendauern ueber Kinder (min):\n")
print(as.data.frame(mutate(phase_summary, across(where(is.numeric), ~ round(.x, 1)))))

attempt_timing <- main %>%
  group_by(attempt) %>%
  summarise(
    n                  = n(),
    median_rt_s        = median(rt_ms, na.rm = TRUE) / 1000,
    median_feedback_s  = median(feedback_rt_ms, na.rm = TRUE) / 1000,
    anteil_timeout     = mean(timed_out %in% TRUE),
    median_stim_plays  = median(stim_plays, na.rm = TRUE),
    median_pattern_plays = median(pattern_plays, na.rm = TRUE),
    .groups = "drop"
  )
cat("\nHaupttest pro Versuch (Median):\n")
print(as.data.frame(mutate(attempt_timing, across(where(is.double), ~ round(.x, 2)))))

median_trial_min <- median(trials$trial_sec, na.rm = TRUE) / 60
fixed_min <- with(phase_summary, sum(median[phase %in% c("intro_min", "practice_min", "ready_min")], na.rm = TRUE))
cat(sprintf(
  "\nMedian pro Haupt-Trial: %.1f min | Intro+Uebung+Bereit (Median): %.1f min\n=> Im Budget von %i min passen ca. %.1f Haupt-Trials.\n",
  median_trial_min, fixed_min, budget_min, (budget_min - fixed_min) / median_trial_min
))

# ---------------------------------------------------------------------
# 2. Loesungsverteilung (Itemniveau)
# ---------------------------------------------------------------------
cat("\n============ 2. LOESUNGSVERTEILUNG (Haupttest) ============\n")
cat("Ziel: Schwerpunkt bei Versuch 2-3; Versuch 1 = Decke, nicht geloest = Boden.\n")

dist_table <- function(d) {
  d %>%
    count(outcome, .drop = FALSE) %>%
    mutate(anteil = round(n / sum(n), 2))
}

cat("\nGesamt:\n")
dist_all <- dist_table(trials)
print(as.data.frame(dist_all))

dist_stratum <- trials %>%
  mutate(stratum = factor(stratum, levels = c("easy_easy", "easy_hard", "normal_easy", "normal_hard"))) %>%
  group_by(stratum) %>%
  count(outcome, .drop = FALSE) %>%
  mutate(anteil = round(n / sum(n), 2)) %>%
  ungroup()
cat("\nPro Stratum (Anteile):\n")
print(as.data.frame(
  dist_stratum %>% select(-n) %>% pivot_wider(names_from = outcome, values_from = anteil)
))

target <- trials %>%
  group_by(stratum) %>%
  summarise(
    n_trials         = n(),
    decke_v1         = mean(solved_at %in% 1L),
    ziel_v2_v3       = mean(solved_at %in% 2:3),
    spaet_v4         = mean(solved_at %in% 4L),
    boden_ungeloest  = mean(!solved),
    mittl_hilfe      = mean(help_needed),
    .groups = "drop"
  )
cat("\nZielbereich pro Stratum:\n")
print(as.data.frame(mutate(target, across(where(is.double), ~ round(.x, 2)))))

per_child <- trials %>%
  group_by(p_id) %>%
  summarise(
    n_trials     = n(),
    n_geloest    = sum(solved),
    mittl_hilfe  = round(mean(help_needed), 2),
    summe_hilfe  = sum(help_needed),
    .groups = "drop"
  )
cat("\nPro Kind (Fallback-Score = benoetigte Hilfestufen, 0-4 pro Trial):\n")
print(as.data.frame(per_child))

# ---------------------------------------------------------------------
# CSV-Export
# ---------------------------------------------------------------------
write.csv(phases,         file.path(out_dir, "phases.csv"),          row.names = FALSE)
write.csv(attempt_timing, file.path(out_dir, "attempt_timing.csv"),  row.names = FALSE)
write.csv(trials,         file.path(out_dir, "trials.csv"),          row.names = FALSE)
write.csv(dist_stratum,   file.path(out_dir, "solution_by_stratum.csv"), row.names = FALSE)
write.csv(per_child,      file.path(out_dir, "per_child.csv"),       row.names = FALSE)
cat(sprintf("\nCSV-Dateien geschrieben nach: %s\n", normalizePath(out_dir)))
