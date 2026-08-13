# =============================================================================
# DMT – Simulation von Demo-Daten für 10 VPn × 10 Items
# =============================================================================
# Simuliert psychTestR-kompatible Ergebnisse im Format, das DMT erzeugt.
# Jedes Item kann 1–4 Versuche haben; Antworten bestehen aus res_summary,
# global_correct und correct_answer – analog zu dmt_get_answer().
# =============================================================================

library(dplyr)
library(tibble)
library(tidyr)
library(purrr)

set.seed(42)

# -----------------------------------------------------------------------------
# Parameter
# -----------------------------------------------------------------------------
N_VP        <- 10L   # Anzahl Versuchspersonen
N_TRIALS    <- 10L   # Anzahl Items pro VP
MAX_ATTEMPT <- 4L    # Maximal-Versuche pro Item (wie in while_logic)
INST_LEVELS <- c("HiHat", "Snare", "Kick")

# Fähigkeitsverteilung der VPn (steuert Trefferwahrscheinlichkeit)
# theta ∈ [-2, 2]; höher = kompetenter
vp_theta <- rnorm(N_VP, mean = 0, sd = 1) |> round(2)

# Item-Schwierigkeiten (analog zu Complexity-Werten im DMT)
item_difficulty <- runif(N_TRIALS, min = -1.5, max = 1.5) |> round(2)

# Stimulusnamen: erst Easy, dann Normal (entspricht Stratified Sampling)
item_stimulus_ids <- c(
  paste0("Easy_", sample(1:20, 5)),
  paste0("Main_", sample(1:36, 5))
) |> sample()  # zufällige Reihenfolge innerhalb der VP

# -----------------------------------------------------------------------------
# Hilfsfunktion: Korrektheitswkeit für ein Item gegeben theta und difficulty
# -----------------------------------------------------------------------------
p_correct <- function(theta, difficulty) {
  # Logistische Funktion (1PL-artig)
  plogis(theta - difficulty)
}

# -----------------------------------------------------------------------------
# Hilfsfunktion: Simuliere res_summary für einen Versuch
# Jedes Instrument hat 16 Beat-Positionen; Fehler sind bernoulli-verteilt.
# -----------------------------------------------------------------------------
simulate_res_summary <- function(theta, difficulty) {
  # Je schwieriger das Item und geringer die Fähigkeit, desto mehr Fehler
  error_rate <- 1 - plogis(theta - difficulty + 1)  # etwas milder

  map_dfr(INST_LEVELS, function(inst) {
    n_mistakes <- rbinom(1, size = 16L, prob = error_rate)
    proportion_correct <- (16L - n_mistakes) / 16L
    tibble(
      Instrument        = inst,
      ProportionCorrect = proportion_correct,
      NoMistakes        = n_mistakes
    )
  }) |>
    mutate(Instrument = factor(Instrument, levels = INST_LEVELS)) |>
    arrange(Instrument)
}

# -----------------------------------------------------------------------------
# Hilfsfunktion: Simuliere correct_answer für ein Stimulus
# (Zufälliges Drum-Pattern mit 3–8 Noten pro Instrument)
# -----------------------------------------------------------------------------
simulate_correct_answer <- function() {
  map_dfr(INST_LEVELS, function(inst) {
    n_beats <- sample(2:8, 1)
    beats   <- sort(sample(1:16, n_beats))
    tibble(
      Instrument             = inst,
      BeatPositionSixteenth  = beats
    )
  })
}

# -----------------------------------------------------------------------------
# Kern-Simulation: Eine VP bearbeitet alle N_TRIALS Items
# -----------------------------------------------------------------------------
simulate_vp <- function(vp_id, theta) {

  map_dfr(seq_len(N_TRIALS), function(trial_no) {

    difficulty   <- item_difficulty[trial_no]
    stimulus_id  <- item_stimulus_ids[trial_no]
    correct_ans  <- simulate_correct_answer()

    # While-loop analog zu while_logic(): stoppe bei Erfolg oder nach 4 Versuchen
    attempt        <- 1L
    trial_results  <- list()

    repeat {
      # Ist dieser Versuch korrekt?
      global_correct <- runif(1) < p_correct(theta, difficulty)

      res_summary <- if (global_correct) {
        # Bei Erfolg: keine Fehler
        tibble(
          Instrument        = factor(INST_LEVELS, levels = INST_LEVELS),
          ProportionCorrect = 1,
          NoMistakes        = 0L
        ) |> arrange(Instrument)
      } else {
        simulate_res_summary(theta, difficulty)
      }

      # Ergebnis für diesen Versuch speichern
      result_name <- paste0("DMT_trial_", trial_no, "_attempt_", attempt)

      trial_results[[result_name]] <- list(
        vp_id          = vp_id,
        trial_no       = trial_no,
        attempt        = attempt,
        stimulus_id    = stimulus_id,
        difficulty     = difficulty,
        theta          = theta,
        global_correct = global_correct,
        res_summary    = res_summary,
        correct_answer = correct_ans
      )

      # Stopp-Bedingung (identisch mit while_logic)
      if (global_correct || attempt >= MAX_ATTEMPT) break
      attempt <- attempt + 1L
    }

    # Flatten zu einem tibble (eine Zeile pro Versuch)
    map_dfr(names(trial_results), function(rname) {
      r <- trial_results[[rname]]

      # Instrument-Fehler breit
      mistakes_wide <- r$res_summary |>
        select(Instrument, NoMistakes) |>
        pivot_wider(names_from = Instrument, values_from = NoMistakes,
                    names_prefix = "Mistakes_")

      bind_cols(
        tibble(
          VP             = r$vp_id,
          TrialNo        = r$trial_no,
          Attempt        = r$attempt,
          ResultLabel    = rname,
          StimulusId     = r$stimulus_id,
          Difficulty     = r$difficulty,
          Theta          = r$theta,
          GlobalCorrect  = r$global_correct
        ),
        mistakes_wide
      )
    })
  })
}

# -----------------------------------------------------------------------------
# Alle VPn simulieren
# -----------------------------------------------------------------------------
sim_data_long <- map_dfr(
  seq_len(N_VP),
  ~ simulate_vp(
    vp_id = paste0("VP_", sprintf("%02d", .x)),
    theta = vp_theta[.x]
  )
)

# -----------------------------------------------------------------------------
# Zusammenfassung auf Trial-Ebene (letzter Versuch entscheidet)
# -----------------------------------------------------------------------------
sim_data_trial <- sim_data_long |>
  group_by(VP, TrialNo) |>
  slice_max(Attempt, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(VP, TrialNo, StimulusId, Difficulty, Theta,
         Attempt, GlobalCorrect,
         starts_with("Mistakes_"))

# -----------------------------------------------------------------------------
# Zusammenfassung auf VP-Ebene
# -----------------------------------------------------------------------------
sim_data_vp <- sim_data_trial |>
  group_by(VP, Theta) |>
  summarise(
    N_Items          = n(),
    N_Correct        = sum(GlobalCorrect),
    PctCorrect       = mean(GlobalCorrect) * 100,
    MeanAttempts     = mean(Attempt),
    MeanMistakes_HiHat = mean(Mistakes_HiHat),
    MeanMistakes_Snare = mean(Mistakes_Snare),
    MeanMistakes_Kick  = mean(Mistakes_Kick),
    .groups = "drop"
  ) |>
  arrange(desc(PctCorrect))

# -----------------------------------------------------------------------------
# Ausgabe
# -----------------------------------------------------------------------------
message("=== Simulierte Rohdaten (alle Versuche) ===")
print(sim_data_long, n = 20)

message("\n=== Trial-Ebene (letzter Versuch pro Item) ===")
print(sim_data_trial, n = 20)

message("\n=== VP-Ebene Zusammenfassung ===")
print(sim_data_vp)

# Optional: als CSV speichern
# write.csv(sim_data_long,  "dmt_sim_raw.csv",   row.names = FALSE)
# write.csv(sim_data_trial, "dmt_sim_trial.csv", row.names = FALSE)
# write.csv(sim_data_vp,    "dmt_sim_vp.csv",    row.names = FALSE)


# =============================================================================
# ISLP-Modell (Yu & Douglas, 2023) für Dynamic Assessment
# Angepasst auf den Drum Machine Test (DMT)
# =============================================================================
#
# Annahmen / Vereinfachungen (bitte lesen):
# - Wachstum erfolgt pro ATTEMPT (nicht nur pro Item), da der DMT vier
#   Feedback-Layer pro Item hat: D(t) = D_i * gamma_layer[attempt]
# - Die drei Dimensionen (HiHat, Snare, Kick) werden separat geschätzt
#   (3 unabhängige MCMC-Läufe) und erst danach über Ladungen zu einer
#   generellen Fähigkeit xi aggregiert (Higher-Order-Idee, aber nicht
#   in einem gemeinsamen hierarchischen Sampler geschätzt).
# - gamma_layer ist standardmäßig FIX (bekannt), da D_i und gamma_layer
#   multiplikativ konfundiert sind (Identifizierbarkeitsproblem). Optional
#   mitschätzbar mit gamma_layer[1] = 1 als Ankerrestriktion.
# - b_j (Item-Schwierigkeit) kann direkt geschätzt ODER über eine
#   Q-Matrix via LLTM strukturiert werden (b_j = Q %*% eta).
#
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

set.seed(42)

# =============================================================================
# 1. MESSMODELL: 1PL (Rasch)
# =============================================================================

p_1pl <- function(theta, b) {
  plogis(theta - b)
}

# =============================================================================
# 2. Q-MATRIX / LLTM: Item-Schwierigkeit aus musikalischen Merkmalen
# =============================================================================
# Q-Matrix Beispiel für DMT-Items: Zeilen = Items, Spalten = Operationen/Merkmale
# z.B. "OffBeat_Bassdrum", "Snare_Density", "HiHat_Density", "Syncopation"
#
# b_j = Q %*% eta  (LLTM: additive Schwierigkeitskomponenten)

lltm_b <- function(Q, eta) {
  as.numeric(Q %*% eta)
}

make_example_Q_matrix <- function(T_items) {
  # Zufällige binäre Q-Matrix mit 4 musikalischen Operationen als Beispiel
  ops <- c("OffBeat_Bassdrum", "Snare_Density", "HiHat_Density", "Syncopation")
  Q <- matrix(
    rbinom(T_items * length(ops), 1, 0.4),
    nrow = T_items, ncol = length(ops),
    dimnames = list(NULL, ops)
  )
  Q
}

# =============================================================================
# 3. ISLP-WACHSTUMSMODELL (rekursiv)
# =============================================================================
# theta_{i,t+1} = theta_{i,t} + D_i(t) * exp(-|theta_{i,t} - b_j|)
#
# D_i(t) = D_i * gamma_layer[attempt]
#
# Gibt für eine Person + eine Dimension die theta-Werte VOR jedem Attempt
# zurück (das sind die Werte, die die Antwortwahrscheinlichkeit steuern),
# sowie den finalen theta-Wert nach dem letzten Attempt.

islp_person_dim_path <- function(theta_start, D_i, gamma_layer,
                                 item_seq, b_vec, max_attempts = 4,
                                 p_correct_fun = p_1pl,
                                 simulate = TRUE, observed_correct = NULL) {

  theta_cur <- theta_start
  out <- list()
  row_i <- 1L

  for (j in seq_along(item_seq)) {

    item_idx <- item_seq[j]
    b_j <- b_vec[item_idx]

    for (attempt in seq_len(max_attempts)) {

      layer <- min(attempt, length(gamma_layer))
      theta_before <- theta_cur
      p <- p_correct_fun(theta_before, b_j)
      p <- min(max(p, 1e-6), 1 - 1e-6)

      if (simulate) {
        correct <- rbinom(1, 1, p)
      } else {
        correct <- observed_correct[row_i]
      }

      out[[row_i]] <- tibble::tibble(
        item_idx     = item_idx,
        attempt      = attempt,
        layer        = layer,
        b_item       = b_j,
        theta_before = theta_before,
        p_correct    = p,
        correct      = correct
      )
      row_i <- row_i + 1L

      # Wachstum nach Feedback (ZPD-gedämpft)
      D_t <- D_i * gamma_layer[layer]
      theta_cur <- theta_cur + D_t * exp(-abs(theta_cur - b_j))

      if (correct == 1L) break
    }
  }

  dplyr::bind_rows(out) %>%
    dplyr::mutate(theta_final = theta_cur)
}

# =============================================================================
# 4. DATENSIMULATION (angepasst an DMT-Design)
# =============================================================================

simulate_dmt_islp <- function(N = 10,
                              T_items = 10,
                              dims = c("HiHat", "Snare", "Kick"),
                              theta_start_mean = 0, theta_start_sd = 1,
                              D_mean = 0.35, D_sd = 0.15,
                              gamma_layer = c(0.5, 0.8, 1.0, 1.3),  # Layer 1..4
                              b_range = c(-1.5, 1.5),
                              dim_loadings = c(HiHat = 0.80, Snare = 0.85, Kick = 0.75),
                              max_attempts = 4,
                              use_lltm = FALSE,
                              Q = NULL, eta = NULL,
                              seed = 42) {

  set.seed(seed)

  # Item-Schwierigkeiten: entweder direkt oder via LLTM/Q-Matrix
  if (use_lltm) {
    if (is.null(Q)) Q <- make_example_Q_matrix(T_items)
    if (is.null(eta)) eta <- rnorm(ncol(Q), 0, 0.5)
    b_items <- lltm_b(Q, eta)
  } else {
    Q <- NULL; eta <- NULL
    b_items <- sort(runif(T_items, b_range[1], b_range[2]))
  }

  item_seq <- seq_len(T_items)  # Reihenfolge der Items für alle Personen gleich

  # Personenparameter
  person_params <- tibble::tibble(
    person_id   = paste0("VP_", sprintf("%02d", seq_len(N))),
    theta_start = rnorm(N, theta_start_mean, theta_start_sd),
    D_i         = pmax(rgamma(N, shape = (D_mean^2) / (D_sd^2),
                              rate  = D_mean / (D_sd^2)), 0.01)
  )

  # Für jede Person x jede Dimension die ISLP-Trajektorie simulieren
  sim_data <- purrr::map_dfr(seq_len(N), function(i) {
    pp <- person_params[i, ]

    purrr::map_dfr(dims, function(d) {
      path <- islp_person_dim_path(
        theta_start = pp$theta_start,
        D_i         = pp$D_i,
        gamma_layer = gamma_layer,
        item_seq    = item_seq,
        b_vec       = b_items,
        max_attempts = max_attempts,
        simulate = TRUE
      )
      path$person_id <- pp$person_id
      path$dimension <- d
      path$theta_start_true <- pp$theta_start
      path$D_i_true <- pp$D_i
      path
    })
  })

  list(
    data           = sim_data,
    person_params  = person_params,
    b_items        = b_items,
    gamma_layer    = gamma_layer,
    dim_loadings   = dim_loadings,
    Q = Q, eta = eta,
    max_attempts = max_attempts
  )
}

# =============================================================================
# 5. LOG-LIKELIHOOD (für eine Person, eine Dimension, gegebene Parameter)
# =============================================================================

loglik_person_dim <- function(theta_start, D_i, gamma_layer, b_vec, person_data) {

  theta_cur <- theta_start
  ll <- 0

  # person_data muss nach item_idx, attempt aufsteigend sortiert sein
  for (r in seq_len(nrow(person_data))) {
    row <- person_data[r, ]
    b_j <- b_vec[row$item_idx]

    p <- plogis(theta_cur - b_j)
    p <- min(max(p, 1e-6), 1 - 1e-6)

    ll <- ll + row$correct * log(p) + (1 - row$correct) * log(1 - p)

    layer <- row$layer
    D_t <- D_i * gamma_layer[layer]
    theta_cur <- theta_cur + D_t * exp(-abs(theta_cur - b_j))
  }

  ll
}

# =============================================================================
# 6. MCMC-SCHÄTZUNG (Metropolis-within-Gibbs)
# =============================================================================
# Schätzt pro Dimension: theta_start_i (N), D_i (N), b_j (T_items)
# gamma_layer bleibt standardmäßig fix (Identifizierbarkeit)

run_mcmc_islp_dim <- function(person_data_by_id,   # named list: person_id -> data.frame
                              T_items,
                              gamma_layer,
                              n_iter = 4000,
                              burnin = 1000,
                              thin = 2,
                              prior_theta_sd = 1,
                              prior_b_sd = 1,
                              prior_D_shape = 4, prior_D_rate = 10,
                              prop_sd_theta = 0.25,
                              prop_sd_logD  = 0.15,
                              prop_sd_b     = 0.2,
                              seed = 1) {

  set.seed(seed)

  N <- length(person_data_by_id)
  person_ids <- names(person_data_by_id)

  # Startwerte
  theta_cur <- rnorm(N, 0, 0.5); names(theta_cur) <- person_ids
  D_cur     <- rep(0.3, N);      names(D_cur) <- person_ids
  b_cur     <- rnorm(T_items, 0, 0.5)

  n_keep <- floor((n_iter - burnin) / thin)
  theta_samples <- matrix(NA, n_keep, N, dimnames = list(NULL, person_ids))
  D_samples     <- matrix(NA, n_keep, N, dimnames = list(NULL, person_ids))
  b_samples     <- matrix(NA, n_keep, T_items)

  keep_i <- 0L

  loglik_full <- function(theta_start, D_i, b_vec, pdata) {
    loglik_person_dim(theta_start, D_i, gamma_layer, b_vec, pdata)
  }

  for (iter in seq_len(n_iter)) {

    # ---- (a) Update theta_start_i und D_i pro Person (M-H) ----
    for (i in seq_len(N)) {

      pdata <- person_data_by_id[[i]]

      ll_old <- loglik_full(theta_cur[i], D_cur[i], b_cur, pdata) +
        dnorm(theta_cur[i], 0, prior_theta_sd, log = TRUE) +
        dgamma(D_cur[i], shape = prior_D_shape, rate = prior_D_rate, log = TRUE)

      # Proposal theta
      theta_prop <- theta_cur[i] + rnorm(1, 0, prop_sd_theta)

      ll_new <- loglik_full(theta_prop, D_cur[i], b_cur, pdata) +
        dnorm(theta_prop, 0, prior_theta_sd, log = TRUE) +
        dgamma(D_cur[i], shape = prior_D_shape, rate = prior_D_rate, log = TRUE)

      if (log(runif(1)) < (ll_new - ll_old)) {
        theta_cur[i] <- theta_prop
      }

      # Proposal D (Random Walk auf log-Skala, D bleibt positiv)
      ll_old_D <- loglik_full(theta_cur[i], D_cur[i], b_cur, pdata) +
        dgamma(D_cur[i], shape = prior_D_shape, rate = prior_D_rate, log = TRUE)

      logD_prop <- log(D_cur[i]) + rnorm(1, 0, prop_sd_logD)
      D_prop <- exp(logD_prop)

      ll_new_D <- loglik_full(theta_cur[i], D_prop, b_cur, pdata) +
        dgamma(D_prop, shape = prior_D_shape, rate = prior_D_rate, log = TRUE)

      # Jacobian für log-Transformation
      if (log(runif(1)) < (ll_new_D - ll_old_D + logD_prop - log(D_cur[i]))) {
        D_cur[i] <- D_prop
      }
    }

    # ---- (b) Update b_j (item-übergreifend über alle Personen) ----
    for (j in seq_len(T_items)) {

      ll_old <- sum(purrr::map_dbl(seq_len(N), ~ loglik_full(
        theta_cur[.x], D_cur[.x], b_cur, person_data_by_id[[.x]]
      ))) + dnorm(b_cur[j], 0, prior_b_sd, log = TRUE)

      b_prop <- b_cur
      b_prop[j] <- b_cur[j] + rnorm(1, 0, prop_sd_b)

      ll_new <- sum(purrr::map_dbl(seq_len(N), ~ loglik_full(
        theta_cur[.x], D_cur[.x], b_prop, person_data_by_id[[.x]]
      ))) + dnorm(b_prop[j], 0, prior_b_sd, log = TRUE)

      if (log(runif(1)) < (ll_new - ll_old)) {
        b_cur <- b_prop
      }
    }

    # ---- (c) Speichern nach Burn-in / Thinning ----
    if (iter > burnin && (iter - burnin) %% thin == 0) {
      keep_i <- keep_i + 1L
      theta_samples[keep_i, ] <- theta_cur
      D_samples[keep_i, ] <- D_cur
      b_samples[keep_i, ] <- b_cur
    }

    if (iter %% 500 == 0) message("MCMC Iteration ", iter, " / ", n_iter)
  }

  list(
    theta_samples = theta_samples,
    D_samples     = D_samples,
    b_samples     = b_samples,
    posterior_mean = list(
      theta_start = colMeans(theta_samples),
      D_i         = colMeans(D_samples),
      b_items     = colMeans(b_samples)
    )
  )
}

# =============================================================================
# 7. WRAPPER: MCMC über alle drei Dimensionen laufen lassen
# =============================================================================

run_mcmc_islp_all_dims <- function(sim_object, n_iter = 3000, burnin = 800, thin = 2) {

  dims <- unique(sim_object$data$dimension)
  T_items <- length(sim_object$b_items)
  gamma_layer <- sim_object$gamma_layer

  results_by_dim <- purrr::map(dims, function(d) {

    dim_data <- sim_object$data %>% dplyr::filter(dimension == d)

    person_data_by_id <- dim_data %>%
      dplyr::arrange(person_id, item_idx, attempt) %>%
      split(.$person_id)

    message("=== MCMC für Dimension: ", d, " ===")

    run_mcmc_islp_dim(
      person_data_by_id = person_data_by_id,
      T_items = T_items,
      gamma_layer = gamma_layer,
      n_iter = n_iter, burnin = burnin, thin = thin
    )
  })

  names(results_by_dim) <- dims
  results_by_dim
}

# =============================================================================
# 8. AGGREGATION ZU GENERELLER FÄHIGKEIT XI (Higher-Order, post-hoc)
# =============================================================================

aggregate_xi <- function(mcmc_results, dim_loadings) {

  dims <- names(mcmc_results)

  theta_start_mat <- purrr::map(dims, ~ mcmc_results[[.x]]$posterior_mean$theta_start) %>%
    setNames(dims) %>%
    as.data.frame()

  loadings_vec <- dim_loadings[dims]

  xi <- as.matrix(theta_start_mat) %*% loadings_vec / sum(loadings_vec)

  tibble::tibble(
    person_id = rownames(theta_start_mat),
    xi_start  = as.numeric(xi)
  )
}

# =============================================================================
# 9. PARAMETER RECOVERY
# =============================================================================

evaluate_recovery <- function(sim_object, mcmc_results) {

  purrr::imap_dfr(mcmc_results, function(res, dim_name) {

    dim_data <- sim_object$data %>% dplyr::filter(dimension == dim_name)

    true_pars <- dim_data %>%
      dplyr::distinct(person_id, theta_start_true, D_i_true)

    tibble::tibble(
      dimension    = dim_name,
      person_id    = names(res$posterior_mean$theta_start),
      theta_est    = res$posterior_mean$theta_start,
      D_est        = res$posterior_mean$D_i
    ) %>%
      dplyr::left_join(true_pars, by = "person_id")
  })
}

plot_recovery <- function(recovery_df) {

  p_theta <- ggplot(recovery_df, aes(x = theta_start_true, y = theta_est, color = dimension)) +
    geom_point(size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = "Parameter Recovery: theta_start",
         x = "wahres theta_start", y = "geschätztes theta_start") +
    theme_minimal()

  p_D <- ggplot(recovery_df, aes(x = D_i_true, y = D_est, color = dimension)) +
    geom_point(size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = "Parameter Recovery: D (Lernpotenzial)",
         x = "wahres D_i", y = "geschätztes D_i") +
    theme_minimal()

  list(theta_plot = p_theta, D_plot = p_D)
}

# =============================================================================
# 10. VISUALISIERUNG: THETA-TRAJEKTORIEN
# =============================================================================

plot_theta_trajectories <- function(sim_object, persons = NULL) {

  d <- sim_object$data

  if (!is.null(persons)) {
    d <- d %>% dplyr::filter(person_id %in% persons)
  }

  d <- d %>%
    dplyr::group_by(person_id, dimension) %>%
    dplyr::mutate(step = dplyr::row_number()) %>%
    dplyr::ungroup()

  ggplot(d, aes(x = step, y = theta_before, color = person_id)) +
    geom_line(alpha = 0.7) +
    geom_point(aes(shape = factor(correct)), size = 1.8) +
    geom_hline(aes(yintercept = b_item), linetype = "dotted", alpha = 0.15) +
    facet_wrap(~dimension) +
    scale_shape_manual(values = c(`0` = 4, `1` = 16), name = "Korrekt") +
    labs(
      title = "ISLP theta-Trajektorien über Attempts, je Dimension",
      subtitle = "Punktierte Linien = jeweilige Item-Schwierigkeit b_j",
      x = "Attempt-Sequenz (kumulativ über Items)",
      y = "theta (vor jedem Attempt)"
    ) +
    theme_minimal()
}

# =============================================================================
# 11. BEISPIEL-WORKFLOW
# =============================================================================

# --- Simulation ---
sim <- simulate_dmt_islp(
  N = 10, T_items = 10,
  theta_start_mean = 0, theta_start_sd = 1,
  D_mean = 0.35, D_sd = 0.15,
  gamma_layer = c(0.5, 0.8, 1.0, 1.3),
  b_range = c(-1.5, 1.5),
  use_lltm = FALSE,
  seed = 42
)

# --- Visualisierung der Trajektorien (vor Schätzung) ---
plot_theta_trajectories(sim)

# --- MCMC-Schätzung (Achtung: n_iter für echte Analyse deutlich höher wählen,
#     hier aus Laufzeitgründen bewusst klein gehalten) ---
mcmc_results <- run_mcmc_islp_all_dims(sim, n_iter = 1500, burnin = 500, thin = 2)

# --- Higher-Order Aggregation zu xi ---
xi_scores <- aggregate_xi(mcmc_results, sim$dim_loadings)
print(xi_scores)

# --- Parameter Recovery ---
recovery_df <- evaluate_recovery(sim, mcmc_results)
print(recovery_df)

recovery_plots <- plot_recovery(recovery_df)
recovery_plots$theta_plot
recovery_plots$D_plot

# Korrelation als schneller Recovery-Check
recovery_df %>%
  dplyr::group_by(dimension) %>%
  dplyr::summarise(
    cor_theta = cor(theta_start_true, theta_est),
    cor_D     = cor(D_i_true, D_est)
  )
