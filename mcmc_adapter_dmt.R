# =============================================================================
# mcmc_adapter_dmt.R
#
# Adapter, der DMT-Ergebnisse im Long-Format (DMT_results_to_long() bzw.
# simulate_dmt_results_long()) für die ISLP-MCMC-Schätzung nutzbar macht.
# Baut auf der Sampler-Logik von data_simulation.R (run_mcmc_islp_dim())
# auf, ist aber eigenständig lauffähig.
#
# Mit Anton abgestimmte Modellentscheidungen:
#   - b_j (Itemschwierigkeit) = complexity, FIX aus dem Long-Tibble über-
#     nommen. KEIN Sampling-Schritt für b_j (Senn-Complexity gilt zunächst
#     als gesetzter Schwierigkeitswert; Revision erst nach Kalibrierungs-
#     studie).
#   - "korrekt" pro Dimension (HiHat/Snare/Kick) = binär: 1 nur bei 0
#     Fehlern (<dim>_mistakes == 0), analog zur global_correct-Logik in
#     dmt_get_answer() / complete_instruments().
#   - Zwei Zeitachsen-Varianten, parallel:
#       INTRA:  theta wächst nach JEDEM Attempt, D_t = D_i * gamma_layer[attempt]
#               (klassische ISLP-Extended-Logik aus data_simulation.R).
#       INTER:  alle Attempts eines Trials fließen einzeln in die
#               Likelihood ein (jeder Attempt ist eine eigene Bernoulli-
#               Beobachtung bei KONSTANTEM theta), aber theta wächst nur
#               EINMAL pro Trial (beim Trial-Wechsel), mit
#               gamma_layer[attempt_des_letzten_Attempts_im_Trial]
#               (mehr Attempts/Feedback-Layer im Trial -> größerer Wachs-
#               tumsschritt beim Übergang).
#   - Demo-Trials (demo == TRUE) werden NICHT in die Schätzung einbezogen.
# =============================================================================

library(dplyr)
library(purrr)
library(tibble)

# -----------------------------------------------------------------------
# 1. Daten-Vorbereitung: Long-Tibble -> person_data_by_id (je Dimension,
#    je Variante)
# -----------------------------------------------------------------------

#' Binärer Korrektheits-Indikator pro Dimension
#'
#' BUGFIX (MCMC-Adapter-Kompatibilitaet): verglich frueher hits == n. In
#' echten DMT_results_to_long()-Daten ist <dim>_n aber IMMER 16
#' (NoPositions = Gesamtgroesse des Grids), nicht die Anzahl der fuer
#' dieses Instrument tatsaechlich erforderlichen Onsets - <dim>_hits
#' erreicht das in der Praxis fast nie, auch bei perfekter Antwort. Der
#' Simulator (simulate_dmt_results_long.R) hat das nicht aufgedeckt, weil
#' er hits/n dort intern zueinander konsistent erzeugt. Jetzt stattdessen
#' <dim>_mistakes == 0 (aus res_summary$NoMistakes, siehe results.R),
#' analog zur global_correct-Logik in dmt_get_answer().
#' @keywords internal
dmt_binarize_dim <- function(df, dimension) {
  mistakes_col <- paste0(tolower(dimension), "_mistakes")
  stopifnot(mistakes_col %in% names(df))
  as.integer(df[[mistakes_col]] == 0)
}

#' Long-Tibble EINER Person in die Zeilenstruktur bringen, die der
#' generische Loglik-/Wachstums-Loop braucht: item_idx, attempt, layer,
#' b_item (= complexity, FIX), correct, grow_after.
#'
#' @param df Long-Tibble, bereits auf eine Person gefiltert.
#' @param dimension "HiHat" | "Snare" | "Kick"
#' @param variant "intra" | "inter"
#' @param max_attempts Deckelt die gamma_layer-Indexierung (Default 4,
#'   entspricht der 4-Attempt-while_logic() im DMT).
#' @keywords internal
prepare_person_dim_rows <- function(df, dimension, variant = c("intra", "inter"),
                                    max_attempts = 4L) {

  variant <- match.arg(variant)

  df <- df %>%
    dplyr::filter(!demo) %>%                # Demo-Trials ausschließen
    dplyr::arrange(cumulative_attempt)

  if (nrow(df) == 0) {
    return(tibble::tibble(
      item_idx = integer(), attempt = integer(), layer = integer(),
      b_item = double(), correct = integer(), grow_after = logical()
    ))
  }

  rows <- tibble::tibble(
    item_idx = df$trial_no,
    attempt  = df$attempt,
    layer    = pmin(df$attempt, max_attempts),
    b_item   = df$complexity,
    correct  = dmt_binarize_dim(df, dimension)
  )

  if (variant == "intra") {
    # Wachstum nach JEDEM Attempt
    rows$grow_after <- TRUE
  } else {
    # Wachstum nur beim Trial-Wechsel: TRUE genau bei der letzten Zeile
    # jedes Trials (letzter Attempt, bevor der nächste Trial beginnt)
    rows <- rows %>%
      dplyr::mutate(.row = dplyr::row_number()) %>%
      dplyr::group_by(item_idx) %>%
      dplyr::mutate(grow_after = .row == max(.row)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-.row)
  }

  rows
}

#' Baut die person_data_by_id-Liste für eine Dimension + Variante aus dem
#' kompletten Long-Tibble (mehrere Personen).
#'
#' @param df_long Tibble im DMT_results_to_long()-Format (>= 1 Person).
#' @param dimension "HiHat" | "Snare" | "Kick"
#' @param variant "intra" | "inter"
#' @export
dmt_long_to_person_data <- function(df_long, dimension, variant = c("intra", "inter"),
                                    max_attempts = 4L) {

  variant <- match.arg(variant)

  split(df_long, df_long$p_id) %>%
    purrr::map(
      prepare_person_dim_rows,
      dimension = dimension, variant = variant, max_attempts = max_attempts
    )
}

# -----------------------------------------------------------------------
# 2. Generische Log-Likelihood + Theta-Wachstumspfad (b_j FIX, kein
#    Sampling-Schritt dafür -> gilt für beide Varianten identisch, der
#    Unterschied steckt allein in grow_after aus Schritt 1)
# -----------------------------------------------------------------------

#' @keywords internal
loglik_person_dim_adapter <- function(theta_start, D_i, gamma_layer, person_data) {

  theta_cur <- theta_start
  ll <- 0

  if (nrow(person_data) == 0) return(ll)

  for (r in seq_len(nrow(person_data))) {
    row <- person_data[r, ]

    p <- plogis(theta_cur - row$b_item)
    p <- min(max(p, 1e-6), 1 - 1e-6)

    ll <- ll + row$correct * log(p) + (1 - row$correct) * log(1 - p)

    if (isTRUE(row$grow_after)) {
      layer <- min(row$layer, length(gamma_layer))
      D_t <- D_i * gamma_layer[layer]
      theta_cur <- theta_cur + D_t * exp(-abs(theta_cur - row$b_item))
    }
  }

  ll
}

# -----------------------------------------------------------------------
# 3. MCMC-Schätzung (Metropolis-within-Gibbs) für EINE Dimension.
#    Schätzt NUR theta_start_i und D_i pro Person (b_j ist fix = complexity,
#    daher entfällt der b_j-Update-Schritt aus data_simulation.R komplett).
# -----------------------------------------------------------------------

#' @param person_data_by_id Named list: person_id -> Tibble (aus
#'   dmt_long_to_person_data()).
#' @export
run_mcmc_islp_adapter <- function(person_data_by_id,
                                  gamma_layer = c(0.5, 0.8, 1.0, 1.3),
                                  n_iter = 4000, burnin = 1000, thin = 2,
                                  prior_theta_sd = 1,
                                  prior_D_shape = 4, prior_D_rate = 10,
                                  prop_sd_theta = 0.25,
                                  prop_sd_logD  = 0.15,
                                  seed = 1) {

  set.seed(seed)

  N <- length(person_data_by_id)
  person_ids <- names(person_data_by_id)

  theta_cur <- rnorm(N, 0, 0.5); names(theta_cur) <- person_ids
  D_cur     <- rep(0.3, N);      names(D_cur) <- person_ids

  n_keep <- floor((n_iter - burnin) / thin)
  theta_samples <- matrix(NA, n_keep, N, dimnames = list(NULL, person_ids))
  D_samples     <- matrix(NA, n_keep, N, dimnames = list(NULL, person_ids))

  keep_i <- 0L

  ll_full <- function(theta_start, D_i, pdata) {
    loglik_person_dim_adapter(theta_start, D_i, gamma_layer, pdata)
  }

  for (iter in seq_len(n_iter)) {

    for (i in seq_len(N)) {

      pdata <- person_data_by_id[[i]]

      # --- theta_start Update ---
      ll_old <- ll_full(theta_cur[i], D_cur[i], pdata) +
        dnorm(theta_cur[i], 0, prior_theta_sd, log = TRUE)

      theta_prop <- theta_cur[i] + rnorm(1, 0, prop_sd_theta)

      ll_new <- ll_full(theta_prop, D_cur[i], pdata) +
        dnorm(theta_prop, 0, prior_theta_sd, log = TRUE)

      if (log(runif(1)) < (ll_new - ll_old)) {
        theta_cur[i] <- theta_prop
      }

      # --- D_i Update (Random Walk auf log-Skala, D bleibt positiv) ---
      ll_old_D <- ll_full(theta_cur[i], D_cur[i], pdata) +
        dgamma(D_cur[i], shape = prior_D_shape, rate = prior_D_rate, log = TRUE)

      logD_prop <- log(D_cur[i]) + rnorm(1, 0, prop_sd_logD)
      D_prop <- exp(logD_prop)

      ll_new_D <- ll_full(theta_cur[i], D_prop, pdata) +
        dgamma(D_prop, shape = prior_D_shape, rate = prior_D_rate, log = TRUE)

      # Jacobian für log-Transformation
      if (log(runif(1)) < (ll_new_D - ll_old_D + logD_prop - log(D_cur[i]))) {
        D_cur[i] <- D_prop
      }
    }

    if (iter > burnin && (iter - burnin) %% thin == 0) {
      keep_i <- keep_i + 1L
      theta_samples[keep_i, ] <- theta_cur
      D_samples[keep_i, ] <- D_cur
    }

    if (iter %% 500 == 0) message("  MCMC Iteration ", iter, " / ", n_iter)
  }

  list(
    theta_samples = theta_samples,
    D_samples     = D_samples,
    posterior_mean = list(
      theta_start = colMeans(theta_samples),
      D_i         = colMeans(D_samples)
    )
  )
}

# -----------------------------------------------------------------------
# 4. Wrapper: beide Varianten x alle drei Dimensionen in einem Rutsch
# -----------------------------------------------------------------------

#' @param df_long Long-Tibble (mehrere Personen), z.B. aus
#'   DMT_results_dir_to_long() oder mehreren simulate_dmt_results_long()-
#'   Aufrufen.
#' @export
run_mcmc_islp_all_variants <- function(df_long,
                                       dims = c("HiHat", "Snare", "Kick"),
                                       gamma_layer = c(0.5, 0.8, 1.0, 1.3),
                                       n_iter = 3000, burnin = 800, thin = 2,
                                       max_attempts = 4L,
                                       seed = 1) {

  variants <- c("intra", "inter")

  results <- purrr::map(variants, function(v) {

    message("=== Variante: ", v, " ===")

    dim_results <- purrr::map(dims, function(d) {

      message("--- Dimension: ", d, " ---")

      person_data_by_id <- dmt_long_to_person_data(
        df_long, dimension = d, variant = v, max_attempts = max_attempts
      )

      run_mcmc_islp_adapter(
        person_data_by_id = person_data_by_id,
        gamma_layer = gamma_layer,
        n_iter = n_iter, burnin = burnin, thin = thin, seed = seed
      )
    })

    names(dim_results) <- dims
    dim_results
  })

  names(results) <- variants
  results
}

# -----------------------------------------------------------------------
# 5. Aggregation zu genereller Fähigkeit xi (Higher-Order, post-hoc) -
#    identische Logik zu data_simulation.R::aggregate_xi()
# -----------------------------------------------------------------------

#' @export
aggregate_xi_adapter <- function(mcmc_results_by_dim,
                                 dim_loadings = c(HiHat = 0.80, Snare = 0.85, Kick = 0.75)) {

  dims <- names(mcmc_results_by_dim)

  theta_start_mat <- purrr::map(dims, ~ mcmc_results_by_dim[[.x]]$posterior_mean$theta_start) %>%
    purrr::set_names(dims) %>%
    as.data.frame()

  loadings_vec <- dim_loadings[dims]

  xi <- as.matrix(theta_start_mat) %*% loadings_vec / sum(loadings_vec)

  tibble::tibble(
    person_id = rownames(theta_start_mat),
    xi_start  = as.numeric(xi)
  )
}

# -----------------------------------------------------------------------
# 6. Vergleich Intra- vs. Inter-Trial-Variante
# -----------------------------------------------------------------------

#' @param results_all_variants Output von run_mcmc_islp_all_variants().
#' @export
compare_islp_variants <- function(results_all_variants,
                                  dim_loadings = c(HiHat = 0.80, Snare = 0.85, Kick = 0.75)) {

  dims <- names(results_all_variants$intra)

  theta_compare <- purrr::map_dfr(dims, function(d) {

    intra_theta <- results_all_variants$intra[[d]]$posterior_mean$theta_start
    inter_theta <- results_all_variants$inter[[d]]$posterior_mean$theta_start

    common_ids <- intersect(names(intra_theta), names(inter_theta))

    tibble::tibble(
      dimension   = d,
      person_id   = common_ids,
      theta_intra = intra_theta[common_ids],
      theta_inter = inter_theta[common_ids]
    )
  })

  cor_by_dim <- theta_compare %>%
    dplyr::group_by(dimension) %>%
    dplyr::summarise(cor_theta_intra_inter = cor(theta_intra, theta_inter), .groups = "drop")

  xi_intra <- aggregate_xi_adapter(results_all_variants$intra, dim_loadings) %>%
    dplyr::rename(xi_intra = xi_start)
  xi_inter <- aggregate_xi_adapter(results_all_variants$inter, dim_loadings) %>%
    dplyr::rename(xi_inter = xi_start)

  xi_compare <- dplyr::inner_join(xi_intra, xi_inter, by = "person_id")

  list(
    theta_compare = theta_compare,
    cor_by_dim    = cor_by_dim,
    xi_compare    = xi_compare,
    cor_xi        = cor(xi_compare$xi_intra, xi_compare$xi_inter)
  )
}

# =============================================================================
# 7. Beispiel-Workflow (auskommentiert)
#
# Voraussetzung: simulate_dmt_results_long.R ist geladen bzw. gesourct.
# =============================================================================

source("simulate_dmt_results_long.R")

simulate_multi <- function(p_ids, ...) {
  purrr::map_dfr(p_ids, function(pid) simulate_dmt_results_long(p_id = pid, ...))
}

df_multi <- simulate_multi(
  p_ids  = paste0("sim_", sprintf("%02d", 1:8)),
  n_demo = 3L, n_main = 10L
)

results_all <- run_mcmc_islp_all_variants(
  df_multi,
  n_iter = 1500, burnin = 500, thin = 2, seed = 1
)

comparison <- compare_islp_variants(results_all)
print(comparison$cor_by_dim)   # Korrelation theta_intra vs. theta_inter je Dimension
print(comparison$cor_xi)       # Korrelation xi_intra vs. xi_inter
print(comparison$xi_compare)
