

##### A Predictive Coding Approach to Modelling Perceived Drum Pattern Complexity
#####
##### Script to estimate drum pattern complexity
#####
##### Created using R version 4.1.2 (2021-11-01); RStudio  version 2022.07.1+554
#####
##### Olivier Senn (January 10, 2023)


##### Predict complexity function #####

predict_complexity <- function (Si, lambda = 3.56, C = 2.492082, D = -6.926637) {

  # Si = drum pattern, M0 = modes of prior distribution

  # Create 65x3 all-ones matrix J
  J <- matrix(1, nrow = 65, ncol = 3)
  # Calculate A0 (65x3 matrix)
  A0 <- (lambda - 2)*M0 + J
  # Calculate B0 (65x3 matrix)
  B0 <- lambda*J - (lambda - 2)*M0 - J
  # Calculate Ai (65x3 matrix)
  Ai <- U %*% Si + A0
  # Calculate Bi (65x3 matrix)
  Bi <- U %*% (J - Si) + B0
  # Calculate Mi (65x3 matrix)
  Mi <- (Ai - 1) / (Ai + Bi - 2)
  # Calculate sigma_i (65x3 matrix, the surprisal function)
  sigma_i <- -log(abs(Si + Mi - J))
  # Calculate Ei (the effort Ei associated with stimulus Si)
  Ei <- sum(sigma_i)
  # Calculate gamma_i (the predicted complexity of stimulus Si)
  gamma_i <- C*log(Ei) + D
  # Return gamma_i
  gamma_i
}


stimuli_df_to_matrix <- function(drum_matrix, stimulus_id) {

  all_zeros <- matrix(0, nrow = 65, ncol = 3) %>%
    as.data.frame() %>%
    mutate(BeatPositionSixteenth = dplyr::row_number() ) %>%
    rename(BD = V1, SD = V2, CY = V3)

  stim <- drum_matrix %>%
    dplyr::filter(Stimulus == stimulus_id) %>%
    dplyr::select(Stimulus, BeatPositionSixteenth, Instrument)

  stim_wide <- stim %>%
    mutate(On = 1) %>%
    pivot_wider(
      names_from = Instrument,
      values_from = On,
      values_fill = 0
    )

  if(!"HiHat" %in% names(stim_wide)) {
    stim_wide <- stim_wide %>% dplyr::mutate(HiHat = 0L)
  }

  if(!"Kick" %in% names(stim_wide)) {
    stim_wide <- stim_wide %>% dplyr::mutate(Kick = 0L)
  }

  if(!"Snare" %in% names(stim_wide)) {
    stim_wide <- stim_wide %>% dplyr::mutate(Snare = 0L)
  }


  mat <- all_zeros %>% dplyr::full_join(stim_wide, by = "BeatPositionSixteenth") %>%
    mutate(BD = case_when (Kick == 1 ~ 1, TRUE ~ 0),
           SD = case_when (Snare == 1 ~ 1, TRUE ~ 0),
           CY = case_when (HiHat == 1 ~ 1, TRUE ~ 0) ) %>%
    dplyr::select(BD, SD, CY) %>%
    as.matrix()

  return(mat)

}


