
library(tidyverse)

load("data-raw/Stimuli_Information/drumpatterns.Rda")
load("data-raw/Stimuli_Information/stimuli.Rda")


sixteenth_divisions <- seq(from = 0, to = 16, by = 0.25)

drum_matrix <- drumpatterns %>%
  filter(
    Beats %in% sixteenth_divisions,
    !Instrument %in% "CrashCymbal"
  ) %>%
  dplyr::select(Stimulus, Audiofile, Instrument, Seconds, Beats) %>%
  mutate(
    Instrument = case_when(
      Instrument %in% c("HiHatOpen", "HiHatHalfOpen") ~ "HiHat",
      TRUE ~ Instrument
    ),
    BeatPositionSixteenth = as.integer(Beats / 0.25 + 1)
  ) %>%
  # Only use a bar
  filter(BeatPositionSixteenth <= 16)



stimuli_being_used <- drum_matrix$Stimulus %>% unique()


# Check no beat info being dropped


filt_test <- drumpatterns %>%
  filter(Stimulus %in% stimuli_being_used)

filt_test %>%
  nrow()


nrow(drum_matrix) == filt_test


filt_test %>%
  filter(!Beats %in% sixteenth_divisions)


# Remove 10 and 22 because they don't use sixteenth divisions

drum_matrix <- drum_matrix %>%
  filter(!Stimulus %in% c(10, 22) ) %>%
  mutate(
    Instrument = case_when(
      Instrument %in% c("SnareDrumClick", "SnareDrum", "SnareDrumRoll") ~ "Snare",
      Instrument == "BassDrum" ~ "Kick",
      Instrument == "RideCymbalCrashed" ~"HiHat",
      TRUE ~ Instrument
    )
  )


# Pattern 5 and 8 are the same, when we just use the 1st bar; pattern 19 and 14 are also very similar.
# So just use one of each pair.


drum_matrix <- drum_matrix %>%
  dplyr::filter(!Stimulus %in% c(5, 14)) %>%
  dplyr::rename(OriginalStimulusId = Stimulus) %>%
  dplyr::arrange(OriginalStimulusId) %>%
  dplyr::mutate(Stimulus = dplyr::dense_rank(OriginalStimulusId)) %>%
  mutate( OriginalStimulusId = as.character(OriginalStimulusId),
          Stimulus = as.character(Stimulus)
          )


# drum_matrix


# Easy Stimuli

easy_stimuli_drum_matrix <- readr::read_csv("data-raw/Stimuli_Information/DMT_easy_stimuli_all.csv") %>%
  dplyr::group_by(pattern) %>%
  dplyr::mutate(BeatPositionSixteenth = dplyr::row_number() ) %>%
  dplyr::ungroup()  %>%
  pivot_longer(c(hihat, snare, bass), names_to = "Instrument", values_to = "Beats") %>%
  dplyr::mutate(
    Instrument = case_when(
      Instrument == "hihat" ~ "HiHat",
      Instrument == "snare" ~ "Snare",
      Instrument == "bass" ~ "Kick",
      TRUE ~ Instrument
    )
  ) %>%
  dplyr::filter(Beats == 1) %>%
  left_join(
    readr::read_csv("data-raw/Stimuli_Information/DMT_easy_stimuli_complexity.csv") %>%
      dplyr::select(-1),
    by = "pattern"
  ) %>%
  rename(OriginalStimulusId = pattern,
         Complexity = complexity) %>%
  mutate(Stimulus = as.factor(OriginalStimulusId) ) %>% # Convert to and from factor to get numbered version
  mutate(Stimulus = paste0("Easy_", as.integer(Stimulus) ) ) %>%
  mutate(
    Audiofile = NA,
    Seconds = NA
  ) %>%
  relocate(OriginalStimulusId, Audiofile, Instrument, Seconds, Beats, BeatPositionSixteenth, Stimulus)

# Predict complexities

easy_stimuli_drum_matrix <- easy_stimuli_drum_matrix |>
  mutate(
    Complexity = map_dbl(
      Stimulus,
      ~ tryCatch(predict_complexity(stimuli_df_to_matrix(easy_stimuli_drum_matrix, .x)), error = function(err) {
          logging::logerror("Error: %s", err)
          return(NA)
      })
    )
  )


drum_matrix <- drum_matrix |>
  mutate(
    Complexity = map_dbl(
      Stimulus,
      ~ tryCatch(predict_complexity(stimuli_df_to_matrix(drum_matrix, .x)), error = function(err) {
        logging::logerror("Error: %s", err)
        return(NA)
      })
    )
  )


complexities <- rbind(
  easy_stimuli_drum_matrix %>% dplyr::select(Stimulus, Complexity) %>% mutate(Type = "Easy") %>% unique(),
  drum_matrix %>% dplyr::select(Stimulus, Complexity) %>% mutate(Type = "Main") %>% unique()
)

complexities %>%
  ggplot(aes(x = Complexity, group = Type, fill = Type)) +
    geom_histogram() +
    facet_wrap(~Type)


# Compute halves bins
easy_stimuli_drum_matrix <- easy_stimuli_drum_matrix %>%
  mutate(ComplexityHalves = paste0("Easy", cut_number(Complexity, n = 2)))

drum_matrix <- drum_matrix %>%
  mutate(ComplexityHalves = cut_number(Complexity, n = 2))


demo_drum_matrix <- read_csv("data-raw/Stimuli_Information/DMT_instruction_stimuli.csv") %>%
  dplyr::select(-1) %>%
  dplyr::rename(BeatPositionSixteenth =Beat) %>%
  pivot_longer(c(hihat, snare, bass), names_to = "Instrument", values_to = "Beats") %>%
  dplyr::mutate(
    Instrument = case_when(
      Instrument == "hihat" ~ "HiHat",
      Instrument == "snare" ~ "Snare",
      Instrument == "bass" ~ "Kick",
      TRUE ~ Instrument
    )
  ) %>%
  mutate(OriginalStimulusId = Stimulus) %>%
  filter(Beats == 1)

demo_drum_matrix <- demo_drum_matrix %>%
  mutate(
    Complexity = map_dbl(
      Stimulus,
      ~ tryCatch(predict_complexity(stimuli_df_to_matrix(demo_drum_matrix, .x)), error = function(err) {
        logging::logerror("Error: %s", err)
        return(NA)
      })
    )
  ) %>%
  mutate(Stimulus = as.factor(OriginalStimulusId) ) %>% # Convert to and from factor to get numbered version
  mutate(Stimulus = paste0("Easy_", as.integer(Stimulus) ) ) %>%
  mutate(
    Audiofile = NA,
    Seconds = NA
  ) %>%
  relocate(OriginalStimulusId, Audiofile, Instrument, Seconds, Beats, BeatPositionSixteenth, Stimulus) %>%
  mutate(TrialNo = as.integer(as.factor(OriginalStimulusId)))

easy_stimuli_drum_matrix <- easy_stimuli_drum_matrix %>%
  filter(!Stimulus %in% unique(paste0("Easy_", demo_drum_matrix$OriginalStimulusId)) )

usethis::use_data(drum_matrix, easy_stimuli_drum_matrix, demo_drum_matrix, overwrite = TRUE)

rm(list = ls())

