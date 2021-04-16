# GOOGLE coud speech-to-text
library(googleLanguageR)
library(tuneR)
library(tidyverse)
library(fs)
library(here)
# AUthenticate google cloud
gl_auth("bubbly-upgrade-183413-47ec9371ccb4.json")

# Get important info in one tibble
audio_data <-
    tibble( raw_file = dir_ls("00_raw/"),
            id = str_remove_all(raw_file, "00_raw/|mp3|wav"),
            ext = str_sub(raw_file, start = -3, end = -1),
            cut_dir = paste0("01_cut/", id),
            mono_dir = paste0("02_mono/", id),
            ffmpeg_cmd = paste0("ffmpeg -i ",
                                here(raw_file),
                                " -ac 1 -ab 192k -f segment -segment_time 30 -c copy ",
                                here(cut_dir, paste0("out%03d.", ext)))
    ) %>% 
    filter(str_detect(id, "tt_kerekasztal"))

# Create the directories that contain the cut material
walk(audio_data$cut_dir, dir_create)
walk(audio_data$mono_dir, dir_create)

# Cut video
walk(audio_data$ffmpeg_cmd, system)

# Read and transform cut files to mono
recording <-
    tibble(file = dir_ls("01_cut", recurse = TRUE, type = "file", glob = "*.wav")) %>% 
    filter(str_detect(file, "tt_kerekasztal")) %>% 
    mutate(wave = map(file, readWave),
           mono_file = str_replace(file, "01_cut", "02_mono"),
           mono_wave = map(wave, mono, which = "both"))

# Write mono files
walk2(recording$mono_wave, 
      recording$mono_file,
      ~writeWave(object = .x, filename = .y, extensible = FALSE))

safe_gl_speech <- safely(gl_speech, otherwise = NA)

recording <-
    tibble(file = dir_ls("01_cut",recurse = TRUE,type = "file",glob = "*.wav"),
        wave = map(file, readWave),
        mono_file = str_replace(file, "01_cut", "02_mono"),
        mono_wave = map(wave, mono, which = "both")
    )


safe_gl_speech <- safely(gl_speech)

texts <-
    recording %>%
    filter(str_detect(mono_file, "PD|SzZs")) %>%
    mutate(trans = map(
        mono_file,
        ~ safe_gl_speech(
            .x,
            sampleRateHertz = 44100L,
            languageCode = "hu-HU",
            asynch = TRUE
        )
    ))

texts <-
    texts %>%
    select(mono_file, text) %>%
    mutate(id = str_match(mono_file, "02_mono/(.*)/.*")[, 2]) %>%
    group_by(id) %>%
    nest() %>%
    mutate(transcript = map_chr(data, ~ paste(.x$text, collapse = " ")),
           text_file = paste0("03_text/", id, "_transcribe.txt")
    )

# Write transcribe
walk2(texts$transcript, texts$text_file,
write_lines(.x, file = .y))
