#' Create time-sliced representation of notes
#'
#' Splits a note-level data frame into fixed time increments and aggregates
#' active notes at each time point.
#'
#' @param notes_df A data frame containing note-level data with at least
#' \code{onset}, \code{offset}, \code{measure}, and \code{midi}.
#' @param increment A numeric value indicating time step size.
#'
#' @return A data frame with:
#' \itemize{
#'   \item \code{time} Time point
#'   \item \code{measure} Measure active at that time
#'   \item \code{midis} List column of active MIDI values
#' }
#'
#' @details
#' For each time step, notes are included if their onset is less than or equal
#' to the time and their offset is greater than the time. MIDI values are stored
#' as a list column.
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("path/to/file.xml")
#' notes_df <- parse_xml(xml)
#' notes_df <- expand_notes_df(notes_df, xml, "My Score")
#'
#' time_df <- time_slice(notes_df, increment = 1)
#' head(time_df)
#' }
#'
#' @importFrom dplyr rowwise mutate ungroup
#'
#' @export
time_slice <- function(notes_df, increment = 0.5){

  time_table <- data.frame(
    time = seq(increment, max(notes_df$offset), by = increment)
  ) %>%
    rowwise() %>%
    mutate(
      measure = notes_df$measure[
        which(notes_df$onset <= time & notes_df$offset > time)[1]],
      midis = list(notes_df$midi[notes_df$onset <= time &
                                   notes_df$offset > time &
                                   !is.na(notes_df$midi) ])
    ) %>%
    ungroup()

  time_table
}

#' Compute time-slice metrics
#'
#' Calculates note density, roughness, harmonicity, and pitch-class sets
#' for each time slice.
#'
#' @param time_df A data frame produced by \code{time_slice()} containing
#' \code{time}, \code{measure}, and \code{midis}.
#'
#' @return A data frame with:
#' \itemize{
#'   \item \code{time} Time point
#'   \item \code{measure} Measure at that time
#'   \item \code{midis} List column of MIDI values
#'   \item \code{mod12_midis} List column of pitch classes
#'   \item \code{roughness} Roughness value
#'   \item \code{harmonicity} Harmonicity value
#'   \item \code{notes_per_slice} Number of notes in the slice
#' }
#'
#' @details
#' The function computes local metrics for each time slice using
#' \code{local_metrics()}, rounds values, and replaces missing values with 0.
#' Requires the \code{incon} package.
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("path/to/file.xml")
#' notes_df <- parse_xml(xml)
#' notes_df <- expand_notes_df(notes_df, xml, "My Score")
#'
#' time_df <- time_slice(notes_df)
#' tm <- time_metrics(time_df)
#'
#' head(tm)
#' }
#'
#' @importFrom dplyr rowwise mutate ungroup select coalesce
#'
#' @export
time_metrics <- function(time_df){
  check_incon()

  message("This may take a moment...")
  time_df <- time_df %>%
    rowwise() %>%
    mutate(
      notes_per_slice = length(midis),
      metrics = list(local_metrics(unlist(midis))),
      roughness = coalesce(round(metrics$roughness, 4), 0),
      harmonicity = coalesce(round(metrics$harmonicity, 4), 0),
      mod12_midis = list(mod_midi(midis))
    ) %>%
    ungroup() %>%
    select(time, measure, midis, mod12_midis, roughness, harmonicity,
           notes_per_slice)

  return(time_df)
}

#' Compute local roughness and harmonicity
#'
#' Calculates roughness and harmonicity for a set of MIDI values.
#'
#' @param midis A numeric vector of MIDI note numbers.
#' @param viz Logical indicating whether to enable visualization (currently unused).
#'
#' @return A data frame with:
#' \itemize{
#'   \item \code{roughness} Roughness value
#'   \item \code{harmonicity} Harmonicity value
#' }
#'
#' @details
#' MIDI values are converted to pitch labels and formatted before computing
#' metrics. If fewer than two unique MIDI values are present, both metrics
#' return \code{NA}. Requires the \code{incon} package.
#'
#' @examples
#' \dontrun{
#' local_metrics(c(60, 64, 67))
#' local_metrics(c(60))
#' }
#'
#' @export
local_metrics <- function(midis, viz = FALSE){
  check_incon()

  pitches <- midi2pitch(midis)
  for(i in seq_along(pitches)){
    if(substr(pitches[i], 2, 2) == "0"){
      pitches[i] <- paste0(substr(pitches[i], 1, 1), substr(pitches[i], 3, nchar(pitches[i])))
    } else if(substr(pitches[i], 2, 2) == "1"){
      pitches[i] <- paste0(substr(pitches[i], 1, 1), "#", substr(pitches[i], 3, nchar(pitches[i])))
    }
  }

  if(length(unique(midis)) > 1){
    df <- data.frame(
      roughness = get_roughness(midis),
      harmonicity = get_harmonicity(midis)
    )
  } else {
    df <- data.frame(
      roughness = NA,
      harmonicity = NA
    )
  }

  return(df)
}

#' Compute global roughness and harmonicity
#'
#' Calculates mean roughness and harmonicity across all time slices.
#'
#' @param time_df A data frame containing \code{roughness} and \code{harmonicity}.
#' @param viz A character string indicating whether to print a summary table ("yes" or "no").
#'
#' @return A data frame with:
#' \itemize{
#'   \item \code{mean_roughness} Mean roughness
#'   \item \code{mean_harmonicity} Mean harmonicity
#' }
#'
#' @details
#' If \code{viz = "yes"}, a simple table of results is printed using
#' \code{knitr::kable()}.
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("path/to/file.xml")
#' notes_df <- parse_xml(xml)
#' notes_df <- expand_notes_df(notes_df, xml, "My Score")
#'
#' time_df <- time_slice(notes_df)
#' time_df <- time_metrics(time_df)
#'
#' global_metrics(time_df)
#' global_metrics(time_df, viz = "yes")
#' }
#'
#' @importFrom knitr kable
#'
#' @export
global_metrics <- function(time_df, viz = "no"){
  df <- data.frame(
    mean_roughness = mean(time_df$roughness, na.rm = TRUE),
    mean_harmonicity = mean(time_df$harmonicity, na.rm = TRUE)
  )

  viz <- tolower(viz)

  if(viz == "yes" && !is.null(df)){
    df_table <- data.frame(
      metric = c("Mean Roughness", "Mean Harmonicity"),
      value = c(
        round(df$mean_roughness, 4),
        round(df$mean_harmonicity, 4)
      )
    )

    print(kable(df_table, format = "simple"))
  }

  return(df)
}

#' Compute part-level pitch metrics
#'
#' Summarizes pitch range and clef information for each part (and staff when applicable).
#'
#' @param notes_df A data frame containing note-level data with at least
#' \code{part_id}, \code{staff}, \code{part_name}, \code{clef_sign},
#' \code{clef_line}, and \code{midi_num}.
#'
#' @return A data frame with:
#' \itemize{
#'   \item \code{part_id} Part identifier
#'   \item \code{staff} Staff identifier (or \code{NA} if single clef)
#'   \item \code{part_name} Part name
#'   \item \code{clef_sign} Clef sign
#'   \item \code{clef_line} Clef line
#'   \item \code{low_p} Lowest MIDI pitch
#'   \item \code{high_p} Highest MIDI pitch
#'   \item \code{p_range} Pitch range
#'   \item \code{n_clefs} Number of distinct clefs in the part
#'   \item \code{clef_base_letter} Clef reference pitch letter
#'   \item \code{clef_base_oct} Clef reference octave
#' }
#'
#' @details
#' The function aggregates pitch ranges by part and staff, determines whether
#' multiple clefs are present, and attaches clef reference pitches using
#' \code{define_clef()}.
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("path/to/file.xml")
#' notes_df <- parse_xml(xml)
#' notes_df <- expand_notes_df(notes_df, xml, "My Score")
#'
#' part_df <- part_metrics(notes_df)
#' head(part_df)
#' }
#'
#' @importFrom dplyr group_by summarize mutate ungroup distinct select n_distinct first
#'
#' @export
part_metrics <- function(notes_df){
  p_df <- notes_df %>%
    group_by(part_id, staff) %>%
    summarize(
      part_name = first(part_name),
      clef_sign = first(clef_sign),
      clef_line = first(clef_line),
      low_p = min(midi_num, na.rm = TRUE),
      high_p = max(midi_num, na.rm = TRUE),
      p_range = max(midi_num, na.rm = TRUE) - min(midi_num, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(part_id) %>%
    mutate(
      n_clefs = n_distinct(paste(clef_sign, clef_line)),
      staff = ifelse(n_clefs > 1, staff, NA)
    ) %>%
    ungroup() %>%
    distinct(part_id, staff, .keep_all = TRUE) %>%
    mutate(
      clef_info = mapply(define_clef, clef_sign, clef_line, SIMPLIFY = FALSE),
      clef_base_letter = sapply(clef_info, `[[`, "base_letter"),
      clef_base_oct = sapply(clef_info, `[[`, "base_oct")
    ) %>%
    select(-clef_info)

  return(p_df)
}

#' Score Summary
#'
#' Computes summary statistics for a musical score using note-level and time-sliced data.
#' Includes pitch range, note/rest counts, activity metrics, variance measures, and total duration.
#'
#' @param notes_df A data frame of note-level data. Must include columns such as \code{offset}, \code{tempo},
#' \code{midi_num}, and \code{isrest}.
#' @param time_df A data frame of time-sliced data. Must include columns such as \code{midis},
#' \code{notes_per_slice}, \code{roughness}, and \code{harmonicity}.
#' @param viz Logical. If \code{TRUE}, prints the summary table using \code{knitr::kable}.
#'
#' @return A data frame containing summary statistics for the score.
#'
#' @details Total time is computed by converting note offsets (in beats) to seconds using tempo,
#' assuming beats are indexed starting at 1. Activity time is derived as a percentage of total time.
#'
#' @examples
#' \dontrun{
#' score_summary(notes_df, time_df)
#' }
#'
#' @importFrom dplyr summarize select first
#' @importFrom knitr kable
#' @export
score_summary <- function(notes_df, time_df, viz = FALSE){
  check_incon()

  seconds <- notes_df %>%
    mutate(
      offset_sec = (offset - 1) * (60 / tempo)
    )
  total_time <- max(seconds$offset_sec, na.rm = TRUE)

  score_df <- notes_df %>%
    summarize(
      score_name = first(score_name),
      l_midi = min(midi_num, na.rm = TRUE),
      h_midi = max(midi_num, na.rm = TRUE),
      midi_range = h_midi - l_midi,
      note_count = sum(isrest == FALSE),
      rest_count = sum(isrest == TRUE),
      avg_notes_per_slice = mean(time_df$notes_per_slice),
      var_tension = var(time_df$roughness, na.rm = TRUE),
      var_stability = var(time_df$harmonicity, na.rm = TRUE),
      slices = nrow(time_df),
      active_slices = sum(lengths(time_df$midis) != 0),
      activity_prct = round((active_slices / slices) * 100, 2),
      total_time = total_time,
      activity_time = total_time * activity_prct / 100
    ) %>%
    select(
      score_name,
      note_count, rest_count,
      avg_notes_per_slice,
      l_midi, h_midi, midi_range,
      var_tension, var_stability,
      slices, active_slices,
      activity_prct,
      total_time, activity_time
    )

  if (viz) {
    print(kable(score_df))
  }

  return(score_df)
}

