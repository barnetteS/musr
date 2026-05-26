#' @importFrom dplyr if_else everything
#' @importFrom stats var
#' @importFrom magrittr %>%
NULL
#' Parse MusicXML into a note-level data frame
#'
#' Converts a MusicXML document into a structured data frame where each row
#' represents a note (or rest) with timing, pitch, and contextual metadata.
#' The function tracks onset and offset in beats, handles measure-level
#' attributes (key, time, clef, tempo), and accounts for \code{backup} nodes
#' to correctly adjust timing.
#'
#' @param s An XML document object. Must be created using \code{xml2::read_xml()}.
#'
#' @return A data frame with one row per note containing:
#' \itemize{
#'   \item \code{note_id} Unique running note index across the full score
#'   \item \code{part_id} Part identifier
#'   \item \code{measure} Measure number
#'   \item \code{note_num} Note index within part
#'   \item \code{onset} Note onset in beats
#'   \item \code{offset} Note offset in beats
#'   \item \code{dur_in_div} Duration in divisions
#'   \item \code{dur_in_beats} Duration in beats
#'   \item \code{isrest} Logical indicating rest
#'   \item \code{step} Pitch step (A–G)
#'   \item \code{alter} Pitch alteration (e.g., -1, 0, 1)
#'   \item \code{octave} Pitch octave
#'   \item \code{divisions} Divisions per quarter note
#'   \item \code{beats} Time signature numerator
#'   \item \code{beat_type} Time signature denominator
#'   \item \code{fifths} Key signature (number of fifths)
#'   \item \code{mode} Key mode (major/minor)
#'   \item \code{clef_sign} Clef sign
#'   \item \code{clef_line} Clef line
#'   \item \code{staff} Staff number
#'   \item \code{voice} Voice number
#'   \item \code{tempo} Tempo (if available)
#' }
#'
#' @details
#' The function iterates through parts, measures, and note nodes to extract
#' both musical content and structural metadata. Measure-level attributes
#' persist until updated. Timing is computed cumulatively using onset and
#' adjusted when encountering \code{backup} elements.
#'
#' @examples
#' \dontrun{
#' library(xml2)
#'
#' # read a MusicXML file
#' xml <- read_xml("path/to/file.xml")
#'
#' # parse into note-level data
#' notes_df <- parse_xml(xml)
#'
#' # inspect output
#' head(notes_df)
#'
#' # filter only pitched notes
#' notes_df[notes_df$isrest == FALSE, ]
#' }
#'
#' @importFrom xml2 xml_children xml_attr xml_find_all xml_find_first xml_name xml_text
#' @importFrom dplyr select
#'
#' @export
parse_xml <- function(s){
  message("Parsing MusicXML... processing time depends on score length, part count, and complexity.")

  if(is.character(s)){
    stop("parse_xml() requires an XML document. Use xml2::read_xml() first.")
  }

  rows <- list()

  parts <- xml_find_all(s, "//part")
  if(length(parts) > 0){
    for(p in parts){
      note_num <- 0
      p_id <- xml_attr(p, "id")
      onset <- 1

      measures <- xml_find_all(p, ".//measure")
      if(length(measures) > 0){
        fifths <- NA
        mode <- "major"
        beats <- NA
        beat_type <- NA
        clef_sign <- NA
        clef_line <- NA
        divisions <- NA
        tempo <- NA

        for(m in measures){
          m_num <- xml_attr(m, "number")

          tempo_node <- xml_find_first(m, ".//sound[@tempo]")
          if(length(tempo_node) > 0){
            tempo <- as.numeric(xml_attr(tempo_node, "tempo"))
          }

          clef <- xml_find_first(m, ".//clef")
          if(length(clef) > 0){
            clef_sign <- xml_text(xml_find_first(clef, ".//sign"))
            clef_line <- xml_text(xml_find_first(clef, ".//line"))
          }

          time <- xml_find_first(m, ".//time")
          if(length(time) > 0){
            beats <- xml_text(xml_find_first(time, ".//beats"))
            beat_type <- xml_text(xml_find_first(time, ".//beat-type"))
          }

          key <- xml_find_first(m, ".//key")
          if(length(key) > 0){
            fifths <- xml_text(xml_find_first(key, ".//fifths"))

            mode_node <- xml_find_first(key, ".//mode")
            if(length(mode_node) > 0){
              mode <- xml_text(mode_node)
            }
          }

          div <- xml_find_first(m, ".//divisions")
          if(length(div) > 0){
            divisions <- as.numeric(xml_text(div))
          }

          nodes <- xml_children(m)
          for(node in nodes){
            if(xml_name(node) == "note"){
              n <- node

              if(length(xml_find_first(n, ".//grace")) > 0){
                next
              }

              step <- NA
              octave <- NA
              alter <- NA
              staff <- NA
              voice <- NA

              staff_node <- xml_find_first(n, ".//staff")
              if(length(staff_node) > 0){
                staff <- xml_text(staff_node)
              }
              voice_node <- xml_find_first(n, ".//voice")
              if(length(voice_node) > 0){
                voice <- xml_text(voice_node)
              }

              rest <- xml_find_first(n, ".//rest")
              if(length(rest) > 0){
                isrest <- TRUE
              } else {
                  isrest <- FALSE
              }

              pitch <- xml_find_first(n, ".//pitch")
              if(length(pitch) > 0){
                step <- xml_text(xml_find_first(pitch, ".//step"))
                octave <- xml_text(xml_find_first(pitch, ".//octave"))
                alter <- xml_find_first(pitch, ".//alter")
                if(length(alter) > 0){
                  alter <- xml_text(alter)
                } else {
                  alter <- 0
                }
              }

              duration <- xml_text(xml_find_first(n, ".//duration"))
              dur_in_beats = (as.numeric(duration) * as.numeric(beat_type)) / (4 * divisions)

              offset <- onset + dur_in_beats
              note_num <- note_num + 1

              rows[[length(rows) + 1]] <- data.frame(
                part_id = p_id,
                measure = m_num,
                note_num = note_num,
                onset = onset,
                offset = offset,
                dur_in_div = as.numeric(duration),
                dur_in_beats = dur_in_beats,
                isrest = isrest,
                step = step,
                alter = alter,
                octave = octave,
                divisions = divisions,
                beats = beats,
                beat_type = beat_type,
                fifths = fifths,
                mode = mode,
                clef_sign = clef_sign,
                clef_line = clef_line,
                staff = staff,
                voice = voice,
                tempo = tempo
              )

              onset <- offset
            }

            if(xml_name(node) == "backup"){
              backup_dur <- as.numeric(xml_text(xml_find_first(node, ".//duration")))
              backup_dur_in_beats <- (as.numeric(backup_dur) * as.numeric(beat_type)) / (4 * divisions)
              onset <- onset - backup_dur_in_beats
            }
          }
        }
      }
    }
  }

  df <- do.call(rbind, rows)
  df$note_id <- seq_len(nrow(df))
  df <- df %>%
    select(note_id, everything())

  df <- df %>%
    group_by(measure) %>%
    mutate(
      tempo = max(tempo, na.rm = TRUE)
    ) %>%
    ungroup()

  return(df)
}

#' Expand notes data frame with derived features
#'
#' Adds identifiers, metadata, MIDI values, duration in seconds, and
#' diatonic context (scale and inside/outside key) to a notes data frame.
#'
#' @param notes_df A data frame of parsed note-level data.
#' @param score An xml document representing the full musical score.
#' @param score_name A character string identifying the score.
#'
#' @return A data frame with additional columns including id, title, composer,
#' part_name, midi_num, dur_in_sec, scale, and inside.
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("path/to/file.xml")
#' notes_df <- parse_xml(xml)
#'
#' expanded_df <- expand_notes_df(notes_df, xml, "My Score")
#'
#' head(expanded_df)
#' expanded_df[expanded_df$inside == FALSE, ]
#' }
#'
#' @importFrom dplyr mutate select filter rowwise left_join
#' @importFrom dplyr everything
#' @importFrom dplyr row_number
#'
#' @export
expand_notes_df <- function(notes_df, score, score_name){
  notes_df <- notes_df %>%
    mutate(
      id = row_number(),
      title = get_title(score),
      composer = get_composer(score),
      midi_num = ifelse(!isrest, pitch2midi(step, octave, alter), NA),
      score_name = score_name,
      part_name = sapply(part_id, function(x) get_part_name(score, x)),
      dur_in_sec = dur_in_beats * (60 / tempo)
    ) %>%
    select(
      id, score_name,
      title, composer,
      part_id, part_name,
      measure, note_num,
      onset, offset,
      dur_in_div, dur_in_beats,
      step, alter, octave, midi_num,
      everything()
    )

  s2 <- notes_df %>%
    select(id, fifths, midi_num, octave, mode) %>%
    filter(!is.na(midi_num)) %>%
    rowwise() %>%
    mutate(
      mods = mod_midi(midi_num),
      scale = list(build_scale(fifths, octave, mode)),
      inside = mods %in% scale
    )

  notes_df <- notes_df  %>%
    left_join(s2 %>% select(id, scale, inside), by = "id")

  return(notes_df)
}

#' Read a MusicXML score
#'
#' Reads a MusicXML file and returns a partwise score. If the file is in
#' timewise format, it is automatically converted to partwise format.
#'
#' @param score_path A character string giving the path to a MusicXML file.
#'
#' @return An XML document object containing a partwise MusicXML score.
#'
#' @examples
#' \dontrun{
#' score <- read_score("path/to/score.xml")
#' }
#'
#' @export
read_score <- function(score_path){
  score <- xml2::read_xml(score_path)

  if (xml2::xml_name(score) == "score-timewise"){
    xsl_path <- system.file("extdata", "timepart.xsl",
                            package = "musr")
    xslt <- xml2::read_xml(xsl_path)
    score <- xslt::xml_xslt(score, xslt)
  }

  return(score)
}
