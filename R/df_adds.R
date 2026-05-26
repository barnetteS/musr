#' Extract title from a MusicXML document
#'
#' Retrieves the title of a score from a MusicXML document. The function first
#' checks for a title in the \code{credit} section and, if not found, falls back
#' to the \code{work-title} element.
#'
#' @param s An XML document object. Must be created using \code{xml2::read_xml()}.
#'
#' @return A character string containing the title, or \code{NA} if no title is found.
#'
#' @details
#' The function prioritizes \code{//credit[credit-type='title']/credit-words}.
#' If that is not present, it searches for \code{//work-title}. If neither exists,
#' \code{NA} is returned.
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("path/to/file.xml")
#' get_title(xml)
#' }
#'
#' @importFrom xml2 xml_find_first xml_text
#'
#' @export
get_title <- function(s){

  credit_node <- xml_find_first(s, "//credit[credit-type='title']/credit-words")

  if(length(credit_node) > 0){
    title <- xml_text(credit_node)
  } else {

    title_node <- xml_find_first(s, "//work-title")

    if(length(title_node) > 0){
      title <- xml_text(title_node)
    } else {
      title <- NA
    }

  }

  return(title)
}

#' Change Title
#'
#' Replaces the \code{title} column in a notes data frame with a specified value.
#'
#' @param notes_df A data frame containing note-level musical data. Must include a \code{title} column.
#' @param title_name A character string specifying the title to assign.
#'
#' @return A data frame with the \code{title} column updated.
#'
#' @examples
#' \dontrun{
#' change_title(notes_df, "Symphony No. 5")
#' }
#'
#' @export
change_title <- function(notes_df, title_name){
  notes_df$title <- as.character(title_name)

  return(notes_df)
}

#' Extract composer from a MusicXML document
#'
#' Retrieves the composer of a score from a MusicXML document. The function first
#' checks for a composer in the \code{credit} section and, if not found, falls back
#' to the \code{creator} element with type "composer".
#'
#' @param s An XML document object. Must be created using \code{xml2::read_xml()}.
#'
#' @return A character string containing the composer, or \code{NA} if no composer is found.
#'
#' @details
#' The function prioritizes \code{//credit[credit-type='composer']/credit-words}.
#' If that is not present, it searches for \code{//creator[@type='composer']}.
#' If neither exists, \code{NA} is returned.
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("path/to/file.xml")
#' get_composer(xml)
#' }
#'
#' @importFrom xml2 xml_find_first xml_text
#'
#' @export
get_composer <- function(s){

  credit_node <- xml_find_first(s, "//credit[credit-type='composer']/credit-words")

  if(length(credit_node) > 0){
    composer <- xml_text(credit_node)
  } else {

    comp_node <- xml_find_first(s, "//creator[@type='composer']")

    if(length(comp_node) > 0){
      composer <- xml_text(comp_node)
    } else {
      composer <- NA
    }

  }

  return(composer)
}

#' Change Composer Name
#'
#' Replaces the \code{composer} column in a notes data frame with a specified value.
#'
#' @param notes_df A data frame containing note-level musical data. Must include a \code{composer} column.
#' @param comp_name A character string specifying the composer name to assign.
#'
#' @return A data frame with the \code{composer} column updated.
#'
#' @examples
#' \dontrun{
#' change_composer(notes_df, "Johann Sebastian Bach")
#' }
#'
#' @export
change_composer <- function(notes_df, comp_name){
  notes_df$composer <- as.character(comp_name)

  return(notes_df)
}

#' Convert pitch components to MIDI number
#'
#' Converts pitch information defined by step, octave, and alteration into a
#' MIDI note number.
#'
#' @param step A character vector of pitch steps (A–G).
#' @param octave A numeric or character vector indicating octave.
#' @param alter A numeric or character vector indicating pitch alteration
#' (e.g., -1 for flat, 0 for natural, 1 for sharp).
#'
#' @return A numeric vector of MIDI note numbers.
#'
#' @details
#' MIDI values are computed using a base-12 system where C4 corresponds to 60.
#' The calculation combines octave position, pitch class, and alteration.
#'
#' @examples
#' pitch2midi("C", 4, 0)
#' pitch2midi("F", 4, 1)
#' pitch2midi("B", 3, -1)
#'
#' @export
pitch2midi <- function(step, octave, alter){
  step_map <- c(C = 0, D = 2, E = 4,
                F = 5, G = 7, A = 9,
                B = 11
                )
  base <- 12

  midi <- (base * (as.numeric(octave) + 1)) + step_map[step] + as.numeric(alter)

  return(midi)
}

#' Convert MIDI number to pitch label
#'
#' Converts MIDI note numbers into pitch labels that combine pitch class and octave.
#'
#' @param midi A numeric vector of MIDI note numbers.
#'
#' @return A character vector of pitch labels.
#'
#' @details
#' Pitch classes are mapped using a 12-tone system with accidentals encoded in the
#' label. The octave is computed from the MIDI number and appended to the pitch class.
#'
#' @examples
#' pitch2midi("C", 4, 0)
#' pitch2midi("F", 4, 1)
#' pitch2midi("B", 3, -1)
#'
#' @export
midi2pitch <- function(midi){
  step_map <- c("C0", "C1", "D0", "D1",
                "E0", "F0", "F1", "G0",
                "G1", "A0", "A1", "B0")

  base <- 12
  octave <- floor(midi / base) - 1
  p <- step_map[(midi %% base) + 1]

  return(paste0(p, octave))
}

#' Extract part name from a MusicXML document
#'
#' Retrieves the name of a specific part from a MusicXML document using its part ID.
#'
#' @param s An XML document object. Must be created using \code{xml2::read_xml()}.
#' @param p_id A character string representing the part ID.
#'
#' @return A character string containing the part name, or \code{NA} if not found.
#'
#' @details
#' The function searches for \code{//score-part[@id='p_id']/part-name}. If no matching
#' node is found, \code{NA} is returned.
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("path/to/file.xml")
#' get_part_name(xml, "P1")
#' }
#'
#' @importFrom xml2 xml_find_first xml_text
#'
#' @export
get_part_name <- function(s, p_id){
  part_node <- xml_find_first(s, paste0("//score-part[@id='", p_id, "']/part-name"))

  if(length(part_node) > 0){
    part_name <- xml_text(part_node)
  } else {
    part_name <- NA
  }

  return(part_name)
}

#' Check for required 'incon' package
#'
#' Verifies that the \code{incon} package is installed. Stops execution with
#' an error message if the package is not available.
#'
#' @return Invisibly returns \code{TRUE} if the package is available.
#'
#' @examples
#' \dontrun{
#' check_incon()
#' }
#'
#' @noRd
check_incon <- function() {

  if (!requireNamespace("incon", quietly = TRUE)) {
    stop(
      "The 'incon' package is required for this function.\n",
      "Install it with:\n",
      "remotes::install_github('pmcharrison/incon')"
    )
  }

  invisible(TRUE)
}

#' Compute pitch class set from MIDI values
#'
#' Converts MIDI note numbers into pitch classes (modulo 12) and returns
#' the unique set in ascending order.
#'
#' @param midis A numeric vector of MIDI note numbers.
#'
#' @return A numeric vector of unique pitch classes (0–11), sorted ascending.
#'
#' @examples
#' mod_midi(c(60, 64, 67))
#' mod_midi(c(61, 73))
#'
#' @export
mod_midi <- function(midis){
  mods <- sort(unique(as.numeric(midis) %% 12))
}

#' Define clef reference pitch
#'
#' Returns the reference pitch (letter and octave) associated with a given clef
#' defined by its sign and line.
#'
#' @param sign A character string representing the clef sign (e.g., "G", "F", "C").
#' @param line A character string representing the clef line.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{base_letter} Reference pitch letter
#'   \item \code{base_oct} Reference octave
#' }
#'
#' @details
#' The function maps common clef definitions (treble, bass, alto, tenor) to a
#' reference pitch used for further pitch calculations. If the clef is not recognized,
#' both values are returned as \code{NA}.
#'
#' @examples
#' define_clef("G", "2")
#' define_clef("F", "4")
#' define_clef("C", "3")
#'
#' @noRd
define_clef <- function(sign, line){
  if(sign == "G" && line == "2") return(list(base_letter = "E", base_oct = 4))
  if(sign == "F" && line == "4") return(list(base_letter = "G", base_oct = 2))
  if(sign == "C" && line == "3") return(list(base_letter = "F", base_oct = 3))
  if(sign == "C" && line == "4") return(list(base_letter = "D", base_oct = 3))

  return(list(base_letter = NA, base_oct = NA))
}

#' Compute roughness using incon
#'
#' Calculates roughness for a numeric vector using the \code{incon} package.
#'
#' @param x A numeric vector.
#'
#' @return A numeric value representing roughness.
#'
#' @details
#' The function validates input and calls \code{incon::roughness_hutch()}.
#' Requires the \code{incon} package.
#'
#' @examples
#' \dontrun{
#' get_roughness(c(60, 64, 67))
#' }
#'
#' @export
get_roughness <- function(x){
  check_incon()

  if (!(is.numeric(x) && is.vector(x))) {
    stop("x must be a numeric vector")
  }

  roughness <- incon::roughness_hutch(x)

  return(roughness)
}

#' Compute harmonicity using incon
#'
#' Calculates harmonicity for a numeric vector using the \code{incon} package.
#'
#' @param x A numeric vector.
#'
#' @return A numeric value representing harmonicity.
#'
#' @details
#' The function validates input and calls \code{incon::gill09_harmonicity()}.
#' Requires the \code{incon} package.
#'
#' @examples
#' \dontrun{
#' get_harmonicity(c(60, 64, 67))
#' }
#'
#' @export
get_harmonicity <- function(x){
  check_incon()

  if (!(is.numeric(x) && is.vector(x))) {
    stop("x must be a numeric vector")
  }

  harmonicity <- incon::gill09_harmonicity(x)

  return(harmonicity)
}

#' Build scale as pitch-class set
#'
#' Constructs a scale from key signature (fifths), octave, and mode, then
#' returns the corresponding pitch classes (modulo 12).
#'
#' @param fifths A numeric value indicating key signature (number of fifths).
#' @param octave A numeric value indicating starting octave.
#' @param mode A character string indicating mode (e.g., "major", "minor").
#'
#' @return A numeric vector of pitch classes (0–11).
#'
#' @details
#' The function maps fifths to a pitch name, builds a scale using
#' \code{music::buildScale()}, converts pitches to MIDI using \code{pitch2midi()},
#' and reduces them to pitch classes via modulo 12.
#'
#' @examples
#' build_scale(0, 4, "major")
#' build_scale(1, 4, "major")
#' build_scale(-3, 4, "minor")
#'
#' @importFrom music buildScale
#' @export
build_scale <- function(fifths, octave, mode){
  scale_map <- c(
    C = 0, G = 1, D = 2, A = 3, E = 4, B = 5, "F#" = 6, "C#" = 7,
    F = -1, "Bb" = -2, "Eb" = -3, "Ab" = -4, "Db" = -5, "Gb" = -6,
    "Cb" = -7
  )
  midis = c()

  pitch <- names(scale_map[scale_map == fifths])
  pitch <- paste0(pitch, octave)

  scale <- buildScale(pitch, mode)

  for (sc in scale) {
    step <- substr(sc, 1, 1)
    if(nchar(sc) != 3){
      alter <- 0
    } else {
      char2 <- substr(sc, 2, 2)
      alter <- ifelse(char2 == "#", 1, -1)
    }
    octave <- as.numeric(substr(sc, nchar(sc), nchar(sc)))

    p <- pitch2midi(step, octave, alter)

    midis[length(midis) + 1] <- mod_midi(p)
  }

  return(midis)
}
