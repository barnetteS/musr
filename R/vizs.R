#' Plot harmonic metric by measure
#'
#' Creates a bar or line plot of roughness or harmonicity aggregated by measure.
#'
#' @param time_df A data frame containing time-slice metrics.
#' @param condis A character string indicating which metric to plot ("roughness" or "harmonicity").
#' @param panel_color Background color of the panel.
#' @param plot_color Background color of the full plot.
#' @param data_fill Fill color for bars or line color.
#'
#' @param xaxis_text Logical indicating whether to show x-axis text.
#' @param yaxis_text Logical indicating whether to show y-axis text.
#' @param xaxis_text_col Color of x-axis text.
#' @param xaxis_text_size Size of x-axis text.
#' @param yaxis_text_col Color of y-axis text.
#' @param yaxis_text_size Size of y-axis text.
#'
#' @param type Plot type ("bar" or "line").
#' @param line_width Line width if \code{type = "line"}.
#' @param by_measure_lab Interval for x-axis breaks.
#'
#' @param x_gridmaj Logical for major x grid.
#' @param y_gridmaj Logical for major y grid.
#' @param x_gridmin Logical for minor x grid.
#' @param y_gridmin Logical for minor y grid.
#' @param x_gridmajcol Color of major x grid.
#' @param y_gridmajcol Color of major y grid.
#' @param x_gridmincol Color of minor x grid.
#' @param y_gridmincol Color of minor y grid.
#' @param x_gridmajwid Width of major x grid lines.
#' @param y_gridmajwid Width of major y grid lines.
#' @param x_gridminwid Width of minor x grid lines.
#' @param y_gridminwid Width of minor y grid lines.
#'
#' @param x_axistitle Logical indicating whether to show x-axis title.
#' @param y_axistitle Logical indicating whether to show y-axis title.
#' @param x_axistitlecol Color of x-axis title.
#' @param y_axistitlecol Color of y-axis title.
#' @param x_axistitlesize Size of x-axis title.
#' @param y_axistitlesize Size of y-axis title.
#' @param x_titlelab Label for x-axis.
#' @param y_titlelab Label for y-axis.
#'
#' @param s_title Logical indicating whether to show subtitle.
#' @param p_title Logical indicating whether to show title.
#' @param title_size Size of plot title.
#' @param title_col Color of plot title.
#' @param sub_size Size of subtitle.
#' @param sub_col Color of subtitle.
#' @param title_align Horizontal alignment of title.
#' @param sub_align Horizontal alignment of subtitle.
#'
#' @param overall_mean Logical indicating whether to show overall score mean line.
#' @param overall_mean_col Color of overall score mean line.
#' @param overall_mean_type Line type of overall score mean line; dashed, solid, etc.
#' @param overall_mean_width Numerical Width of the overall score mean line.
#'
#' @return A ggplot object.
#'
#' @details
#' The function aggregates values by measure and plots either mean roughness or
#' harmonicity. Plot appearance is controlled through extensive theme and labeling options.
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
#' harmony_plot(time_df)
#' harmony_plot(time_df, condis = "harmonicity", type = "line")
#' }
#'
#' @importFrom dplyr filter mutate group_by summarize
#' @import ggplot2
#'
#' @export
harmony_plot <- function(time_df, condis = "roughness",
                        panel_color = "#E0E0E0",
                        plot_color = "#E0E0E0", data_fill = "#6B8FA3",
                        xaxis_text = TRUE, yaxis_text = TRUE,
                        xaxis_text_col = "black",
                        xaxis_text_size = 10,
                        yaxis_text_col = "black",
                        yaxis_text_size = 10,
                        type = "line", line_width = 0.7,
                        by_measure_lab = 10,
                        x_gridmaj = TRUE, y_gridmaj = TRUE,
                        x_gridmin = TRUE, y_gridmin = TRUE,
                        x_gridmajcol = "#BDBDBD", y_gridmajcol = "#BDBDBD",
                        x_gridmincol = "#BDBDBD", y_gridmincol = "#BDBDBD",
                        x_gridmajwid = 0.4, y_gridmajwid = 0.4,
                        x_gridminwid = 0.3, y_gridminwid = 0.3,
                        x_axistitle = TRUE, y_axistitle = TRUE,
                        x_axistitlecol = "black",
                        y_axistitlecol = "black",
                        x_axistitlesize = 10,
                        y_axistitlesize = 10,
                        x_titlelab = waiver(),
                        y_titlelab = paste0(condis, " - measure avg"),
                        s_title = TRUE, p_title = TRUE,
                        title_size = 12, title_col = "black",
                        sub_size = 10, sub_col = "black",
                        title_align = 0, sub_align = 0,
                        overall_mean = FALSE, overall_mean_col = "red",
                        overall_mean_type = "dashed", overall_mean_width = 0.7){


  pl_title = paste0("Mean ", toupper(substr(condis, 1, 1)),
                      substr(condis, 2, nchar(condis)))

  if (condis == "roughness"){
    pl_subtitle <- "tension of harmonies per slice"
  } else {
    pl_subtitle <- "clarity of harmonies per measure"
  }

  m_df <- time_df %>%
    filter(!is.na(measure)) %>%
    mutate(measure = as.numeric(measure)) %>%
    group_by(measure) %>%
    summarize(
      roughness = mean(roughness),
      harmonicity = mean(harmonicity),
      .groups = "drop"
    )

  p <- ggplot(m_df, aes(x = measure, y = .data[[condis]])) +
    {
      if (type == "bar") {
        geom_col(fill = data_fill)
      } else {
        geom_line(aes(group = 1), color = data_fill, linewidth = line_width)
      }
    } +
    scale_x_continuous(breaks = c(1, seq(by_measure_lab,
                                         max(m_df$measure) + by_measure_lab,
                                         by = by_measure_lab))) +
    labs(
      x = x_titlelab, y = y_titlelab,
      title = if (p_title) pl_title else NULL,
      subtitle = if (s_title) pl_subtitle else NULL
    ) +
    theme(
      panel.background = element_rect(fill = panel_color),
      plot.background  = element_rect(fill = plot_color),

      panel.grid.major.x = if (x_gridmaj)
        element_line(color = x_gridmajcol, linewidth = x_gridmajwid) else element_blank(),

      panel.grid.major.y = if (y_gridmaj)
        element_line(color = y_gridmajcol, linewidth = y_gridmajwid) else element_blank(),

      panel.grid.minor.x = if (x_gridmin)
        element_line(color = x_gridmincol, linewidth = x_gridminwid) else element_blank(),

      panel.grid.minor.y = if (y_gridmin)
        element_line(color = y_gridmincol, linewidth = y_gridminwid) else element_blank(),

      axis.text.x = if (xaxis_text)
        element_text(color = xaxis_text_col, size = xaxis_text_size)
      else element_blank(),

      axis.text.y = if (yaxis_text)
        element_text(color = yaxis_text_col, size = yaxis_text_size)
      else element_blank(),

      axis.title.x = if (x_axistitle)
        element_text(color = x_axistitlecol, size = x_axistitlesize,
                     margin = margin(t = 10, b = 5))
      else element_blank(),

      axis.title.y = if (y_axistitle)
        element_text(color = y_axistitlecol, size = y_axistitlesize,
                     margin = margin(r = 10, l = 5))
      else element_blank(),

      plot.title = element_text(size = title_size,
                                color = title_col,
                                face = "bold",
                                hjust = title_align),
      plot.subtitle = element_text(size = sub_size,
                                   color = sub_col,
                                   face = "italic",
                                   hjust = sub_align)
    )

  if(overall_mean) {
    p <-
      p + geom_hline(yintercept = mean(m_df[[condis]]),
                     color = overall_mean_col,
                     linetype = overall_mean_type,
                     line_width = overall_mean_width)
  }

  p
}

#' Plot diatonic deviation
#'
#' Creates a bar chart showing counts of diatonic vs nondiatonic notes.
#'
#' @param notes_df A data frame containing note-level data with \code{inside}.
#'
#' @param panel_color Background color of the panel.
#' @param plot_color Background color of the full plot.
#'
#' @param xaxis_text Logical indicating whether to show x-axis text.
#' @param yaxis_text Logical indicating whether to show y-axis text.
#' @param xaxis_text_col Color of x-axis text.
#' @param xaxis_text_size Size of x-axis text.
#' @param yaxis_text_col Color of y-axis text.
#' @param yaxis_text_size Size of y-axis text.
#'
#' @param x_gridmaj Logical for major x grid.
#' @param y_gridmaj Logical for major y grid.
#' @param x_gridmin Logical for minor x grid.
#' @param y_gridmin Logical for minor y grid.
#' @param x_gridmajcol Color of major x grid.
#' @param y_gridmajcol Color of major y grid.
#' @param x_gridmincol Color of minor x grid.
#' @param y_gridmincol Color of minor y grid.
#' @param x_gridmajwid Width of major x grid lines.
#' @param y_gridmajwid Width of major y grid lines.
#' @param x_gridminwid Width of minor x grid lines.
#' @param y_gridminwid Width of minor y grid lines.
#'
#' @param x_axistitle Logical indicating whether to show x-axis title.
#' @param y_axistitle Logical indicating whether to show y-axis title.
#' @param x_axistitlecol Color of x-axis title.
#' @param y_axistitlecol Color of y-axis title.
#' @param x_axistitlesize Size of x-axis title.
#' @param y_axistitlesize Size of y-axis title.
#' @param x_titlelab Label for x-axis.
#' @param y_titlelab Label for y-axis.
#'
#' @param pl_title Plot title.
#' @param pl_subtitle Plot subtitle.
#' @param s_title Logical indicating whether to show subtitle.
#' @param p_title Logical indicating whether to show title.
#' @param title_size Size of plot title.
#' @param title_col Color of plot title.
#' @param sub_size Size of subtitle.
#' @param sub_col Color of subtitle.
#' @param title_align Horizontal alignment of title.
#' @param sub_align Horizontal alignment of subtitle.
#'
#' @param flip Logical indicating whether to flip coordinates.
#' @param dia_col Fill color for diatonic bar.
#' @param ndia_col Fill color for non diatonic bar.
#' @param dev_leg Logical indicating whether to show legend.
#'
#' @param show_prct Logical indicating whether to show percentages.
#' @param dia_prct_col Color of diatonic percentage label.
#' @param ndia_prct_col Color of non diatonic percentage label.
#' @param prct_size Size of percentage text.
#' @param prct_vert Vertical position of percentage labels.
#'
#' @return A ggplot object.
#'
#' @details
#' Notes are categorized as diatonic or nondiatonic and counted. Optional
#' percentage labels, legend control, and coordinate flipping are supported.
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("path/to/file.xml")
#' notes_df <- parse_xml(xml)
#' notes_df <- expand_notes_df(notes_df, xml, "My Score")
#'
#' devn_plot(notes_df)
#' devn_plot(notes_df, flip = TRUE, show_prct = TRUE)
#' }
#'
#' @importFrom dplyr filter mutate group_by n
#' @importFrom ggplot2 ggplot aes geom_bar geom_text scale_fill_manual labs theme element_rect element_line element_blank element_text margin coord_flip guides
#'
#' @export
devn_plot <- function(notes_df, panel_color = "#E0E0E0",
                     plot_color = "#E0E0E0",
                     xaxis_text = TRUE, yaxis_text = TRUE,
                     xaxis_text_col = "black",
                     xaxis_text_size = 10,
                     yaxis_text_col = "black",
                     yaxis_text_size = 10,
                     x_gridmaj = TRUE, y_gridmaj = TRUE,
                     x_gridmin = TRUE, y_gridmin = TRUE,
                     x_gridmajcol = "black", y_gridmajcol = "black",
                     x_gridmincol = "black", y_gridmincol = "black",
                     x_gridmajwid = 0.4, y_gridmajwid = 0.4,
                     x_gridminwid = 0.3, y_gridminwid = 0.3,
                     x_axistitle = TRUE, y_axistitle = TRUE,
                     x_axistitlecol = "black",
                     y_axistitlecol = "black",
                     x_axistitlesize = 10,
                     y_axistitlesize = 10,
                     x_titlelab = "diatonic vs nondiatonic",
                     y_titlelab = "note count",
                     pl_title = "Diatonic Deviation",
                     pl_subtitle = "Key Signature Deviation",
                     s_title = TRUE, p_title = TRUE,
                     title_size = 12, title_col = "black",
                     sub_size = 10, sub_col = "black",
                     title_align = 0, sub_align = 0,
                     flip = FALSE, dia_col = "blue",
                     ndia_col = "red", dev_leg = TRUE,
                     show_prct = FALSE,
                     ndia_prct_col = "black", dia_prct_col = "black",
                     prct_size = 5, prct_vert = -0.5){
  cnotes <- notes_df %>%
    filter(!is.na(inside)) %>%
    nrow()

  p <- notes_df %>%
    filter(!is.na(inside)) %>%
    mutate(
      inside = if_else(inside == TRUE, "diatonic", "nondiatonic"),
    ) %>%
    group_by(inside) %>%
    mutate(
      prct = n() / cnotes
    ) %>%
  ggplot(aes(x = inside, fill = inside)) +
    geom_bar() +
    scale_fill_manual(values = c("diatonic" = dia_col, "nondiatonic" = ndia_col)) +
    labs(
      x = x_titlelab, y = y_titlelab,
      title = if (p_title) pl_title else NULL,
      subtitle = if (s_title) pl_subtitle else NULL
    ) +
    theme(
      panel.background = element_rect(fill = panel_color),
      plot.background  = element_rect(fill = plot_color),

      panel.grid.major.x = if (x_gridmaj)
        element_line(color = x_gridmajcol, linewidth = x_gridmajwid) else element_blank(),

      panel.grid.major.y = if (y_gridmaj)
        element_line(color = y_gridmajcol, linewidth = y_gridmajwid) else element_blank(),

      panel.grid.minor.x = if (x_gridmin)
        element_line(color = x_gridmincol, linewidth = x_gridminwid) else element_blank(),

      panel.grid.minor.y = if (y_gridmin)
        element_line(color = y_gridmincol, linewidth = y_gridminwid) else element_blank(),

      axis.text.x = if (xaxis_text)
        element_text(color = xaxis_text_col, size = xaxis_text_size)
      else element_blank(),

      axis.text.y = if (yaxis_text)
        element_text(color = yaxis_text_col, size = yaxis_text_size)
      else element_blank(),

      axis.title.x = if (x_axistitle)
        element_text(color = x_axistitlecol, size = x_axistitlesize,
                     margin = margin(t = 10, b = 5))
      else element_blank(),

      axis.title.y = if (y_axistitle)
        element_text(color = y_axistitlecol, size = y_axistitlesize,
                     margin = margin(r = 10, l = 5))
      else element_blank(),

      plot.title = element_text(size = title_size,
                                color = title_col,
                                face = "bold",
                                hjust = title_align),
      plot.subtitle = element_text(size = sub_size,
                                   color = sub_col,
                                   face = "italic",
                                   hjust = sub_align)
    )

  if (flip) {
    p <- p +
      coord_flip() +
      theme(axis.text.y = element_text(angle = 90))
  }

  if (!dev_leg) {
    p <- p + guides(fill = "none")
  }

  if (show_prct) {
    p <- p + geom_text(
      stat = "count", size = prct_size,
      aes(label = paste0(round(prct * 100, 1), "%"),
          color = inside),
      y = prct_vert
    ) +
      scale_color_manual(values = c("diatonic" = dia_prct_col, "nondiatonic" = ndia_prct_col)) +
      guides(color = "none")
  }

  return(p)
}

#' Plot melodic contour by measure
#'
#' Generates a contour plot of average MIDI pitch across measures using time-sliced
#' note data. Supports optional smoothing, pitch range reference lines, and shading.
#'
#' @param time_df Data frame containing time-sliced note data. Must include `measure`,
#' `time`, and a list-column `midis`.
#' @param panel_color Background color of the panel.
#' @param plot_color Background color of the full plot.
#' @param line_width Width of the contour line.
#' @param line_alpha Transparency of the contour line.
#' @param line_color Color of the contour line.
#'
#' @param x_gridmaj Logical. Show major x grid lines.
#' @param x_gridmin Logical. Show minor x grid lines.
#' @param y_gridmaj Logical. Show major y grid lines.
#' @param y_gridmin Logical. Show minor y grid lines.
#' @param x_gridmajcol Color of major x grid lines.
#' @param y_gridmajcol Color of major y grid lines.
#' @param x_gridmincol Color of minor x grid lines.
#' @param y_gridmincol Color of minor y grid lines.
#' @param x_gridmajwid Width of major x grid lines.
#' @param y_gridmajwid Width of major y grid lines.
#' @param x_gridminwid Width of minor x grid lines.
#' @param y_gridminwid Width of minor y grid lines.
#'
#' @param xaxis_text Logical. Show x-axis text.
#' @param yaxis_text Logical. Show y-axis text.
#' @param xaxis_text_col Color of x-axis text.
#' @param yaxis_text_col Color of y-axis text.
#' @param xaxis_text_size Size of x-axis text.
#' @param yaxis_text_size Size of y-axis text.
#'
#' @param x_axistitle Logical. Show x-axis title.
#' @param y_axistitle Logical. Show y-axis title.
#' @param x_axistitlecol Color of x-axis title.
#' @param y_axistitlecol Color of y-axis title.
#' @param x_axistitlesize Size of x-axis title.
#' @param y_axistitlesize Size of y-axis title.
#'
#' @param by_lab Break interval for x-axis labels.
#' @param by_midi_lab Break interval for y-axis labels.
#'
#' @param p_title Logical. Show plot title.
#' @param s_title Logical. Show subtitle.
#' @param x_titlelab Label for x-axis.
#' @param y_titlelab Label for y-axis.
#' @param pl_title Plot title text.
#' @param pl_subtitle Plot subtitle text.
#' @param title_size Size of plot title.
#' @param title_col Color of plot title.
#' @param sub_size Size of subtitle.
#' @param sub_col Color of subtitle.
#' @param title_align Horizontal alignment of title.
#' @param sub_align Horizontal alignment of subtitle.
#'
#' @param highlow Logical. Add horizontal lines for highest and lowest pitch.
#' @param high_col Color of highest pitch line.
#' @param low_col Color of lowest pitch line.
#' @param high_size Width of highest pitch line.
#' @param low_size Width of lowest pitch line.
#'
#' @param shade Character. One of `"none"`, `"high"`, `"low"`, `"both"`.
#' @param shade_high Color for high shading.
#' @param shade_low Color for low shading.
#' @param shade_high_alpha Transparency for high shading.
#' @param shade_low_alpha Transparency for low shading.
#'
#' @param con_shape Logical. Add smoothed contour line.
#' @param con_size Width of smoothed line.
#' @param con_col Color of smoothed line.
#' @param con_span Span of smoothed line. Default is 0.75. Lower values produce a more jagged curve, higher values produce a smoother curve.
#'
#' @return A ggplot object.
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
#' contour_plot(time_df)
#' contour_plot(time_df, highlow = TRUE, shade = "both", con_shape = TRUE)
#' }
#'
#' @importFrom dplyr mutate filter group_by summarize arrange
#' @importFrom tidyr unnest
#' @importFrom ggplot2 ggplot aes geom_line geom_hline geom_ribbon scale_x_continuous scale_y_continuous labs theme element_rect element_line element_blank element_text margin
#'
#' @export
contour_plot <- function(time_df, panel_color = "#E0E0E0",
                         plot_color = "#E0E0E0",
                         line_width = 1, line_alpha = 1, line_color = "#6B8FA3",
                         x_gridmaj = TRUE, x_gridmin = TRUE,
                         y_gridmaj = TRUE, y_gridmin = TRUE,
                         x_gridmajcol = "#676767",
                         y_gridmajcol = "#676767",
                         x_gridmincol = "#676767",
                         y_gridmincol = "#676767",
                         x_gridmajwid = 0.4, y_gridmajwid = 0.4,
                         x_gridminwid = 0.3, y_gridminwid = 0.3,
                         xaxis_text = TRUE, yaxis_text = TRUE,
                         xaxis_text_col = "black",
                         xaxis_text_size = 10,
                         yaxis_text_col = "black",
                         yaxis_text_size = 10,
                         x_axistitle = TRUE, y_axistitle = TRUE,
                         x_axistitlecol = "black",
                         y_axistitlecol = "black",
                         x_axistitlesize = 10, y_axistitlesize = 10,
                         by_lab = 10, by_midi_lab = 5,
                         p_title = TRUE, s_title = TRUE,
                         x_titlelab = "Measure",
                         y_titlelab = "Avg. Midi",
                         pl_title = "Contour",
                         pl_subtitle = "Relative Shape of Music by Measure",
                         title_size = 12, title_col = "black",
                         sub_size = 10, sub_col = "black",
                         title_align = 0, sub_align = 0,
                         highlow = FALSE,
                         high_col = "darkred", low_col = "blue",
                         high_size = 1, low_size = 1,
                         shade = "none",
                         shade_high = "red", shade_low = "blue",
                         shade_high_alpha = 0.2,
                         shade_low_alpha = 0.2, con_shape = FALSE,
                         con_size = 1.1, con_col = "yellow",
                         con_span = 0.75){
  con_df <- time_df %>%
    mutate(
      measure = as.numeric(measure)
    ) %>%
    filter(!is.na(time)) %>%
    unnest(midis) %>%
    group_by(measure) %>%
    summarize(
      avg_midi = mean(midis)
    ) %>%
    arrange(as.numeric(measure))

  high_pitch <- max(unlist(time_df$midis), na.rm = TRUE)
  low_pitch <- min(unlist(time_df$midis), na.rm = TRUE)

  high_stop <- ceiling(
    ifelse(
      high_pitch > max(con_df$avg_midi, na.rm = TRUE),
      high_pitch, max(con_df$avg_midi, na.rm = TRUE)
    ) / by_midi_lab
  ) * by_midi_lab

  low_stop <- floor(
    ifelse(
      low_pitch < min(con_df$avg_midi, na.rm = TRUE),
      low_pitch, min(con_df$avg_midi, na.rm = TRUE)
    ) / by_midi_lab
  ) * by_midi_lab

  p <- ggplot(con_df, aes(x = measure, y = avg_midi, group = 1)) +
    geom_line(color = line_color, linewidth = line_width,
              alpha = line_alpha) +
    scale_x_continuous(breaks = c(1, seq(by_lab,
                                         max(con_df$measure) +
                                           by_lab,
                                         by = by_lab))) +
    scale_y_continuous(breaks = c(1, seq(low_stop,
                                         high_stop,
                                         by = by_midi_lab))) +
    labs(
      x = x_titlelab, y = y_titlelab,
      title = if (p_title) pl_title else NULL,
      subtitle = if (s_title) pl_subtitle else NULL
    ) +
    theme(
      plot.background = element_rect(fill = plot_color),
      panel.background = element_rect(fill = panel_color),

      panel.grid.major.x = if (x_gridmaj)
        element_line(color = x_gridmajcol, linewidth = x_gridmajwid) else element_blank(),

      panel.grid.major.y = if (y_gridmaj)
        element_line(color = y_gridmajcol, linewidth = y_gridmajwid) else element_blank(),

      panel.grid.minor.x = if (x_gridmin)
        element_line(color = x_gridmincol, linewidth = x_gridminwid) else element_blank(),

      panel.grid.minor.y = if (y_gridmin)
        element_line(color = y_gridmincol, linewidth = y_gridminwid) else element_blank(),

      axis.text.x = if (xaxis_text)
        element_text(color = xaxis_text_col, size = xaxis_text_size)
      else element_blank(),

      axis.text.y = if (yaxis_text)
        element_text(color = yaxis_text_col, size = yaxis_text_size)
      else element_blank(),

      axis.title.x = if (x_axistitle)
        element_text(color = x_axistitlecol, size = x_axistitlesize,
                     margin = margin(t = 10, b = 5))
      else element_blank(),

      axis.title.y = if (y_axistitle)
        element_text(color = y_axistitlecol, size = y_axistitlesize,
                     margin = margin(r = 10, l = 5))
      else element_blank(),

      plot.title = element_text(size = title_size,
                                color = title_col,
                                face = "bold",
                                hjust = title_align),
      plot.subtitle = element_text(size = sub_size,
                                   color = sub_col,
                                   face = "italic",
                                   hjust = sub_align)
    )

  if (highlow) {
    p <- p +
      annotate("segment",
               x = min(con_df$measure, na.rm = TRUE),
               xend = max(con_df$measure, na.rm = TRUE),
               y = high_pitch, yend = high_pitch,
               color = high_col, linewidth = high_size
      ) +
      annotate("segment",
               x = min(con_df$measure, na.rm = TRUE),
               xend = max(con_df$measure, na.rm = TRUE),
               y = low_pitch, yend = low_pitch,
               color = low_col, linewidth = low_size
      )
  }

  if (shade == "high") {
    p <- p +
      geom_ribbon(
        aes(ymin = avg_midi, ymax = high_pitch),
        fill = shade_high, alpha = shade_high_alpha
      )

  } else if (shade == "low") {
    p <- p +
      geom_ribbon(
        aes(ymin = low_pitch, ymax = avg_midi),
        fill = shade_low, alpha = shade_low_alpha
      )

  } else if (shade == "both") {
    p <- p +
      geom_ribbon(
        aes(ymin = avg_midi, ymax = high_pitch),
        fill = shade_high, alpha = shade_high_alpha
      ) +
      geom_ribbon(
        aes(ymin = low_pitch, ymax = avg_midi),
        fill = shade_low, alpha = shade_low_alpha
      )
  }

  if (con_shape) {
    p <- p +
      geom_smooth(method = "loess", se = FALSE,
                  linewidth = con_size, color = con_col,
                  span = con_span)
  }

  return(p)
}

#' Plot pitch class frequency profile
#'
#' Creates a polar bar chart showing the frequency distribution of pitch classes
#' across a piece, with customizable color groupings and decorative center rings.
#'
#' @param notes_df A data frame containing note-level data.
#' @param panel_color Background color of the panel.
#' @param plot_color Background color of the full plot.
#'
#' @param xaxis_text Logical indicating whether to show x-axis text.
#' @param yaxis_text Logical indicating whether to show y-axis text.
#' @param xaxis_text_col Color of x-axis text.
#' @param xaxis_text_size Size of x-axis text.
#' @param yaxis_text_col Color of y-axis text.
#' @param yaxis_text_size Size of y-axis text.
#'
#' @param x_gridmaj Logical for major x grid.
#' @param y_gridmaj Logical for major y grid.
#' @param x_gridmin Logical for minor x grid.
#' @param y_gridmin Logical for minor y grid.
#' @param x_gridmajcol Color of major x grid lines.
#' @param y_gridmajcol Color of major y grid lines.
#' @param x_gridmincol Color of minor x grid lines.
#' @param y_gridmincol Color of minor y grid lines.
#' @param x_gridmajwid Width of major x grid lines.
#' @param y_gridmajwid Width of major y grid lines.
#' @param x_gridminwid Width of minor x grid lines.
#' @param y_gridminwid Width of minor y grid lines.
#'
#' @param x_axistitle Logical indicating whether to show x-axis title.
#' @param y_axistitle Logical indicating whether to show y-axis title.
#' @param x_axistitlecol Color of x-axis title.
#' @param y_axistitlecol Color of y-axis title.
#' @param x_axistitlesize Size of x-axis title.
#' @param y_axistitlesize Size of y-axis title.
#' @param x_titlelab Label for x-axis.
#' @param y_titlelab Label for y-axis.
#'
#' @param s_title Logical indicating whether to show subtitle.
#' @param p_title Logical indicating whether to show title.
#' @param pl_title Text of the plot title.
#' @param pl_subtitle Text of the plot subtitle.
#' @param title_size Size of plot title.
#' @param title_col Color of plot title.
#' @param sub_size Size of subtitle.
#' @param sub_col Color of subtitle.
#' @param title_align Horizontal alignment of title.
#' @param sub_align Horizontal alignment of subtitle.
#'
#' @param leg_title Logical indicating whether to show legend title.
#' @param leg_text Label for legend title if \code{leg_title = TRUE}.
#' @param leg_textcol Color of legend text.
#' @param leg_textsize Size of legend text.
#' @param leg_bg Background color of the legend.
#'
#' @param show_counts Logical indicating whether to show note counts above bars.
#' @param count_vert1 Vertical offset for count labels on the highest quantile group.
#' @param count_vert2 Vertical offset for count labels on all other quantile groups.
#'
#' @param inner_size Size of the decorative center circle.
#' @param center_fill Fill color of the center circle.
#' @param center_line Border color of the center circle.
#' @param decor Logical indicating whether to show decorative center rings.
#' @param decor_circles Number of decorative rings to draw inside the center circle.
#' @param decor_col Border color of the decorative rings.
#'
#' @param show_total Logical indicating whether to show the total note count in the center circle.
#' @param total_size Size of the total note count text.
#' @param total_col Color of the total note count text.
#'
#' @param colors A character vector of colors defining fill groups. The number of
#'   colors determines the number of quantile groups (e.g. 5 colors = quintiles,
#'   4 colors = quartiles).
#'
#' @return A ggplot object.
#'
#' @details
#' The function computes pitch class frequencies from MIDI note numbers and assigns
#' each pitch class to a quantile group based on the distribution of counts. Bar
#' height and fill color reflect group membership. The number of groups is determined
#' entirely by the length of the \code{colors} vector.
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("path/to/file.xml")
#' notes_df <- parse_xml(xml)
#' notes_df <- expand_notes_df(notes_df, xml, "My Score")
#'
#' profile_plot(notes_df)
#' profile_plot(notes_df, colors = c("#001f3f", "#0074D9", "#7FDBFF", "#39CCCC"))
#' profile_plot(notes_df, decor = FALSE, show_counts = FALSE)
#' }
#'
#' @importFrom dplyr filter mutate select count
#' @importFrom tidyr complete
#' @importFrom stats quantile setNames
#' @importFrom utils head
#' @import ggplot2
#'
#' @export
profile_plot <- function(notes_df,
                         panel_color = "#E0E0E0",
                         plot_color = "#E0E0E0",
                         xaxis_text = TRUE, yaxis_text = FALSE,
                         xaxis_text_col = "black",
                         xaxis_text_size = 10,
                         yaxis_text_col = "black",
                         yaxis_text_size = 10,
                         x_gridmaj = TRUE, y_gridmaj = FALSE,
                         x_gridmin = TRUE, y_gridmin = FALSE,
                         x_gridmajcol = "#404040", y_gridmajcol = "#BDBDBD",
                         x_gridmincol = "#BDBDBD", y_gridmincol = "#BDBDBD",
                         x_gridmajwid = 0.4, y_gridmajwid = 0.4,
                         x_gridminwid = 0.3, y_gridminwid = 0.3,
                         x_axistitle = FALSE, y_axistitle = FALSE,
                         x_axistitlecol = "black",
                         y_axistitlecol = "black",
                         x_axistitlesize = 10,
                         y_axistitlesize = 10,
                         x_titlelab = "MIDI Number",
                         y_titlelab = "Frequency",
                         s_title = TRUE, p_title = TRUE,
                         pl_title = "Pitch Profile",
                         pl_subtitle = "Pitch Frequency",
                         title_size = 12, title_col = "black",
                         sub_size = 10, sub_col = "black",
                         title_align = 0, sub_align = 0,
                         leg_title = FALSE, leg_text = "Note Count",
                         leg_textcol = "white", leg_textsize = 11.5,
                         leg_bg = "black", show_counts = TRUE,
                         center_fill = "#E0E0E0", center_line = "#E0E0E0",
                         count_vert1 = -10, count_vert2 = 15,
                         inner_size = 25, show_total = TRUE,
                         total_size = 4, total_col = "black",
                         decor = FALSE, decor_circles = 3, decor_col = "black",
                         colors = c("#770771", "#9E1597", "#D52FCD",
                                    "#EB6FE5", "#F5A6F1")) {

  n_groups <- length(colors)
  probs <- seq(1/n_groups, 1, by = 1/n_groups)
  plot_sizes <- seq(50, 50 + (n_groups - 1) * 25, by = 25)

  pro_df <- notes_df %>%
    select(midi_num, isrest, fifths, mode) %>%
    filter(!isrest) %>%
    mutate(mod12 = midi_num %% 12) %>%
    select(mod12, fifths, mode)

  mod_count <- pro_df %>%
    count(mod12) %>%
    complete(mod12 = 0:11, fill = list(n = 0)) %>%
    mutate(
      quant_group = findInterval(n, quantile(n, probs = head(probs, -1))) + 1,
      plot_n = plot_sizes[quant_group],
      fill_group = paste0("q", quant_group)
    )

  total <- sum(mod_count$n)
  cut_points <- total * probs

  p <- ggplot(mod_count, aes(factor(mod12), n)) +
    geom_col(aes(y = plot_n, fill = fill_group)) +
    scale_fill_manual(
      values = setNames(colors, paste0("q", 1:n_groups)),
      breaks = paste0("q", 1:n_groups),
      labels = paste0(c(0, head(cut_points, -1) + 1), " - ", cut_points)
    ) +
    scale_x_discrete(
      labels = paste0(
        c("C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B"),
        "\n(",
        mod_count$n,
        ")"
      )
    ) +
    labs(
      x = if (x_axistitle) x_titlelab else NULL,
      y = if (y_axistitle) y_titlelab else NULL,
      title = if (p_title) pl_title else NULL,
      subtitle = if (s_title) pl_subtitle else NULL
    ) +
    annotate(
      "point", x = 1, y = 0, size = inner_size, shape = 21,
      fill = center_fill, color = center_line
    ) +
    coord_polar() +
    theme(
      panel.background = element_rect(fill = panel_color),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = plot_color),

      panel.grid.major.x = if (x_gridmaj)
        element_line(color = x_gridmajcol, linewidth = x_gridmajwid) else element_blank(),

      panel.grid.major.y = if (y_gridmaj)
        element_line(color = y_gridmajcol, linewidth = y_gridmajwid) else element_blank(),

      panel.grid.minor.x = if (x_gridmin)
        element_line(color = x_gridmincol, linewidth = x_gridminwid) else element_blank(),

      panel.grid.minor.y = if (y_gridmin)
        element_line(color = y_gridmincol, linewidth = y_gridminwid) else element_blank(),

      axis.text.x = if (xaxis_text)
        element_text(color = xaxis_text_col, size = xaxis_text_size)
      else element_blank(),

      axis.text.y = if (yaxis_text)
        element_text(color = yaxis_text_col, size = yaxis_text_size)
      else element_blank(),

      axis.title.x = if (x_axistitle)
        element_text(color = x_axistitlecol, size = x_axistitlesize,
                     margin = margin(t = 10, b = 5))
      else element_blank(),

      axis.title.y = if (y_axistitle)
        element_text(color = y_axistitlecol, size = y_axistitlesize,
                     margin = margin(r = 10, l = 5))
      else element_blank(),

      axis.ticks = element_blank(),

      plot.title = element_text(size = title_size,
                                color = title_col,
                                face = "bold",
                                hjust = title_align),
      plot.subtitle = element_text(size = sub_size,
                                   color = sub_col,
                                   face = "italic",
                                   hjust = sub_align),

      legend.position = "bottom",

      legend.title = if (leg_title)
        element_text(label = leg_text) else element_blank(),
      legend.background = element_rect(fill = leg_bg),
      legend.text = element_text(color = leg_textcol, size = leg_textsize)
    )

  if (decor) {
    sizes <- seq(inner_size, 0, length.out = decor_circles + 1)[-( decor_circles + 1)]
    for (i in seq_len(decor_circles)) {
      p <- p + annotate(
        "point", x = 1, y = 0, size = sizes[i], shape = 21,
        fill = center_fill, color = decor_col
      )
    }
  }

  if (show_total) {
    p <- p + annotate(
      "text", x = 1, y = 3,
      label = paste0("n =\n", total),
      size = total_size,
      color = total_col
    )
  }

  return(p)
}
