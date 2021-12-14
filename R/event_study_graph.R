#' Graphs an event study with no labs.
#'
#' @description
#' `event_study_graph` returns a ggplot object of the event study graph.
#'
#' @details
#' This function relies on dplyr, ggplot, and tibble packages to work.
#'
#' @param x a regression fixest object.
#' @param window_size the length of the event window.
#' @param reference_period the period to be omitted, defaults to period before. This is found by counting from last period to period wanted.
#'
#' @examples
#' event_study_graph(x, 8, 8)
#'
#' @export event_study_graph
#'



event_study_graph <- function(x, window_size, reference_period = window_size) {
  coefs <- broom::tidy(x, conf.int =T)
  window_size_times_two <- window_size *2
  coefs <- coefs %>%
    add_row(.before = reference_period) %>%
    mutate(time = c(-window_size:window_size)) %>%
    mutate(across(.cols = -c(term), ~ ifelse(is.na(.), 0, .))) %>%
    slice(2:window_size_times_two) %>%
    mutate(across(starts_with("conf"), ~ifelse(. == 0, NA, .)))
  plot <- coefs %>%
    ggplot(aes(x = time, y = estimate), alpha = 0.8) +
    scale_x_continuous(labels = c(-window_size:-1, "In Moratorium\n(64 day average)",1:window_size), breaks = c(-window_size:window_size)) +
    geom_rect(aes(xmin = -.5, xmax = 0.5, ymin = -Inf, ymax = Inf), fill = "cornsilk2", alpha = 0.1) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha =0.8) +
    geom_point() +
    theme_minimal() +
    geom_hline(yintercept = 0, color = "dark red", linetype = "dashed")
  return(plot)
}
