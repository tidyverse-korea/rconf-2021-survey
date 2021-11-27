library(magrittr)

parse_checkboxes <- function(df, var, sep = ";") {
  var <- rlang::enquo(var)
  
  res <- df %>% 
    dplyr::select(response_id, !!var) %>% 
    dplyr::mutate(value = stringr::str_split(!!var, pattern = sep)) %>% 
    tidyr::unnest(cols = value)
  
  return(res)
}

resolve_dup <- function(df, match_vec = c(NA)) {
  var <- rlang::enquo(var)
  
  res <- df %>% 
    dplyr::select(response_id, value) %>% 
    dplyr::mutate(value = stringr::str_trim(value)) %>%
    dplyr::mutate(value = dplyr::coalesce(match_vec[value], value)) %>% 
    dplyr::group_by(response_id, value) %>% 
    dplyr::slice(1L) %>% 
    dplyr::ungroup()
  
  return(res)
}


plot_checkboxes <- function(.df, .var, .match_vec = c(NA), ..., 
                            .color = c(NA),
                            .min_response = 1L) {
  .var <- rlang::enquo(.var)
  
  n <- nrow(.df)
  
  plot_df <- parse_checkboxes(.df, !!.var, ...) %>% 
    resolve_dup(.match_vec) %>% 
    dplyr::mutate(
      value = forcats::fct_infreq(value) %>% 
        forcats::fct_rev() %>% 
        forcats::fct_lump_min(.min_response, other_level = "기타 응답") %>% 
        forcats::fct_relevel("기타 응답", after = 0L) %>%
        forcats::fct_explicit_na("무응답") %>% 
        forcats::fct_relevel("무응답", after = 0L)
    ) %>% 
    dplyr::filter(value != "기타 응답") %>% 
    dplyr::group_by(value) %>% 
    dplyr::summarize(
      n = dplyr::n(),
      prop = n / .env$n,
      .groups = "drop"
    )
  
  unique_value <- unique(as.character(plot_df$value))
  color <- tibble::tibble(value = unique_value) %>%
    dplyr::mutate(color = dplyr::coalesce(.color[value], "grey60")) %>% 
    tibble::deframe()
  
  p <- plot_df %>% 
    ggplot2::ggplot(ggplot2::aes(x = value, y = prop)) +
    ggplot2::geom_col(
      ggplot2::aes(fill = value),
      width = 0.6, alpha = 0.7
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = scales::percent(prop, accuracy = 1L),
        # hjust = if_else(prop > 0.1, 1.1, -0.1)
      ),
      alpha = 0.7,
      size = 6,
      hjust = -0.1
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::scale_fill_manual(values = color) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::coord_flip() +
    ggplot2::theme_light(base_size = 24) +
    ggplot2::theme(
      plot.title.position = "plot",
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  
  return(p)
}



plot_radiobutton <- function(.df, .var, .match_vec = c(NA), ..., 
                            .color = c(NA),
                            .min_response = 1L) {
  .var <- rlang::enquo(.var)
  
  n <- nrow(.df)
  
  plot_df <- dplyr::select(.df, response_id, !!.var) %>% 
    dplyr::rename("value" = rlang::as_name(.var)) %>% 
    resolve_dup(.match_vec) %>% 
    dplyr::mutate(
      value = forcats::fct_infreq(value) %>% 
        forcats::fct_rev() %>% 
        forcats::fct_lump_min(.min_response, other_level = "기타 응답") %>% 
        forcats::fct_relevel("기타 응답", after = 0L) %>%
        forcats::fct_explicit_na("무응답") %>% 
        forcats::fct_relevel("무응답", after = 0L)
    ) %>% 
    dplyr::group_by(value) %>% 
    dplyr::summarize(
      n = dplyr::n(),
      prop = n / .env$n,
      .groups = "drop"
    )
  
  unique_value <- unique(as.character(plot_df$value))
  color <- tibble::tibble(value = unique_value) %>%
    dplyr::mutate(color = dplyr::coalesce(.color[value], "grey60")) %>% 
    tibble::deframe()
  
  p <- plot_df %>% 
    ggplot2::ggplot(ggplot2::aes(x = value, y = prop)) +
    ggplot2::geom_col(
      ggplot2::aes(fill = value),
      width = 0.6, alpha = 0.7
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = scales::percent(prop, accuracy = 1L),
        # hjust = if_else(prop > 0.1, 1.1, -0.1)
      ),
      alpha = 0.7,
      size = 6,
      hjust = -0.1
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::scale_fill_manual(values = color) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::coord_flip() +
    ggplot2::theme_light(base_size = 24) +
    ggplot2::theme(
      plot.title.position = "plot",
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  
  return(p)
}


