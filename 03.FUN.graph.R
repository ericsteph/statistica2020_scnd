# Grafico "classico", confronto Ticino e Svizzera
# valori ABS
plot_classic_abs <- function(d, z,
                             minimo,
                             titolo,
                             sottotitolo,
                             fonte,
                             cadenza,
                             scales = "fixed",
                             nrow = 1){
  
  k <- (max_anno - primo_anno)/cadenza
  
  z <- sym(z)
  
  d %>% ggplot() +
    aes(x = Trimestre, y = values, group = !!z) +
    geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "loess") +
    geom_line(na.rm = TRUE, size = 1.2) +
    facet_wrap(paste("~", z), scales = scales, nrow = nrow) +
    scale_x_yearqtr(n = k, format = "T%q\n%Y") +
    expand_limits(y = minimo) +
    scale_y_continuous(expand = c(.15, .3)) +
    labs(x = "", y = "",
         title = paste0(titolo, " ", primo_anno),
         subtitle = sottotitolo,
         caption = fonte) +
    theme_bw() +
    theme(
      plot.title = element_text(size = rel(0.8), face = "bold"),
      plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
      plot.caption = element_text(size = rel(0.6), colour = "gray"),
      axis.text = element_text(size = rel(0.5)))
}

# Grafico "classico", confronto Ticino e Svizzera
# in PERC
plot_classic_perc <- function(d, var.y,
                              titolo,
                              sottotitolo,
                              fonte,
                              primo_anno,
                              cadenza,
                              nrow = 1){
  
  k <- (max_anno - primo_anno)/cadenza
  
  z <- sym(z)
  
  min.value <- min(d$min.value)
  max.value <- max(d$max.value)
  
  d %>% ggplot() +
    aes(x = Trimestre, y = values, group = !!z) +
    geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "loess") +
    geom_line(na.rm = TRUE, size = 1.2) +
    facet_wrap(paste("~", z), scales = "free_y", nrow = nrow) +
    scale_x_yearqtr(n = k, format = "T%q\n%Y") +
    geom_blank(aes(y = round(min.value*0.8125, -2))) +
    geom_blank(aes(y = round(max.value*1.075, -2))) +
    labs(x = "", y = "",
         title = paste0(titolo, " ", primo_anno),
         subtitle = sottotitolo,
         caption = fonte) +
    theme_bw() +
    theme(
      plot.title = element_text(size = rel(0.8), face = "bold"),
      plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
      plot.caption = element_text(size = rel(0.6), colour = "gray"),
      axis.text = element_text(size = rel(0.5)))
}