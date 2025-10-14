
# Load helpers --------------------------

source("bin/helper_figure_1a.R")
source("bin/helper_figure_1b.R")

# Load `patchwork` ---------------

library(patchwork)


# patch plots -----------------

a <- plot_figure_1A()
b <- plot_figure_1B()

multi <- (free(b$f2b) | free(a)) +
  plot_layout(widths = c(1, 1.25)) +
  plot_annotation(tag_levels = "A") &
  theme(
      plot.tag = element_text(size = 22, face = "bold", family = "Calibri")
  )

multi2 <- (free(b$f2b_s) | free(a)) +
  plot_layout(widths = c(1, 1.25)) +
  plot_annotation(tag_levels = "A") &
  theme(
      plot.tag = element_text(size = 22, face = "bold", family = "Calibri")
  )

save_plot <- function(plot, filename, w, h) {
    
    ggsave(
        plot = plot, filename = paste0(filename, ".png"),
        width = w, height = h, units = "in", dpi = 600
    )
    
    ggsave(
        plot = plot, filename = paste0(filename, ".svg"),
        width = w, height = h, units = "in", dpi = 600
    )
    
    ggsave(
        plot = plot, filename = paste0(filename, ".pdf"),
        width = w, height = h, units = "in", device = cairo_pdf
    )
    
}

save_plot(multi, "../docs/figures/figure_2", 16, 8)
save_plot(multi2, "../docs/figures/figure_2_v2", 16, 8)