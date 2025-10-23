message("Figure 3 ...\t", appendLF = FALSE)

suppressWarnings({
  # Load helpers --------------------------

  suppressMessages({
    source("bin/helper_figure_3a.R")
    source("bin/helper_figure_3b.R")
    source("bin/helper_figure_3c.R")

    # Load `patchwork` ---------------

    library(patchwork, quietly = TRUE)
  })

  # patch plots -----------------

  a <- plot_figure_3A()
  b <- plot_figure_3B()
  c <- plot_figure_3C()

  multi <- ((a | b) / c) +
    plot_annotation(tag_levels = "A") &
    theme(
      plot.tag = element_text(face = "bold", size = 22, family = "Calibri"),
      plot.margin = margin(10, 10, 10, 15)
    )

  outfolder <- "../docs/extended/"

  dir.create(outfolder, showWarnings = FALSE)

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

  save_plot(multi, "./docs/figures/figure_3", 10, 12)
})


message("done!")
