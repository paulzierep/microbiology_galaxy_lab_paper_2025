# Load data analytics `libraries` ----------------------

library(data.table, quietly = TRUE)
library(stringr, quietly = TRUE)

# Load plotting `libraries` -----------------------

library(ggstream, quietly = TRUE)
library(ggtext, quietly = TRUE)
library(colorspace, quietly = TRUE)

# helper function -------------------------

plot_figure_2B <- function() {
  ##  input data -------------------

  all_papers <- fread("https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/results/citations/all_papers.csv")
  all_papers <- all_papers[which(year >= 2006)]


  microbial_papers <- fread("https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/results/citations/microbial_papers.csv")
  microbial_papers <- microbial_papers[which(year >= 2006)]

  ## Barplot --------------------

  # p0 <- all

  p0 <- all_papers[, by = year, .(counts = title |> unique() |> length())]
  p1 <- microbial_papers[, by = .(year, Topics), .(counts = title |> unique() |> length())]

  p0$Topics <- "All"

  gr1_df <- rbind(p0, p1)

  gr1_df$Topics <- ifelse(
    gr1_df$Topics == "All", "Non microbiology-related",
    ifelse(
      gr1_df$Topics == "", "Unlabeled microbiology-related",
      gr1_df$Topics
    )
  )

  # gr1_df$counts2 <- log10(gr1_df$counts)

  gr1_df$Topics <- gr1_df$Topics |> factor(levels = c(
    "Ecosystem Dynamics & Biodiversity",
    "Ecosystem Dynamics & Biodiversity, Health & Disease",
    "Health & Disease",
    "Unlabeled microbiology-related",
    "Non microbiology-related"
  ))

  gr1 <- gr1_df |>
    ggplot(aes(year, counts)) +
    geom_stream(aes(fill = Topics), color = "grey25", linewidth = .25, type = "ridge") +
    geom_vline(xintercept = c(2010, 2015, 2017, 2020, 2023), linewidth = .5, linetype = "dotted", lineend = "round", color = "white") +

    # geom_stream(aes(fill = Topics), type = "ridge") +

    # geom_line(aes(color = variable, group = variable)) +

    # geom_point(aes(fill = Topics), color = "grey25",
    #            shape = 21, size = 5.3 - 1.23, stroke = .25,
    #            position = position_dodge(width = .9)) +

    # geom_col(aes(fill = Topics), color = "grey25",
    #          linewidth = .15, position = "stack") +

    # geom_point(aes(color = Topics), size = 4.65 - 1.23,
    #            position = position_dodge(width = .9)) +

    scale_x_continuous(expand = c(0, 0), breaks = c(2006, 2010, 2015, 2017, 2020, 2023, 2025)) +
    scale_y_continuous(breaks = seq(100, 1350, by = 100), expand = c(0, 0), limits = c(0, 1350)) +
    # scale_y_continuous(expand = c(0, 0), breaks = c(10, 20, 50, 100, 200, 500, 1000)) +
    # scale_y_continuous(expand = c(0, 0), transform = "log2", limits = c(0, 1100), breaks = seq(200, 1000, by = 200)) +

    scale_fill_manual(values = c(
      "Non microbiology-related" = "grey" |> lighten(.25),
      "Unlabeled microbiology-related" = "#6abd66" |> darken(.1),
      "Ecosystem Dynamics & Biodiversity" = "#7ca07aff",
      "Ecosystem Dynamics & Biodiversity, Health & Disease" = "#BCB45D",
      "Health & Disease" = "#fbc73fff"
    )) +
    # scale_color_manual(values = c("All" = "#4E79A7" |> lighten(.25),
    #                               "Microbial" = "#E15759" |> darken(.3))) +

    coord_cartesian(clip = "off") +
    theme_minimal(base_family = "Calibri") +
    theme(
      legend.position = "inside",
      legend.position.inside = c(.35, .85),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),

      # axis.line.x = element_line(lineend = "round"),
      # axis.ticks.y = element_line(lineend = "round"),

      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(linetype = "dashed", lineend = "round"),
      axis.text.x = element_text(face = "bold", size = 12),
      axis.text.y = element_text(face = "bold", size = 12),
      axis.title.x = element_text(margin = margin(t = 10), size = 12),
      axis.title.y = element_markdown(margin = margin(r = 10), size = 12)
    ) +
    labs(x = "Year", y = "**Number of publications** citing Galaxy papers")

  # gr2 <- gr1_df[which(Topics == "All")] |>
  #   ggplot(aes(year, counts)) +
  #   geom_stream(aes(fill = Topics), color = "grey25", linewidth = .25, type = "ridge") +
  #   geom_vline(xintercept = c(2010, 2015, 2017, 2020, 2023), linewidth = .5, linetype = "dotted", lineend = "round", color = "white") +
  # 
  #   # geom_stream(aes(fill = Topics), type = "ridge") +
  # 
  #   # geom_line(aes(color = variable, group = variable)) +
  # 
  #   # geom_point(aes(fill = Topics), color = "grey25",
  #   #            shape = 21, size = 5.3 - 1.23, stroke = .25,
  #   #            position = position_dodge(width = .9)) +
  # 
  #   # geom_col(aes(fill = Topics), color = "grey25",
  #   #          linewidth = .15, position = "stack") +
  # 
  #   # geom_point(aes(color = Topics), size = 4.65 - 1.23,
  #   #            position = position_dodge(width = .9)) +
  # 
  #   scale_x_continuous(expand = c(0, 0), breaks = c(2006, 2010, 2015, 2017, 2020, 2023, 2025)) +
  #   scale_y_continuous(breaks = seq(100, 1350, by = 100), expand = c(0, 0), limits = c(0, 1000)) +
  #   # scale_y_continuous(expand = c(0, 0), breaks = c(10, 20, 50, 100, 200, 500, 1000)) +
  #   # scale_y_continuous(expand = c(0, 0), transform = "log2", limits = c(0, 1100), breaks = seq(200, 1000, by = 200)) +
  # 
  #   scale_fill_manual(values = c(
  #     "Non microbiology-related" = "grey" |> lighten(.25),
  #     "Unlabeled microbiology-related" = "#6abd66" |> darken(.1),
  #     "Ecosystem Dynamics & Biodiversity" = "#7ca07aff",
  #     "Ecosystem Dynamics & Biodiversity, Health & Disease" = "#BCB45D",
  #     "Health & Disease" = "#fbc73fff"
  #   )) +
  #   # scale_color_manual(values = c("All" = "#4E79A7" |> lighten(.25),
  #   #                               "Microbial" = "#E15759" |> darken(.3))) +
  # 
  #   coord_cartesian(clip = "off") +
  #   theme_minimal(base_family = "Calibri") +
  #   theme(
  #     legend.position = "inside",
  #     legend.position.inside = c(.35, .85),
  #     legend.text = element_text(size = 12),
  #     legend.title = element_text(size = 12),
  #     strip.text = element_text(size = 16),
  # 
  #     # axis.line.x = element_line(lineend = "round"),
  #     # axis.ticks.y = element_line(lineend = "round"),
  # 
  #     panel.grid.major.x = element_blank(),
  #     panel.grid.minor.x = element_blank(),
  #     panel.grid.minor.y = element_line(linetype = "dashed", lineend = "round"),
  #     axis.text.x = element_text(face = "bold", size = 12),
  #     axis.text.y = element_text(face = "bold", size = 12),
  #     axis.title.x = element_text(margin = margin(t = 10), size = 12),
  #     axis.title.y = element_markdown(margin = margin(r = 10), size = 12)
  #   ) +
  #   labs(x = "Year", y = "**Number of publications** citing Galaxy papers")

  return(list("f2b" = gr1))
}
