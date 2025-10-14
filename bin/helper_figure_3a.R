
# load data analytics libraries -----------------------

library(data.table)
library(stringr)

# load plotting libraries ------------------

library(ggplot2)
library(ggrepel)
library(ggtext)
library(ggh4x)

library(extrafont)
library(paletteer)
library(colorspace)

# helper function -----------------------

plot_figure_3A <- function() {

  tools_url     <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_2.tsv"
  workflows_url <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_5.tsv"
  tutorials_url <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_6.tsv"

  tools_dt     <- tools_url |> fread()
  workflows_dt <- workflows_url |> fread()
  tutorials_dt <- tutorials_url |> fread()

  d1 <- tools_dt[, by = `Suite first commit date`, .N]
  d2 <- tutorials_dt[, by = Creation, .N]
  d3 <- workflows_dt[, by = `Creation time`, .N]

  d1 <- d1[order(`Suite first commit date`)]
  d2 <- d2[order(Creation)]
  d3 <- d3[order(`Creation time`)]

  d1$ypos <- cumsum(d1$N)
  d2$ypos <- cumsum(d2$N)
  d3$ypos <- cumsum(d3$N)

  d1 <- d1[, c("Suite first commit date", "ypos"), with = FALSE]
  d2 <- d2[, c("Creation", "ypos"), with = FALSE]
  d3 <- d3[, c("Creation time", "ypos"), with = FALSE]

  colnames(d1) <- c("Date", "ypos")
  colnames(d2) <- c("Date", "ypos")
  colnames(d3) <- c("Date", "ypos")

  d1$category <- "Tool suites"
  d2$category <- "Tutorials"
  d3$category <- "Workflows"

  d4 <- d2 |> merge(d3, by = "Date", all = TRUE)

  d4$category.x <- "Tutorials"
  d4$category.y <- "Workflows"

  for(i in seq_along(d4$Date)) {
      
    if(i == 1) {
      d4[i]$ypos.x <- 1
      d4[i]$ypos.y <- 1
    } 
    
    if( is.na(d4[i]$ypos.x) ) d4[i]$ypos.x <- d4[i - 1]$ypos.x
    if( is.na(d4[i]$ypos.y) ) d4[i]$ypos.y <- d4[i - 1]$ypos.y
      
  }

  gr <- ggplot() +
    geom_area(data = d1, aes(Date, ypos), fill = "#4E79A7", alpha = .1) +
    
    stat_difference(data = d4, aes(Date, ymin = ypos.y, ymax = ypos.x), alpha = 0.5, levels = c("More tutorials", "More workflows"),) +

    geom_line(data = d1, aes(Date, ypos, color = category), linewidth = .75) +
    geom_line(data = d4, aes(Date, ypos.x, color = category.x), linewidth = .75) +
    geom_line(data = d4, aes(Date, ypos.y, color = category.y), linewidth = .75) +
    
    scale_x_date(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(50, 300, by = 50), limits = c(0, 300)) +
    
    scale_fill_manual(values = c("More tutorials" = "#F28E2B", "More workflows" = "#E15759")) +
    scale_color_manual(values = c("Tool suites" = "#4E79A7", "Tutorials" = "#F28E2B", "Workflows" = "#E15759")) +
    theme_minimal(base_family = "Calibri") +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      legend.position.inside = c(.25, .75),
      legend.position = "inside",
      
      panel.grid.major = element_line(linewidth = .3, linetype = "dashed", lineend = "round", color = "grey75"),
      
      axis.title.x = element_markdown(margin = margin(t = 10), face = "bold", size = 14),
      axis.title.y = element_markdown(margin = margin(r = 10), size = 14),
      
      axis.text = element_text(size = 14),
      
      axis.line = element_line(lineend = "round"),
      axis.ticks = element_line(lineend = "round")
    ) +
    labs(y = "Cumulative number of<br>**tool suites**, **tutorials**, and **workflows**")

  return(gr)
}