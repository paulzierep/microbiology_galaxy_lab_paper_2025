# Load data analytics libraries -------------------

library(data.table)
library(stringr)

# load plotting libraries --------------------

library(ggdensity)
library(ggdist)
library(ggforce)

library(tidytext)

# helper function -------------------------

plot_figure_3C <- function() {
  tools_url <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_2.tsv"
  workflows_url <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_5.tsv"
  tutorials_url <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_6.tsv"

  tools_dt <- tools_url |> fread()
  workflows_dt <- workflows_url |> fread()
  tutorials_dt <- tutorials_url |> fread()

  df2 <- tools_dt |>
    tidyr::separate_rows("EDAM reduced operations", sep = ",") |>
    setDT()

  w_dt <- workflows_dt |>
    tidyr::separate_rows("Tools", sep = ",") |>
    setDT()
  t_dt <- tutorials_dt |>
    tidyr::separate_rows("Tools", sep = ",") |>
    setDT()

  w_dt$Tools <- w_dt$Tools |> str_squish()
  t_dt$Tools <- t_dt$Tools |> str_squish()

  df2$`Included in workflows` <- df2$`Suite ID` %in% w_dt$Tools
  df2$`Included in tutorials` <- df2$`Suite ID` %in% t_dt$Tools

  df2$`Included in` <- ifelse(df2$`Included in workflows` & df2$`Included in tutorials`, "Both",
    ifelse(df2$`Included in tutorials`, "Tutorials",
      ifelse(df2$`Included in workflows`, "Workflows", "Ν/Α")
    )
  )

  xvar <- "Suite runs (last 5 years) on main servers"
  yvar <- "Suite users (last 5 years) on main servers"

  df2$runs <- df2[[xvar]]
  df2$users <- df2[[yvar]]

  df2 <- df2[which(runs != 0 & users != 0)]

  df2$`EDAM reduced operations` <- df2$`EDAM reduced operations` |> str_squish()
  df2$`EDAM reduced operations` <- ifelse(df2$`EDAM reduced operations` == "", "No Operation", df2$`EDAM reduced operations`)

  st <- df2[, by = `EDAM reduced operations`, .(N = `Suite ID` |> unique() |> length())]
  st <- st[order(-N)]
  st <- st[which(`EDAM reduced operations` != "No Operation")]

  df2$`EDAM reduced operations` <- df2$`EDAM reduced operations` |> factor(levels = c(st$`EDAM reduced operations`, "No Operation"))

  df2 <- df2[order(`Suite ID`, -`EDAM reduced operations`)]

  df2 <- df2[, c("Suite ID", "runs", "users", "EDAM reduced operations", "Included in")] |> unique()
  st <- st[1:15]

  df2$cluster <- ifelse(df2$`EDAM reduced operations` %in% st$`EDAM reduced operations`, df2$`EDAM reduced operations` |> as.character(), "Other")

  df2 <- df2[order(-runs, -users)]

  plot_dt_2 <- df2[, -c("EDAM reduced operations"), with = FALSE] |>
    unique() |>
    melt(
      id.vars = c("Suite ID", "Included in", "cluster"),
      variable.factor = FALSE, value.factor = FALSE
    )

  plot_dt_2[, by = .(cluster, variable), value_median := value |> median()]

  plot_dt_2$variable <- plot_dt_2$variable |>
    str_replace_all("runs", xvar) |>
    str_replace_all("users", yvar)

  hg2 <- plot_dt_2[, by = variable, .(value = value |> median())]

  gr <- plot_dt_2 |>
    ggplot(aes(reorder_within(cluster, -value_median, variable), value)) +
    geom_hline(data = hg2, aes(yintercept = value), color = "white", linewidth = 1.15) +
    geom_hline(data = hg2, aes(yintercept = value), color = "#E15759", linewidth = 1, linetype = "dotted", lineend = "round") +
    geom_point(
      fill = "grey", color = "grey10", shape = 21, size = 1.5, stroke = .1,
      position = position_jitternormal(sd_y = 0, sd_x = .075)
    ) +
    geom_violin(color = NA, alpha = .5, fill = "grey") +
    geom_boxplot(fill = "#4E79A7" |> lighten(.15), width = .25, outliers = FALSE, linewidth = .25) +

    # stat_pointinterval(color = "#4E79A7" |> darken(.25)) +

    scale_x_reordered() +
    scale_y_continuous(
      transform = "log10",
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    facet_wrap2(vars(variable), nrow = 1, scales = "free_x", axes = "all") +
    theme_minimal(base_family = "Calibri") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14),
      strip.text = element_markdown(size = 14),
      axis.title.x = element_markdown(margin = margin(t = 10), size = 14),
      axis.title.y = element_blank(), # element_markdown(margin = margin(r = 10)),

      # axis.ticks = element_line(linewidth = .3),
      panel.grid.major.y = element_line(linewidth = .3, linetype = "dashed", lineend = "round", color = "grey75"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),

      # panel.border = element_rect(linewidth = .3, fill = NA),

      axis.line = element_line(lineend = "round"),
      axis.ticks = element_line(lineend = "round")
    ) +
    labs(x = "**EDAM** operation")

  return(gr)
}
