# Load data analytics libraries -------------------

library(data.table)
library(stringr)

# load plotting libraries --------------------

library(ggdensity)
library(ggdist)
library(ggforce)

# helper function ------------------

plot_figure_3B <- function() {
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
  hg0 <- df2[1:10, c("Suite ID", "runs", "users", "Included in"), with = FALSE] |> unique()

  hg1 <- tools_dt[which(str_detect(`EDAM reduced topics`, "Microbial ecology|Phylogenetics"))]
  hg1 <- df2[which(`Suite ID` %in% hg1$`Suite ID`)]
  hg1 <- hg1[which(!(`Suite ID` %in% hg0$`Suite ID`)), c("Suite ID", "runs", "users", "Included in"), with = FALSE] |>
    unique() |>
    head()

  ncolors <- df2$cluster |>
    unique() |>
    length()

  xvar <- xvar |> str_replace("Suite runs", "**Suite runs**")
  yvar <- yvar |> str_replace("Suite users", "**Suite users**")

  plot_dt <- df2[, c("Suite ID", "runs", "users", "Included in"), with = FALSE] |> unique()

  gr <- plot_dt |>
    ggplot(aes(runs, users)) +
    geom_point(aes(fill = `Included in`, color = `Included in`), shape = 21, stroke = .15, size = 2) +
    geom_hdr_lines(
      data = plot_dt[which(`Included in` %in% c("Both", "Tutorials", "Workflows"))],
      aes(runs, users, color = `Included in`),
      probs = c(.75, 0.5), linewidth = .75
    ) +
    geom_text_repel(
      data = hg0, aes(label = `Suite ID`),
      bg.r = .075, bg.color = "grey96", fontface = "bold", family = "Calibri", box.padding = .5,
      segment.size = .3, max.overlaps = Inf, size = 3.5
    ) +
    geom_text_repel(
      data = hg1, aes(label = `Suite ID`),
      bg.r = .05, bg.color = "grey96", fontface = "bold", family = "Calibri", box.padding = .5,
      segment.size = .3, max.overlaps = Inf, size = 3.5
    ) +
    scale_color_manual(values = c("Both" = "#8e2bf2", "Tutorials" = "#F28E2B", "Workflows" = "#E15759", "Ν/Α" = "grey"), guide = "none") +
    scale_fill_manual(values = c("Both" = "#8e2bf2", "Tutorials" = "#F28E2B", "Workflows" = "#E15759", "Ν/Α" = "grey") |> lighten(.25)) +
    scale_x_continuous(
      trans = "log10", # expand = c(0, 0),
      limits = c(1, 10000000),
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    scale_y_continuous(
      trans = "log10", # limits = c(1, 10000),
      labels = scales::comma, # expand = c(0, 0),
      breaks = c(.1, 1, 10, 100, 1000, 10000, 100000)
    ) +
    theme_minimal(base_family = "Calibri") +
    theme(
      legend.position = "inside",
      legend.position.inside = c(.25, .7),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      strip.text = element_markdown(),
      axis.title.x = element_markdown(margin = margin(t = 10), size = 14),
      axis.title.y = element_markdown(margin = margin(r = 10), size = 14),
      axis.text = element_text(size = 14),

      # axis.ticks = element_line(linewidth = .3),
      panel.grid.major = element_line(linewidth = .3, linetype = "dashed", lineend = "round", color = "grey75"),

      # panel.border = element_rect(linewidth = .3, fill = NA),

      axis.line = element_line(lineend = "round"),
      axis.ticks = element_line(lineend = "round")
    ) +
    labs(x = xvar, y = yvar, alpha = "Probabilities")

  return(gr)
}
