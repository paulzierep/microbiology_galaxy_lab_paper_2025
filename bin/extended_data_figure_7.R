## Load `libraries`

library(data.table)
library(stringr)

library(ggplot2)
library(ggrepel)
library(ggtext)
library(ggh4x)

library(shadowtext)

library(extrafont)

library(packcircles)


microGalaxy_tutorials_url <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_6.tsv"
microGalaxy_tools_url <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_2.tsv"

microGalaxy_tutorials_dt <- microGalaxy_tutorials_url |> fread()
microGalaxy_tools_dt <- microGalaxy_tools_url |> fread()


## Compute number of tools and number of microGalaxy tools per tutorial


# 1 ------------------------

dt1 <- microGalaxy_tutorials_dt$Tools |>
  str_split("\\,") |>
  lapply(str_squish) |>
  lapply(function(q) data.table("Tool ID" = q)) |>
  rbindlist(idcol = "Topic_id")

dt1$Topic <- microGalaxy_tutorials_dt[dt1$Topic_id]$Topic
dt1$Title <- microGalaxy_tutorials_dt[dt1$Topic_id]$Title

dt1$`EDAM topic` <- microGalaxy_tutorials_dt[dt1$Topic_id]$`EDAM topic`
dt1$`EDAM operation` <- microGalaxy_tutorials_dt[dt1$Topic_id]$`EDAM operation`

tmp <- microGalaxy_tools_dt$`Tool IDs` |>
  str_split("\\,") |>
  lapply(str_squish) |>
  lapply(function(q) data.table("Tool ID" = str_squish(q))) |>
  rbindlist(idcol = "id")

tmp$`Suite ID` <- microGalaxy_tools_dt[tmp$id]$`Suite ID`

index <- match(dt1$`Tool ID`, tmp$`Tool ID`)

dt1$`Suite ID` <- tmp[index]$`Suite ID`


### Exclude unnecessary columns


microGalaxy_tutorials_dt$Topic <- NULL
microGalaxy_tutorials_dt$Link <- NULL
microGalaxy_tutorials_dt$`Servers with precise tool versions` <- NULL
microGalaxy_tutorials_dt$`Servers with tool but different versions` <- NULL


## Tools coverage graph: Heatmap



dt1$`Suite ID` <- ifelse(is.na(dt1$`Suite ID`) | dt1$`Suite ID` == "", "Not Available", dt1$`Suite ID`)

dt1 <- dt1[which(`Suite ID` != "Not Available")]

dt1$value <- 1

mm <- dt1[, c("Title", "Suite ID", "value"), with = FALSE] |>
  unique() |>
  dcast(Title ~ `Suite ID`, value.var = "value", fill = 0) |>
  as.matrix(rownames = "Title")


hc_x <- mm |>
  t() |>
  dist(method = "binary") |>
  hclust("ward.D2")
hc_y <- mm |>
  dist(method = "binary") |>
  hclust("ward.D2")

dt1$Title <- dt1$Title |> factor(hc_y$labels[hc_y$order] |> rev())
dt1$`Suite ID` <- dt1$`Suite ID` |> factor(hc_x$labels[hc_x$order] |> rev())

a_1 <- dt1 |>
  ggplot(aes(`Suite ID`, Title)) +
  geom_tile(color = "grey96") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_minimal(base_family = "Calibri") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(fill = NA, linewidth = .3),
    axis.ticks.x = element_line(linewidth = .3),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", lineend = "round", linewidth = .35),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(y = "Tutorials", x = "Galaxy Tool Suites")


edam_operations <- dt1$`EDAM operation` |>
  str_split("\\,") |>
  unlist() |>
  str_squish() |>
  table() |>
  as.data.table()

edam_topics <- dt1$`EDAM topic` |>
  str_split("\\,") |>
  unlist() |>
  str_squish() |>
  table() |>
  as.data.table()


edam_operations <- edam_operations[order(-N)] |> head(4)
edam_topics <- edam_topics[order(-N)] |> head(4)


dt2 <- dt1 |>
  tidyr::separate_rows("EDAM topic", sep = ",") |>
  setDT()
dt2 <- dt2 |>
  tidyr::separate_rows("EDAM operation", sep = ",") |>
  setDT()

dt2$`EDAM topic` <- dt2$`EDAM topic` |> str_squish() # |> str_wrap(15)
dt2$`EDAM operation` <- dt2$`EDAM operation` |> str_squish() # |> str_wrap(15)

edam_topics$V1 <- edam_topics$V1 # |> str_wrap(15)
edam_operations$V1 <- edam_operations$V1 # |> str_wrap(15)

dt2$`EDAM topic` <- ifelse(dt2$`EDAM topic` %in% edam_topics$V1, dt2$`EDAM topic`, "Other")
dt2$`EDAM operation` <- ifelse(dt2$`EDAM operation` %in% edam_operations$V1, dt2$`EDAM operation`, "Other")

dt2$`EDAM topic` <- dt2$`EDAM topic` |> factor(c(edam_topics$V1, "Other"))
dt2$`EDAM operation` <- dt2$`EDAM operation` |> factor(c(edam_operations$V1, "Other"))

a_2 <- dt2 |>
  ggplot(aes(`Suite ID`, Title)) +
  geom_tile(color = "white") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_grid2(
    rows = vars(`EDAM topic`),
    cols = vars(`EDAM operation`),
    space = "free",
    scales = "free"
  ) +
  theme_minimal(base_family = "Calibri") +
  theme(
    axis.text.x = element_text(size = 4, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.border = element_rect(fill = NA, color = "grey", linewidth = .3),
    # axis.ticks = element_line(linewidth = .3, color = "grey"),

    strip.text.x = element_text(face = "bold"),
    strip.text.y = element_text(face = "bold", angle = 0, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", lineend = "round", linewidth = .15),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(y = "Tutorials", x = "Galaxy Tool Suites")

## Saving graphs

dir.create("../results/tutorials", showWarnings = FALSE)
dir.create("../docs/extended/", showWarnings = FALSE)

# dt --------------------------

# writexl::write_xlsx(dt1, "./results/tutorials/microGalaxy-tools.xlsx")

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


save_plot(a_2, "./docs/extended/extended_data_figure_7", 20, 10)
