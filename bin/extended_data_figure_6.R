message("Extended Figure 6 ...\t", appendLF = FALSE)

suppressWarnings({
  # Load `libraries`

  suppressMessages({
    library(data.table, quietly = TRUE)
    library(stringr, quietly = TRUE)

    library(ggplot2, quietly = TRUE)
    library(ggrepel, quietly = TRUE)
    library(ggtext, quietly = TRUE)
    library(ggh4x, quietly = TRUE)

    library(shadowtext, quietly = TRUE)
    library(extrafont, quietly = TRUE)
  })


  workflows_url <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_5.tsv"
  all_tools_url <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_2.tsv"

  workflows_dt <- workflows_url |> fread(sep = "\t")
  all_tools_dt <- all_tools_url |> fread()


  ## Extract Tool Suite IDs


  workflows_dt <- workflows_dt |>
    tidyr::separate_rows("Tools", sep = ",") |>
    setDT()
  all_tools_dt <- all_tools_dt |>
    tidyr::separate_rows("Tool IDs", sep = ",") |>
    setDT()

  workflows_dt$Tools <- workflows_dt$Tools |> str_squish()
  all_tools_dt$`Tool IDs` <- all_tools_dt$`Tool IDs` |> str_squish()

  index <- match(workflows_dt$Tools, all_tools_dt$`Tool IDs`)

  workflows_dt$`Suite ID` <- all_tools_dt[index]$`Suite ID`


  dt1 <- workflows_dt[which(!is.na(`Suite ID`)), c("Name", "Suite ID", "Source", "Projects", "EDAM operations", "EDAM topics"), with = FALSE] |> unique()

  dt1$Source <- ifelse(
    dt1$Source == "WorkflowHub", "WorkflowHub",
    ifelse(
      dt1$Source == "dev.WorkflowHub", "GTN",
      "Public Servers"
    )
  )

  dt1$Projects <- ifelse(
    dt1$Projects == "Intergalactic Workflow Commission (IWC)", "IWC",
    ifelse(dt1$Projects == "Galaxy Training Network", "GTN", "Other Projects")
  ) |> str_wrap(22)

  dt1$cluster <- paste0(dt1$Source, "; ", dt1$Projects)

  dt1$cluster <- ifelse(
    dt1$cluster == "WorkflowHub; IWC", "IWC on WorkflowHub",
    ifelse(
      dt1$cluster == "WorkflowHub; Other Projects", "Other WorkflowHub workflows",
      ifelse(
        dt1$cluster == "Public Servers; Other Projects", "Public server workflows",
        "Workflows related to GTN tutorials"
      )
    )
  ) |> str_wrap(15)

  dt1 <- dt1 |>
    tidyr::separate_rows("EDAM operations", sep = "\\,") |>
    setDT()

  dt1$`EDAM operations` <- dt1$`EDAM operations` |> str_squish()
  dt1$`EDAM operations` <- dt1$`EDAM operations` |> str_wrap(50) # |> str_replace("DNA barcoding", "DNA\nbarcoding")

  edam_operations <- dt1[, by = `EDAM operations`, .(N = Name |> unique() |> length())]
  edam_operations <- edam_operations[order(-N)] |> head(4)

  dt1$`EDAM operations` <- ifelse(dt1$`EDAM operations` %in% edam_operations$`EDAM operations`, dt1$`EDAM operations`, "Other")
  dt1$`EDAM operations` <- dt1$`EDAM operations` |> factor(c(edam_operations$`EDAM operations`, "Other"))

  dt1$value <- 1

  mm <- dt1[, c("Name", "Suite ID", "value"), with = FALSE] |>
    unique() |>
    dcast(Name ~ `Suite ID`, value.var = "value", fill = 0) |>
    as.matrix(rownames = "Name")

  hc_y <- mm |>
    dist(method = "binary") |>
    hclust(method = "ward.D2")
  hc_x <- mm |>
    t() |>
    dist(method = "binary") |>
    hclust(method = "ward.D2")


  dt1$Name <- dt1$Name |> factor(levels = hc_y$labels[hc_y$order] |> rev())
  dt1$`Suite ID` <- dt1$`Suite ID` |> factor(levels = hc_x$labels[hc_x$order])


  gr2 <- dt1 |>
    ggplot(aes(`Suite ID`, Name)) +
    geom_tile(color = "white") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    facet_grid2(
      rows = vars(cluster),
      cols = vars(`EDAM operations`),
      space = "free",
      scales = "free"
    ) +
    coord_cartesian(clip = "off", expand = TRUE) +
    theme_minimal(base_family = "Calibri") +
    theme(
      axis.text.x = element_text(size = 2.5, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 6),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      panel.border = element_rect(fill = NA, color = "grey", linewidth = .3),
      strip.text.x = element_text(face = "bold"),
      strip.text.y = element_text(face = "bold", angle = 0, hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linetype = "dashed", lineend = "round", linewidth = .15),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(y = "Workflows", x = "Galaxy Tool Suites")

  ## Save plots


  outfolder <- "../results/workflows/"

  # dir.create(outfolder, showWarnings = FALSE)
  # dir.create("../docs/extended/", showWarnings = FALSE)

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

  save_plot(gr2, "./docs/extended/extended_data_figure_6", 16, 8)

  # fwrite(dt2, paste0(outfolder, "/workflows-stats.tsv"), row.names = FALSE, quote = FALSE, sep = "\t")
})

message("done!")
