

## Load `libraries` ----------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggrepel)
library(ggtext)
library(ggh4x)

library(extrafont)
library(paletteer)
library(colorspace)

library(patchwork)

## Load input dataset --------------------------------

tools_url     <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_2.tsv"
workflows_url <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_5.tsv"
tutorials_url <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_6.tsv"

tools_dt     <- tools_url |> fread()
workflows_dt <- workflows_url |> fread()
tutorials_dt <- tutorials_url |> fread()


## Tool Availability graph: Heatmap 

### Extract availability information


index <- tools_dt |> colnames() |> str_subset("Number of tools on")

availability <- tools_dt[, c("Suite ID", "EDAM reduced topics", index), with = FALSE] |> unique()

colnames(availability) <- availability |> 
  colnames() |> 
  str_remove_all("Number\\ of\\ tools\\ on") |> 
  str_squish()


### Filter out duplicates

index <- which(availability$`Suite ID` %in% c("msstatstmt", "srst2") & availability$`EDAM reduced topics` == "")

availability <- availability[-index]

### Hierarchical clustering

mm <- availability[, -c(1, 2)] |> as.matrix(rownames = availability$`Suite ID`)

mm_c <- mm |> dist(method = "manhattan") |> hclust(method = "ward.D2")
mm_r <- t(mm) |> dist(method = "manhattan") |> hclust(method = "ward.D2")

d <- availability |> melt(id.vars = c("Suite ID", "EDAM reduced topics"), variable.factor = FALSE, value.factor = FALSE)
d <- d[which(value >= 1)]

d$`Suite ID` <- d$`Suite ID` |> factor(levels = mm_c$labels[mm_c$order |> rev()])
d$variable   <- d$variable |> factor(levels = mm_r$labels[mm_r$order |> rev()])

d$fct <- ifelse(d$variable |> str_detect("UseGalaxy"), "UseGalaxy", "vOther")

index <- d[which(fct == "UseGalaxy")][[1]] |> unique()

p  <- d[which(`Suite ID` %in% index)]
p2 <- p |> tidyr::separate_rows("EDAM reduced topics", sep = ",") |> setDT()

p2$`EDAM reduced topics` <- p2$`EDAM reduced topics` |> str_squish()
p2$`EDAM reduced topics` <- p2$`EDAM reduced topics` |> str_wrap(width = 10)

t <- p2[, by = "EDAM reduced topics", .(N = `Suite ID` |> unique() |> length())]
t <- t[order(-N)]
t <- t[which(`EDAM reduced topics` != ""), head(.SD, 5)]

p2$edam_clean <- ifelse(p2$`EDAM reduced topics` %in% t$`EDAM reduced topics`, p2$`EDAM reduced topics`, "Other")
p2$edam_clean <- p2$edam_clean |> factor(levels = c(t$`EDAM reduced topics`, "Other"))

plot_heatmap <- function(edam_topics) {
    
  p2[which(edam_clean %in% edam_topics)] |> 
    ggplot(aes(variable, `Suite ID`)) + 

    geom_tile(aes(fill = value), color = "grey") + 
    
    scale_fill_stepsn(
      colors = c('#00429d', '#5681b9', '#93c4d2', '#ffffe0', '#ffa59e', '#dd4c65', '#93003a'),
      guide = guide_colorsteps(barwidth = unit(.35, "lines"), barheight = unit(14, "lines")),
      breaks = c(2, 4, 8, 16, 32, 64, 128),
      limits = c(1, 129),
      transform = "log2"
    ) +
    
    facet_grid(cols = vars(fct), rows = vars(edam_clean), scales = "free", space = "free") +
    
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
      legend.position = "bottom",
      legend.title.position = "top",
      
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8),
      
      panel.border = element_rect(fill = NA, color = "grey25"),
      axis.ticks.x = element_line(lineend = "round", color = "grey25"),
      
      strip.text.x = element_blank(),
      strip.text.y = element_text(face = "bold", angle = 0, hjust = 0, size = 12),
      
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linetype = "dashed", lineend = "round", linewidth = .25)
    ) +
    
    labs(y = "Galaxy Tool Suites", x = "Availability of Tool Suites Across **Servers**", fill = "No. of Tools")
}

c_1_a <- plot_heatmap(c("Metagenomics", "Sequence\nassembly")) + theme(axis.title.x = element_markdown())
c_1_b <- plot_heatmap(c("Sequence\nanalysis", "Genomics", "Sequencing")) + theme(axis.title.x = element_markdown(), axis.title.y = element_blank())


### Save plots

multi <- (c_1_a | c_1_b) +
  plot_layout(widths = c(1, 1.1), guides = "collect") &
  theme(
    legend.direction = "vertical"
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

save_plot(multi, "../docs/extended/extended_data_figure_2", 12, 12)
