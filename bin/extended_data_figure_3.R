# Load `libraries`

library(data.table)
library(stringr)

library(ggplot2)
library(ggrepel)
library(ggtext)
library(ggh4x)

library(shadowtext)
library(extrafont)
library(packcircles)


workflows_url <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_5.tsv"
all_tools_url <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/docs/supplementary/supplementary_table_2.tsv"

workflows_dt <- workflows_url |> fread(sep = "\t")
all_tools_dt <- all_tools_url |> fread()


## Extract Tool Suite IDs


workflows_dt <- workflows_dt |> tidyr::separate_rows("Tools", sep = ",") |> setDT()
all_tools_dt <- all_tools_dt |> tidyr::separate_rows("Tool IDs", sep = ",") |> setDT()

workflows_dt$Tools <- workflows_dt$Tools |> str_squish()
all_tools_dt$`Tool IDs` <- all_tools_dt$`Tool IDs` |> str_squish()

index <- match(workflows_dt$Tools, all_tools_dt$`Tool IDs`)

workflows_dt$`Suite ID` <- all_tools_dt[index]$`Suite ID`


dt1 <- workflows_dt[which(!is.na(`Suite ID`)), c("Name", "Suite ID", "Source", "Projects", "EDAM operations", "EDAM topics"), with = FALSE] |> unique()

dt1$Source   <- ifelse(
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

dt1 <- dt1 |> tidyr::separate_rows("EDAM operations", sep = "\\,") |> setDT()

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

hc_y <- mm |> dist(method = "binary") |> hclust(method = "ward.D2")
hc_x <- mm |> t() |> dist(method = "binary") |> hclust(method = "ward.D2")


dt1$Name <- dt1$Name |> factor(levels = hc_y$labels[hc_y$order] |> rev())
dt1$`Suite ID` <- dt1$`Suite ID` |> factor(levels = hc_x$labels[hc_x$order])


dt2 <- dt1[, by = Name, .("No. of suites" = `Suite ID` |> unique() |> length())]

dt2$`Perc. of suites` <- dt2$`No. of suites` / (dt1$`Suite ID` |> unique() |> length())

dt2 <- dt2[order(-`No. of suites`)]

dt3 <- workflows_dt[which(!is.na(`Suite ID`)), c("Name", "Suite ID", "Source", "Projects", "EDAM operations", "EDAM topics"), with = FALSE] |> unique()

dt3$Source   <- ifelse(
    dt3$Source == "WorkflowHub", "WorkflowHub", 
    ifelse(
        dt3$Source == "dev.WorkflowHub", "GTN", 
        "Public Servers"
    )
)

dt3$Projects <- ifelse(
    dt3$Projects == "Intergalactic Workflow Commission (IWC)", "IWC",
    ifelse(
        dt3$Projects == "Galaxy Training Network", "GTN", 
        "Other Projects"
    )
) |> str_wrap(22)

dt3$cluster <- paste0(dt3$Source, "; ", dt3$Projects)

dt3$cluster <- ifelse(
    dt3$cluster == "WorkflowHub; IWC", "IWC on WorkflowHub",
    ifelse(
        dt3$cluster == "WorkflowHub; Other Projects", "Other WorkflowHub workflows", 
        ifelse(
            dt3$cluster == "Public Servers; Other Projects", "Public server workflows", 
            "Workflows related to GTN tutorials"
        )
    )
) |> str_wrap(15)


dt1 <- dt1 |> tidyr::separate_rows("EDAM operations", sep = "\\,") |> setDT()

dt1$`EDAM operations` <- dt1$`EDAM operations` |> str_squish()
dt1$`EDAM operations` <- dt1$`EDAM operations` |> str_wrap(50) # |> str_replace("DNA barcoding", "DNA\nbarcoding")

edam_operations <- dt1[, by = `EDAM operations`, .(N = Name |> unique() |> length())]
edam_operations <- edam_operations[which(`EDAM operations` != "Other")]
edam_operations <- edam_operations[order(-N)] |> head(14)

dt1$`EDAM operations` <- ifelse(dt1$`EDAM operations` %in% edam_operations$`EDAM operations`, dt1$`EDAM operations`, "Other")
dt1$`EDAM operations` <- dt1$`EDAM operations` |> factor(c(edam_operations$`EDAM operations`, "Other")) 

dt1$value <- 1

mm <- dt1[, c("Name", "Suite ID", "value"), with = FALSE] |> 
    unique() |> 
    dcast(Name ~ `Suite ID`, value.var = "value", fill = 0) |> 
    as.matrix(rownames = "Name")

hc_y <- mm |> dist(method = "binary") |> hclust(method = "ward.D2")
hc_x <- mm |> t() |> dist(method = "binary") |> hclust(method = "ward.D2")


dt1$Name <- dt1$Name |> factor(levels = hc_y$labels[hc_y$order] |> rev())
dt1$`Suite ID` <- dt1$`Suite ID` |> factor(levels = hc_x$labels[hc_x$order])



dt3 <- dt1[, by = .(Name, cluster, `EDAM operations`), .("No. of suites" = `Suite ID` |> unique() |> length())]

mm <- dt3[, c("Name", "EDAM operations", "No. of suites"), with = FALSE] |> 
    unique() |>
    dcast(Name ~ `EDAM operations`, value.var = "No. of suites", fill = 0) |> 
    as.matrix(rownames = "Name")

hc_y <- mm |> dist(method = "euclidean") |> hclust(method = "ward.D2")
hc_x <- mm |> t() |> dist(method = "euclidean") |> hclust(method = "ward.D2")

dt3$Name <- dt3$Name |> factor(levels = hc_y$labels[hc_y$order] |> rev())
dt3$`EDAM operations 2` <-  dt3$`EDAM operations` |> as.character() |> 
    factor(levels = c(hc_x$labels[hc_x$order] |> rev() |> str_subset("Other", negate = TRUE), "Other"))

gr3 <- dt3 |>
    ggplot(aes(`EDAM operations 2`, Name)) +
    geom_tile(aes(fill = `No. of suites`), color = "grey", linewidth = .25) +
    
    facet_grid(rows = vars(cluster), scales = "free_y", space = "free_y") +
    
    # facet_wrap2(vars(cluster), nrow = 1, scales = "free_x") +
    
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    
    scale_fill_stepsn(colors = c('#00429d', '#73a2c6', '#ffffe0', '#f4777f', '#93003a'),
                      breaks = c(2, 4, 6, 8),
                      guide = guide_colorsteps(barheight = unit(12, "lines"),
                                               barwidth = unit(.5, "lines"))) +
    
    coord_cartesian(clip = "off", expand = TRUE) +
    
    theme_minimal(base_family = "Calibri") +
    theme(
        legend.title.position = "left",
        legend.title = element_text(angle = 90, hjust = 0, size = 11),
        legend.text = element_text(size = 10),
        
        # strip.text.x.top = element_text(angle = 45),
        strip.text.y.right = element_text(angle = 0, hjust = 0, size = 11, face = "bold"),
        strip.clip = "off",
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        panel.border = element_rect(fill = NA, color = "grey", linewidth = .25),
        
        axis.ticks = element_line(color = "grey", linewidth = .25),
        
        axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8),
        
        axis.title = element_text(size = 11, face = "bold")
    ) +
    
    labs(x = "EDAM operations", y = "Workflows", fill = "No. of Tool Suites")




## Save plots


outfolder <- "../results/workflows/"

dir.create(outfolder, showWarnings = FALSE)
dir.create("../docs/extended/", showWarnings = FALSE)

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

save_plot(gr3, "../docs/extended/extended_data_figure_3", 9, 12)

# fwrite(dt2, paste0(outfolder, "/workflows-stats.tsv"), row.names = FALSE, quote = FALSE, sep = "\t")




