# Reproduce the final social-media figure from the repository root:
# Rscript analysis.R

library(foreign)
library(ResIN)
library(ggplot2)
library(patchwork)

data_dir <- "data"
data_path <- file.path(data_dir, "gss2024.sav")
if (!file.exists(data_path)) {
  dir.create(data_dir, showWarnings = FALSE)
  download.file("https://osf.io/download/nkr4t", data_path, mode = "wb")
}

gss <- read.spss(data_path,
                 to.data.frame = TRUE, use.value.labels = FALSE)

levels_5 <- c(
  "Strong concern / discomfort",
  "Some concern / discomfort",
  "Neutral",
  "Some optimism / comfort",
  "Strong optimism / comfort"
)

response_palette <- c(
  "Strong concern / discomfort" = "#C62828",
  "Some concern / discomfort" = "#F59E0B",
  "Neutral" = "#8F98A3",
  "Some optimism / comfort" = "#A7D7A5",
  "Strong optimism / comfort" = "#1B5E20"
)

as_direction <- function(x, reverse = FALSE) {
  if (reverse) x <- 6 - x
  factor(x, levels = 1:5, labels = levels_5, ordered = TRUE)
}

comfort_direction <- function(x) {
  band <- cut(x, breaks = c(-1, 2, 4, 5, 7, 10), labels = FALSE)
  factor(band, levels = 1:5, labels = levels_5, ordered = TRUE)
}

responses <- data.frame(
  Technology_easier = as_direction(gss$TECHESY, reverse = TRUE),
  Technology_harm = as_direction(gss$HARMGOOD1),
  Next_generation = as_direction(gss$NEXTGEN1, reverse = TRUE),
  AI_job_loss = as_direction(gss$AIWORRY),
  AI_surgery = comfort_direction(gss$AIMED),
  Driverless_cars = comfort_direction(gss$AIDRIVE)
)
keep <- complete.cases(responses) & !is.na(gss$WTSSPS)

run_resin <- function(screen = FALSE) {
  ResIN(
    responses[keep, ], weights = gss$WTSSPS[keep],
    left_anchor = "AI_job_loss_Strong concern / discomfort",
    offset = 0,
    remove_nonsignificant = screen,
    remove_nonsignificant_method = "default",
    sign_threshold = .10,
    detect_clusters = TRUE,
    network_stats = TRUE,
    generate_ggplot = FALSE,
    plot_ggplot = FALSE,
    seed = 20260921
  )
}

full <- run_resin(FALSE)
sparse <- run_resin(TRUE)
nodes <- sparse$ResIN_nodeframe
edges <- sparse$ResIN_edgelist
nodes$position <- factor(nodes$choices, levels = levels_5)

level_map <- setNames(as.character(nodes$position), nodes$node_names)
edges$from_position <- level_map[edges$from]
edges$to_position <- level_map[edges$to]
edges$alpha <- scales::rescale(edges$weight, to = c(.10, .72))
edges$linewidth <- scales::rescale(edges$weight, to = c(.18, 2.3))

within_count <- function(level) {
  sum(edges$from_position == level & edges$to_position == level)
}
optimism_links <- within_count("Strong optimism / comfort")
concern_links <- within_count("Strong concern / discomfort")
possible_links <- choose(sum(nodes$position == "Strong optimism / comfort"), 2)

# Neutral-side asymmetry is measured in the full, zero-offset network so it is
# not inflated by the display's p <= .10 sparsification.
full_nodes <- full$ResIN_nodeframe
full_edges <- full$ResIN_edgelist
full_map <- setNames(as.character(full_nodes$choices), full_nodes$node_names)
full_edges$a <- full_map[full_edges$from]
full_edges$b <- full_map[full_edges$to]
neutral_to <- function(level) {
  sum(full_edges$weight[
    (full_edges$a == "Neutral" & full_edges$b == level) |
      (full_edges$b == "Neutral" & full_edges$a == level)
  ])
}
neutral_concern <- neutral_to("Strong concern / discomfort") +
  neutral_to("Some concern / discomfort")
neutral_optimism <- neutral_to("Some optimism / comfort") +
  neutral_to("Strong optimism / comfort")
neutral_ratio <- neutral_concern / neutral_optimism

# Soft convex hulls make the two extreme response packages immediately visible.
make_hull <- function(df, scale = 1.17) {
  h <- df[chull(df$x, df$y), c("x", "y")]
  cx <- mean(h$x)
  cy <- mean(h$y)
  h$x <- cx + (h$x - cx) * scale
  h$y <- cy + (h$y - cy) * scale
  h
}
concern_hull <- make_hull(nodes[nodes$position == "Strong concern / discomfort", ])
optimism_hull <- make_hull(nodes[nodes$position == "Strong optimism / comfort", ])

network_plot <- ggplot() +
  geom_polygon(data = concern_hull, aes(x = x, y = y),
               fill = response_palette[["Strong concern / discomfort"]], alpha = .08) +
  geom_polygon(data = optimism_hull, aes(x = x, y = y),
               fill = response_palette[["Strong optimism / comfort"]], alpha = .10) +
  geom_segment(data = edges,
               aes(x = from.x, y = from.y, xend = to.x, yend = to.y,
                   alpha = alpha, linewidth = linewidth),
               colour = "#64748B", lineend = "round") +
  geom_point(data = nodes, aes(x = x, y = y, fill = position),
             shape = 21, size = 4.7, colour = "#17212B", stroke = .75) +
  scale_fill_manual(values = response_palette, name = NULL) +
  scale_alpha_identity(guide = "none") +
  scale_linewidth_identity(guide = "none") +
  coord_equal(clip = "off") +
  labs(caption = "Nodes are answer options; thicker links show stronger positive associations.") +
  theme_void(base_family = "sans") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8.4, colour = "#334155"),
    legend.key.width = grid::unit(7, "pt"),
    plot.caption = element_text(size = 8.5, colour = "#64748B", hjust = .5,
                                margin = margin(t = 10)),
    plot.margin = margin(8, 10, 8, 8),
    plot.background = element_rect(fill = "#F8FAFC", colour = NA),
    panel.background = element_rect(fill = "#F8FAFC", colour = NA)
  )

stats_plot <- ggplot() +
  annotate("text", x = 0, y = 9.3, label = "OPTIMISM IS THE\nMORE COHERENT PACKAGE",
           hjust = 0, vjust = 1, size = 5.6, fontface = "bold",
           colour = "#102A1C", lineheight = .95) +
  annotate("text", x = 0, y = 7.35,
           label = paste0(optimism_links, " / ", possible_links),
           hjust = 0, size = 10.5, fontface = "bold", colour = response_palette[[5]]) +
  annotate("text", x = 2.25, y = 7.35,
           label = "possible links remain\namong strong optimism answers",
           hjust = 0, vjust = .5, size = 3.6, colour = "#334155", lineheight = 1.05) +
  annotate("segment", x = 0, xend = 5.8, y = 6.35, yend = 6.35,
           colour = "#D7DEE7", linewidth = .6) +
  annotate("text", x = 0, y = 5.45,
           label = paste0(concern_links, " / ", possible_links),
           hjust = 0, size = 8.5, fontface = "bold", colour = response_palette[[1]]) +
  annotate("text", x = 2.25, y = 5.45,
           label = "possible links remain\namong strong concern answers",
           hjust = 0, vjust = .5, size = 3.6, colour = "#334155", lineheight = 1.05) +
  annotate("segment", x = 0, xend = 5.8, y = 4.45, yend = 4.45,
           colour = "#D7DEE7", linewidth = .6) +
  annotate("text", x = 0, y = 3.35,
           label = paste0(format(round(neutral_ratio, 1), nsmall = 1), "×"),
           hjust = 0, size = 9.5, fontface = "bold", colour = "#475569") +
  annotate("text", x = 2.25, y = 3.35,
           label = "Neutral answers connect more\nstrongly to concern than optimism",
           hjust = 0, vjust = .5, size = 3.6, colour = "#334155", lineheight = 1.05) +
  annotate("label", x = 0, y = 1.35,
           label = "The enthusiastic side behaves like a shared worldview.\nThe middle looks cautious rather than enthusiastic.",
           hjust = 0, vjust = .5, size = 3.7, fontface = "bold",
           colour = "#102A1C", fill = "#EAF4EC", linewidth = 0,
           label.padding = grid::unit(.42, "lines"), lineheight = 1.1) +
  coord_cartesian(xlim = c(-.15, 6.2), ylim = c(.2, 9.7), clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#F8FAFC", colour = NA),
        panel.background = element_rect(fill = "#F8FAFC", colour = NA),
        plot.margin = margin(10, 24, 10, 16))

social_figure <- network_plot + stats_plot +
  plot_layout(widths = c(1.42, 1)) +
  plot_annotation(
    title = "AI optimism forms a tighter package than AI concern",
    subtitle = paste0("Response Item Network • U.S. GSS 2024 Digital Societies module • weighted N = ", sum(keep)),
    caption = "Sparse network uses ResIN's p ≤ .10 screen; the neutral-side ratio is calculated from the full zero-offset network. Descriptive associations, not causal effects.",
    theme = theme(
      plot.title = element_text(size = 24, face = "bold", colour = "#0F172A", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, colour = "#53657A", margin = margin(b = 10)),
      plot.caption = element_text(size = 8.2, colour = "#64748B", hjust = 0, margin = margin(t = 8)),
      plot.background = element_rect(fill = "#F8FAFC", colour = NA),
      plot.margin = margin(22, 30, 18, 30)
    )
  )

out_path <- "resin_social_takeaway.png"
ggsave(out_path, social_figure, width = 15.5, height = 8.7, dpi = 240, bg = "#F8FAFC")
message("Saved ", out_path)
message("Optimism links: ", optimism_links, "/", possible_links,
        "; concern links: ", concern_links, "/", possible_links,
        "; neutral ratio: ", round(neutral_ratio, 2))
