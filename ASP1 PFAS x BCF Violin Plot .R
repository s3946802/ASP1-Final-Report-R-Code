# Install packages once if required:
# install.packages(c("readxl", "dplyr", "ggplot2", "showtext"))

library(readxl)
library(dplyr)
library(ggplot2)
library(showtext)

# -------------------------------------------------------------------
# 1. Register Segoe UI
# -------------------------------------------------------------------

font_add(
  family = "Segoe UI",
  regular = "C:/Windows/Fonts/segoeui.ttf",
  bold = "C:/Windows/Fonts/segoeuib.ttf"
)

showtext_auto()

# -------------------------------------------------------------------
# 2. Read the Excel file
# -------------------------------------------------------------------

file_path <- "C:\\Users\\flynn\\Documents\\ASP1 R\\Written Report\\orchid R.xlsx"

pfas_data <- read_excel(
  file_path,
  sheet = 1
)

# -------------------------------------------------------------------
# 3. Check and clean the data
# -------------------------------------------------------------------

required_columns <- c("pfas_ID", "effect_size", "ln_SD")

if (!all(required_columns %in% names(pfas_data))) {
  stop(
    "The Excel file must contain the following columns: ",
    paste(required_columns, collapse = ", ")
  )
}

pfas_data <- pfas_data %>%
  mutate(
    pfas_ID = as.character(pfas_ID),
    effect_size = as.numeric(effect_size)
  ) %>%
  filter(
    !is.na(pfas_ID),
    !is.na(effect_size)
  )

# -------------------------------------------------------------------
# 4. Summary statistics by PFAS species
# -------------------------------------------------------------------

summary_table <- pfas_data %>%
  group_by(pfas_ID) %>%
  summarise(
    n = n(),
    mean_effect_size = mean(effect_size, na.rm = TRUE),
    sd_effect_size = sd(effect_size, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n)) %>%
  mutate(
    mean_effect_size = round(mean_effect_size, 3),
    sd_effect_size = round(sd_effect_size, 3)
  )

print(summary_table)

# Optional: save the table as a CSV file
write.csv(
  summary_table,
  "pfas_summary_statistics.csv",
  row.names = FALSE
)
# -------------------------------------------------------------------
# 4. Select the eight PFAS species with the most observations
# -------------------------------------------------------------------

species_counts <- pfas_data %>%
  count(pfas_ID, name = "k") %>%
  arrange(desc(k), pfas_ID)

top_eight <- species_counts %>%
  slice_head(n = 8) %>%
  pull(pfas_ID)

plot_data <- pfas_data %>%
  filter(pfas_ID %in% top_eight) %>%
  left_join(species_counts, by = "pfas_ID") %>%
  mutate(
    # Reverse order so the species with the most observations appears at the top
    pfas_ID = factor(
      pfas_ID,
      levels = rev(top_eight)
    )
  )

# Data used for the k = count labels
plot_counts <- plot_data %>%
  distinct(pfas_ID, k) %>%
  mutate(
    label = paste0("k = ", k)
  )

# -------------------------------------------------------------------
# 5. Set colours and plot appearance
# -------------------------------------------------------------------

species_colours <- setNames(
  hcl.colors(length(top_eight), palette = "Dark 3"),
  rev(top_eight)
)

plot_background <- "#FFFFFF"

# -------------------------------------------------------------------
# 6. Create the violin plot
# -------------------------------------------------------------------

violin_plot <- ggplot(
  plot_data,
  aes(
    x = effect_size,
    y = pfas_ID,
    fill = pfas_ID
  )
) +
  geom_violin(
    orientation = "y",
    alpha = 1,
    trim = FALSE,
    scale = "width",
    colour = "black"
  ) +
  # Box plot inside each violin
  geom_boxplot(
    width = 0.12,
    fill = "white",
    colour = "black",
    outlier.shape = NA
  ) +
  # Observation counts
  geom_text(
    data = plot_counts,
    aes(
      x = Inf,
      y = pfas_ID,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = -0.15,
    size = 17,
    family = "Segoe UI",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = species_colours
  ) +
  scale_x_continuous(
    breaks = seq(-8, 10, by = 1),
    expand = c(0, 0)
  ) +
  coord_cartesian(
    xlim = c(-8, 10),
    clip = "off"
  ) +
  labs(
    x = "ln(BCF)",
    y = "PFAS Species"
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "Segoe UI"
  ) +
  theme(
    text = element_text(
      family = "Segoe UI"
    ),
    legend.position = "none",
    
    # Allow space for the k = labels
    plot.margin = margin(
      t = 10,
      r = 100,
      b = 20,
      l = 20
    ),
    
    # X-axis title: increase distance below the x-axis
    axis.title.x = element_text(
      size = 55,
      face = "bold",
      margin = margin(t = 15)
    ),
    
    # Y-axis title: increase distance to the left of the y-axis
    axis.title.y = element_text(
      size = 55,
      face = "bold",
      margin = margin(r = 15)
    ),
    
    # Axis values
    axis.text.x = element_text(
      size = 50,
      face = "bold"
    ),
    
    axis.text.y = element_text(
      size = 50,
      face = "bold"
    ),
    
    # Plot background
    panel.background = element_rect(
      fill = plot_background,
      colour = NA
    ),
    
    plot.background = element_rect(
      fill = plot_background,
      colour = NA
    ),
    
    # Black major gridlines and no minor gridlines
    panel.grid.major = element_line(
      colour = "black",
      linewidth = 0.1
    ),
    
    panel.grid.minor = element_blank()
  )
# -------------------------------------------------------------------
# 7. Display the plot
# -------------------------------------------------------------------

print(violin_plot)

# -------------------------------------------------------------------
# 8. Save the plot
# -------------------------------------------------------------------

ggsave(
  filename = "pfas_violin_plot_written_report.png",
  plot = violin_plot,
  width = 13,
  height = 6.3,
  units = "in",
  dpi = 300,
  bg = plot_background
)