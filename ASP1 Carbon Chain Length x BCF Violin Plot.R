# Install packages once if required
# install.packages(c(
#   "readxl",
#   "dplyr",
#   "ggplot2",
#   "writexl",
#   "showtext"
# ))

library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(showtext)


# -------------------------------------------------------------------
# 1. Register Times New Roman
# -------------------------------------------------------------------

font_add(
  family = "Times New Roman",
  regular = "C:/Windows/Fonts/times.ttf",
  bold = "C:/Windows/Fonts/timesbd.ttf",
  italic = "C:/Windows/Fonts/timesi.ttf",
  bolditalic = "C:/Windows/Fonts/timesbi.ttf"
)

showtext_auto()
# -------------------------------------------------------------------
# 2. File locations
# -------------------------------------------------------------------

input_file <- "C:\\Users\\flynn\\Documents\\ASP1 R\\Written Report\\Written Report R\\Chain length R.xlsx"

plot_file <- "BCF_by_carbon_chain_violin_plot.png"
summary_file <- "PFAS_BCF_summary.xlsx"

# -------------------------------------------------------------------
# 3. Read the Excel sheets
# -------------------------------------------------------------------

bcf_data <- read_excel(
  input_file,
  sheet = "BCF_data"
)

chain_mapping <- read_excel(
  input_file,
  sheet = "Chain_mapping"
)

# Check the column names
print(names(bcf_data))
print(names(chain_mapping))

# -------------------------------------------------------------------
# 4. Check that the required columns are present
# -------------------------------------------------------------------

required_bcf_columns <- c(
  "pfas_id",
  "ratio_value",
  "ratio_SD"
)

required_mapping_columns <- c(
  "pfas_id",
  "chain_length"
)

if (!all(required_bcf_columns %in% names(bcf_data))) {
  stop(
    "The BCF_data sheet must contain: ",
    paste(required_bcf_columns, collapse = ", ")
  )
}

if (!all(required_mapping_columns %in% names(chain_mapping))) {
  stop(
    "The Chain_mapping sheet must contain: ",
    paste(required_mapping_columns, collapse = ", ")
  )
}

# -------------------------------------------------------------------
# 5. Convert variables to the correct formats
# -------------------------------------------------------------------

bcf_data <- bcf_data %>%
  mutate(
    pfas_id = as.character(pfas_id),
    ratio_value = as.numeric(ratio_value),
    ratio_SD = as.numeric(ratio_SD)
  )

chain_mapping <- chain_mapping %>%
  mutate(
    pfas_id = as.character(pfas_id),
    chain_length = as.numeric(chain_length)
  )

# -------------------------------------------------------------------
# 6. Define carbon-chain-length levels
# -------------------------------------------------------------------
#
# Chain length 14 is excluded from the analysis and plot.
# -------------------------------------------------------------------

chain_levels <- sort(
  unique(
    chain_mapping$chain_length[
      !is.na(chain_mapping$chain_length) &
        chain_mapping$chain_length != 14
    ]
  )
)

# -------------------------------------------------------------------
# 7. Calculate mean log-BCF and mean log-SD for each PFAS
# -------------------------------------------------------------------
#
# ratio_value and ratio_SD are already natural-log transformed.
# No additional log transformation is applied.
# -------------------------------------------------------------------

pfas_summary <- bcf_data %>%
  group_by(pfas_id) %>%
  summarise(
    mean_ln_BCF = mean(ratio_value, na.rm = TRUE),
    mean_ln_SD = mean(ratio_SD, na.rm = TRUE),
    n_observations = sum(!is.na(ratio_value)),
    .groups = "drop"
  )

# -------------------------------------------------------------------
# 8. Add carbon-chain length to the PFAS summary
# -------------------------------------------------------------------

pfas_summary <- pfas_summary %>%
  left_join(
    chain_mapping,
    by = "pfas_id"
  )

# Identify PFAS species without a chain-length match
unmatched_pfas <- pfas_summary %>%
  filter(is.na(chain_length))

if (nrow(unmatched_pfas) > 0) {
  warning(
    "The following PFAS species have no matching carbon-chain length: ",
    paste(unmatched_pfas$pfas_id, collapse = ", ")
  )
}

# -------------------------------------------------------------------
# 9. Prepare PFAS-level summary data
# -------------------------------------------------------------------

plot_data_pfas <- pfas_summary %>%
  filter(
    !is.na(chain_length),
    chain_length != 14,
    !is.na(mean_ln_BCF)
  ) %>%
  mutate(
    chain_length = factor(
      chain_length,
      levels = chain_levels
    )
  )

if (nrow(plot_data_pfas) == 0) {
  stop("There are no valid PFAS-level data available.")
}

# -------------------------------------------------------------------
# 10. Prepare original observation-level plotting data
# -------------------------------------------------------------------
#
# The violin and boxplots use the original observations rather than
# one averaged value per PFAS species.
# -------------------------------------------------------------------

plot_data_observations <- bcf_data %>%
  left_join(
    chain_mapping,
    by = "pfas_id"
  ) %>%
  filter(
    !is.na(chain_length),
    chain_length != 14,
    !is.na(ratio_value)
  ) %>%
  mutate(
    chain_length = factor(
      chain_length,
      levels = chain_levels
    )
  )

if (nrow(plot_data_observations) == 0) {
  stop("There are no valid observation-level data available.")
}

# -------------------------------------------------------------------
# 11. Count observations by carbon-chain length
# -------------------------------------------------------------------

observation_counts <- plot_data_observations %>%
  count(
    chain_length,
    name = "n_observations"
  )

# -------------------------------------------------------------------
# 12. Create the chain-length summary table
# -------------------------------------------------------------------

# BCF summary by carbon-chain length
bcf_chain_summary <- plot_data_pfas %>%
  group_by(chain_length) %>%
  summarise(
    mean_chain_ln_BCF = mean(
      mean_ln_BCF,
      na.rm = TRUE
    ),
    SD_ln_BCF = sd(
      mean_ln_BCF,
      na.rm = TRUE
    ),
    n_PFAS = n(),
    .groups = "drop"
  )

# ln(SD) summary by carbon-chain length
#
# ratio_SD is already natural-log transformed.
# Therefore, no additional log transformation is applied.
ln_sd_chain_summary <- plot_data_observations %>%
  filter(!is.na(ratio_SD)) %>%
  group_by(chain_length) %>%
  summarise(
    mean_chain_ln_SD = mean(
      ratio_SD,
      na.rm = TRUE
    ),
    SD_ln_SD = ifelse(
      n() > 1,
      sd(ratio_SD, na.rm = TRUE),
      NA_real_
    ),
    n_ln_SD_observations = n(),
    .groups = "drop"
  )

# Combine the BCF and ln(SD) summaries
chain_summary <- bcf_chain_summary %>%
  left_join(
    ln_sd_chain_summary,
    by = "chain_length"
  ) %>%
  left_join(
    observation_counts,
    by = "chain_length"
  ) %>%
  mutate(
    count_label = paste0("k = ", n_observations)
  )
# -------------------------------------------------------------------
# 13. Set plot appearance
# -------------------------------------------------------------------

plot_background <- "#FFFFFF"
# -------------------------------------------------------------------
# 13. Manually assign a colour to each carbon-chain length
# -------------------------------------------------------------------

blue_colours <- c(
  "4"  = "#E3F2FD",  # lightest; bottom
  "5"  = "#D0E8F7",
  "6"  = "#B9DCF2",
  "7"  = "#A2D0ED",
  "8"  = "#8BC4E8",
  "9"  = "#74B8E3",
  "10" = "#5DABDE",
  "11" = "#469FD9",
  "12" = "#2F93D4"   # darkest; top
)

# Check that every plotted chain length has a colour
if (!all(as.character(chain_levels) %in% names(blue_colours))) {
  stop(
    "A colour has not been assigned to every carbon-chain length. ",
    "Chain lengths requiring colours: ",
    paste(
      setdiff(as.character(chain_levels), names(blue_colours)),
      collapse = ", "
    )
  )
}
# -------------------------------------------------------------------
# 14. Create the violin plot
# -------------------------------------------------------------------
#
# The k labels are placed in the same plot using x = Inf. This keeps
# their vertical positions aligned automatically with the y-axis
# categories.
# -------------------------------------------------------------------
bcf_plot <- ggplot(
  plot_data_observations,
  aes(
    x = ratio_value,
    y = chain_length,
    fill = chain_length
  )
) +
  geom_violin(
    orientation = "y",
    alpha = 1,
    trim = FALSE,
    scale = "width",
    colour = "black"
  ) +
  geom_boxplot(
    width = 0.18,
    orientation = "y",
    fill = "white",
    colour = "black",
    alpha = 0.85,
    outlier.shape = NA
  ) +
  geom_text(
    data = chain_summary,
    aes(
      x = Inf,
      y = chain_length,
      label = count_label
    ),
    inherit.aes = FALSE,
    hjust = -0.15,
    size = 15,
    family = "Times New Roman",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = blue_colours
  ) +
  scale_x_continuous(
    limits = c(-9, 12),
    breaks = seq(-9, 12, by = 1),
    labels = seq(-9, 12, by = 1),
    expand = c(0, 0)
  ) +
  scale_y_discrete(
    limits = chain_levels,
    drop = FALSE,
    expand = expansion(
      add = c(4, -2)
    )
  ) +
  coord_cartesian(
    xlim = c(-9, 12),
    clip = "off"
  ) +
  labs(
    x = "ln(BCF)",
    y = "Carbon-chain length"
  ) +
  theme_minimal(
    base_size = 13,
    base_family = "Times New Roman"
  ) +
  theme(
    text = element_text(
      family = "Times New Roman"
    ),
    
    # Remove the legend
    legend.position = "none",
    
    # Bold axis titles with larger font size
    axis.title.x = element_text(
      size = 60,
      face = "bold",
      margin = margin(t = 15)
    ),
    axis.title.y = element_text(
      size = 60,
      face = "bold",
      margin = margin(r = 15)
    ),
    
    # Bold axis values
    axis.text.x = element_text(
      face = "bold",
      size = 50
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 50
    ),
    
    # Gridlines
    panel.grid.major = element_line(
      colour = "black",
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    
    # White background
    panel.background = element_rect(
      fill = plot_background,
      colour = NA
    ),
    plot.background = element_rect(
      fill = plot_background,
      colour = NA
    ),
    
    # Space for the k labels outside the plot
    plot.margin = margin(
      t = 10,
      r = 100,
      b = 20,
      l = 20
    )
  )
# -------------------------------------------------------------------
# 15. Display the plot
# -------------------------------------------------------------------

print(bcf_plot)

# -------------------------------------------------------------------
# 16. Save the plot
# -------------------------------------------------------------------

ggsave(
  filename = plot_file,
  plot = bcf_plot,
  width = 13,
  height = 6.3,
  units = "in",
  dpi = 300,
  bg = plot_background
)

# -------------------------------------------------------------------
# 17. Export summary tables
# -------------------------------------------------------------------

write_xlsx(
  list(
    PFAS_summary = pfas_summary,
    Carbon_chain_summary = chain_summary
  ),
  path = summary_file
)

# -------------------------------------------------------------------
# 18. Display output locations
# -------------------------------------------------------------------

cat("Plot saved to:", plot_file, "\n")
cat("Summary tables saved to:", summary_file, "\n")