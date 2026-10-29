# Load packages
library(readxl)
library(tidyverse)
library(viridis)

# File paths
file_path <- "C:\\Users\\flynn\\Documents\\ASP1 R\\Written Report\\Written Report R\\Evidence Map R V2.xlsx"

output_path <- "C:/Users/flynn/Documents/ASP1 R/Written Report/evidence_map_top8.png"

# Import the Excel file
pfas_data <- read_excel(
  path = file_path,
  sheet = "Sheet1",
  col_names = TRUE
)

# Rename the first column as the plant-species column
names(pfas_data)[1] <- "Plant_Species"

# Convert from wide format to long format
pfas_long <- pfas_data %>%
  pivot_longer(
    cols = -Plant_Species,
    names_to = "PFAS_Species",
    values_to = "Value"
  ) %>%
  mutate(
    Plant_Species = str_trim(as.character(Plant_Species)),
    PFAS_Species = str_trim(as.character(PFAS_Species)),
    Value = as.numeric(Value)
  )

# Identify the eight most abundant PFAS species
top_pfas <- pfas_long %>%
  group_by(PFAS_Species) %>%
  summarise(
    total_studies = sum(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_studies), PFAS_Species) %>%
  slice_head(n = 8) %>%
  arrange(total_studies, PFAS_Species) %>%
  pull(PFAS_Species)

# Identify the eight most abundant plant species
top_plants <- pfas_long %>%
  group_by(Plant_Species) %>%
  summarise(
    total_studies = sum(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_studies), Plant_Species) %>%
  slice_head(n = 8) %>%
  arrange(total_studies, Plant_Species) %>%
  pull(Plant_Species)

# Keep only the selected PFAS and plant species
pfas_top8 <- pfas_long %>%
  filter(
    PFAS_Species %in% top_pfas,
    Plant_Species %in% top_plants
  ) %>%
  mutate(
    PFAS_Species = factor(
      PFAS_Species,
      levels = top_pfas
    ),
    Plant_Species = factor(
      Plant_Species,
      levels = rev(top_plants)
    )
  )

# Optional: display the selected species
print(top_pfas)
print(top_plants)

# Create the evidence-map heatmap
evidence_map <- ggplot(
  pfas_top8,
  aes(
    x = PFAS_Species,
    y = Plant_Species,
    fill = Value
  )
) +
  geom_tile(
    colour = "black",
    linewidth = 0.3
  ) +
  geom_text(
    aes(
      label = if_else(is.na(Value), "", as.character(Value)),
      colour = case_when(
        is.na(Value) ~ "black",
        Value >= 20 ~ "white",
        TRUE ~ "black"
      )
    ),
    family = "SegoeUI",
    size = 20,
    na.rm = TRUE
  ) +
  scale_colour_identity() +
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    name = "Number of studies",
    na.value = "grey90"
  ) +
  guides(
    fill = guide_colourbar(
      title.position = "top",
      title.hjust = 0.5,
      frame.colour = "black",
      frame.linewidth = 0.3,
      ticks = TRUE,
      ticks.colour = "black",
      ticks.linewidth = 0.3,
      draw.ulim = TRUE,
      draw.llim = TRUE
    )
  ) +
  labs(
    x = "PFAS species",
    y = "Plant species"
  ) +
  theme_minimal(
    base_family = "SegoeUI",
    base_size = 10
  ) +
  theme(
    axis.text.x = element_text(
      family = "SegoeUI",
      size = 45,
      angle = 45,
      hjust = 1,
      face = "bold"
    ),
    axis.text.y = element_text(
      family = "SegoeUI",
      size = 45,
      face = "bold"
    ),
    axis.title = element_text(
      family = "SegoeUI",
      size = 50,
      face = "bold"
    ),
    legend.text = element_text(
      family = "SegoeUI",
      size = 45,
      colour = "black",
      face = "bold"
    ),
    legend.box.spacing = grid::unit(2, "pt"),
    legend.margin = margin(0, 0, 0, 0),
    legend.title = element_text(
      family = "SegoeUI",
      size = 50,
      face = "bold",
      hjust = 0.5,
      colour = "black",
      margin = margin(b = 20, unit = "pt")
    ),
    panel.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    plot.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    panel.grid = element_blank()
  )

# Save the graph
ggsave(
  filename = output_path,
  plot = evidence_map,
  width = 13,
  height = 6.3,
  units = "in",
  dpi = 300,
  bg = "#DCEAF7"
)

# Display the graph
evidence_map