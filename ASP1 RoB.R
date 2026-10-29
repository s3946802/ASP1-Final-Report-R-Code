# Install packages if required
# install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "stringr"))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Optional: helps ensure Times New Roman is used when exporting
# install.packages("showtext")
library(showtext)
showtext_auto()

# -------------------------------------------------------------------
# 1. Read the Excel file
# -------------------------------------------------------------------

file_path <- "C:/Users/flynn/Documents/ASP1 R/Written Report/Written Report R/Risk of Bias R.xlsx"

raw_data <- read_excel(
  path = file_path,
  sheet = 1,
  col_names = TRUE
)

# Check that there are six risk-of-bias columns
if (ncol(raw_data) != 6) {
  stop("The Excel file must contain exactly six risk-of-bias item columns.")
}

# Keep the first twelve rows below the header
risk_data <- raw_data %>%
  slice(1:12)

# Store the risk-of-bias item names
item_names <- names(risk_data)

# -------------------------------------------------------------------
# 2. Clean and check the High/Low values
# -------------------------------------------------------------------

long_data <- risk_data %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "risk"
  ) %>%
  mutate(
    original_risk = as.character(risk),
    
    # Remove extra spaces and standardise capitalisation
    risk = str_to_lower(str_squish(as.character(risk))),
    
    # Recognise common variations
    risk = case_when(
      risk %in% c("high", "high risk", "high-risk") ~ "high",
      risk %in% c("low", "low risk", "low-risk") ~ "low",
      TRUE ~ risk
    )
  )

# Identify invalid or missing values
invalid_values <- long_data %>%
  filter(is.na(risk) | !risk %in% c("high", "low"))

if (nrow(invalid_values) > 0) {
  print(
    invalid_values %>%
      count(original_risk, sort = TRUE)
  )
  
  stop(
    "Some cells do not contain a recognised High or Low value. ",
    "The unrecognised values are shown above."
  )
}

# -------------------------------------------------------------------
# 3. Calculate percentages
# -------------------------------------------------------------------

risk_percentages <- long_data %>%
  count(item, risk, name = "count") %>%
  complete(
    item = item_names,
    risk = c("high", "low"),
    fill = list(count = 0)
  ) %>%
  group_by(item) %>%
  mutate(
    percentage = count / sum(count) * 100
  ) %>%
  ungroup()

# -------------------------------------------------------------------
# 4. Prepare the bar data
#    High risk is placed closest to 0
# -------------------------------------------------------------------

plot_data <- risk_percentages %>%
  select(item, risk, percentage) %>%
  pivot_wider(
    names_from = risk,
    values_from = percentage,
    values_fill = 0
  ) %>%
  mutate(
    item_number = match(item, item_names),
    
    # The first Excel item appears at the top
    y = length(item_names) - item_number + 1,
    
    # High risk starts at 0; low risk follows high risk
    high_start = 0,
    high_end = high,
    low_start = high,
    low_end = high + low
  )

# Create rectangles for each bar section
bar_data <- bind_rows(
  plot_data %>%
    transmute(
      item,
      y,
      risk = "High risk",
      xmin = high_start,
      xmax = high_end
    ),
  
  plot_data %>%
    transmute(
      item,
      y,
      risk = "Low risk",
      xmin = low_start,
      xmax = low_end
    )
)
# -------------------------------------------------------------------
# 5. Define the domains
# -------------------------------------------------------------------

# Items 1 and 2 = Domain 1
# Items 3 and 4 = Domain 2
# Item 5         = Domain 3
# Item 6         = Domain 4

domain_data <- tibble(
  domain = c("Exposure Characterisation", "Outcome Measurement", "Missing data / selective reporting", "Precision and reproducibility"),
  first_column = c(1, 3, 5, 6),
  last_column  = c(2, 4, 5, 6)
) %>%
  mutate(
    ymin = length(item_names) - last_column + 1 - 0.45,
    ymax = length(item_names) - first_column + 1 + 0.45,
    xmin = 0,
    xmax = 100,
    label_y = (ymin + ymax) / 2,
    label_x = 102
  )

# -------------------------------------------------------------------
# 6. Create the plot
# -------------------------------------------------------------------

risk_plot <- ggplot() +
  
  # High- and low-risk bar sections
  geom_rect(
    data = bar_data,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = y - 0.32,
      ymax = y + 0.32,
      fill = risk
    ),
    colour = "white",
    linewidth = 0.3
  ) +
  
  # Boxes surrounding each domain
  geom_rect(
    data = domain_data,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    fill = NA,
    colour = "black",
    linewidth = 0.8
  ) +
  
  # Domain labels on the right
  geom_text(
    data = domain_data,
    aes(
      x = label_x,
      y = label_y,
      label = domain
    ),
    hjust = 0,
    family = "Times New Roman",
    size = 14
  ) +
  
  # Colours
  scale_fill_manual(
    values = c(
      "High risk" = "#D73027",
      "Low risk" = "#2E8B57"
    ),
    breaks = c("High risk", "Low risk")
  ) +
  
  # X-axis: 0 to 100 in intervals of 25
  scale_x_continuous(
    breaks = seq(0, 100, by = 25),
    limits = c(0, 115),
    expand = c(0, 0),
    name = "Percentage (%)"
  ) +
  
  # Y-axis: first item at the top
  scale_y_continuous(
    breaks = seq_along(item_names),
    labels = rev(item_names),
    expand = expansion(mult = c(0.02, 0.02)),
    name = "Risk-of-bias item"
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    title = NULL,
    fill = NULL
  ) +
  
  theme_minimal(
    base_size = 12,
    base_family = "Times New Roman"
  ) +
  
  theme(
    text = element_text(
      family = "Times New Roman"
    ),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(
      family = "Times New Roman",
      size = 60
    ),
    
    axis.text.y = element_text(
      family = "Times New Roman",
      hjust = 1,
      size = 60
    ),
    
    axis.title.x = element_text(
      family = "Times New Roman",
      face = "bold",
      size = 70,
      margin = margin(t = 15)
    ),
    
    axis.title.y = element_text(
      family = "Times New Roman",
      face = "bold",
      size = 70,
      margin = margin(r = 15)
    ),
    
    legend.text = element_text(
      family = "Times New Roman",
      size = 60
    ),
    
    legend.position = "right",
    
    # Reduce the space between the plot and the legend
    legend.box.spacing = grid::unit(100, "pt"),
    
    # Keep a small outer margin on the right
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 10,
      l = 10
    )
  )

# Display the plot
print(risk_plot)

# -------------------------------------------------------------------
# 7. Save the plot
# -------------------------------------------------------------------

ggsave(
  filename = "risk_of_bias_plot.png",
  plot = risk_plot,
  width = 16,
  height = 10,
  dpi = 300
)