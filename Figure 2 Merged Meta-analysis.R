library(dplyr)
library(ggplot2)
library(patchwork)

# Create the data frame with all values
df <- tribble(
  ~outcome, ~domain, ~income, ~apr, ~lower, ~upper,
  "Stunting", "Attitude Towards Violence", "Upper-middle", 0.86, 0.79, 0.94,
  "Stunting", "Attitude Towards Violence", "Lower-middle", 0.96, 0.94, 0.97,
  "Stunting", "Attitude Towards Violence", "Low income", 0.97, 0.94, 0.97,
  "Stunting", "Attitude Towards Violence", "Overall", 0.95, 0.94, 0.97,
  "Stunting", "Social Independence", "Upper-middle", 0.85, 0.81, 0.89,
  "Stunting", "Social Independence", "Lower-middle", 0.88, 0.85, 0.90,
  "Stunting", "Social Independence", "Low income", 0.92, 0.90, 0.94,
  "Stunting", "Social Independence", "Overall", 0.89, 0.87, 0.91,
  "Stunting", "Decision Making", "Upper-middle", 0.89, 0.86, 0.92,
  "Stunting", "Decision Making", "Lower-middle", 0.96, 0.94, 0.97,
  "Stunting", "Decision Making", "Low income", 0.95, 0.93, 0.97,
  "Stunting", "Decision Making", "Overall", 0.95, 0.94, 0.96,
  "Underweight", "Attitude Towards Violence", "Upper-middle", 0.85, 0.77, 0.94,
  "Underweight", "Attitude Towards Violence", "Lower-middle", 0.96, 0.94, 0.98,
  "Underweight", "Attitude Towards Violence", "Low income", 0.96, 0.93, 0.98,
  "Underweight", "Attitude Towards Violence", "Overall", 0.96, 0.94, 0.97,
  "Underweight", "Social Independence", "Upper-middle", 0.91, 0.69, 1.19,
  "Underweight", "Social Independence", "Lower-middle", 0.89, 0.86, 0.92,
  "Underweight", "Social Independence", "Low income", 0.95, 0.91, 0.98,
  "Underweight", "Social Independence", "Overall", 0.91, 0.89, 0.93,
  "Underweight", "Decision Making", "Upper-middle", 0.87, 0.81, 0.93,
  "Underweight", "Decision Making", "Lower-middle", 0.96, 0.94, 0.98,
  "Underweight", "Decision Making", "Low income", 0.95, 0.93, 0.97,
  "Underweight", "Decision Making", "Overall", 0.95, 0.94, 0.97,
  "Wasting", "Attitude Towards Violence", "Upper-middle", 1.09, 0.78, 1.53,
  "Wasting", "Attitude Towards Violence", "Lower-middle", 0.98, 0.94, 1.03,
  "Wasting", "Attitude Towards Violence", "Low income", 0.98, 0.94, 1.03,
  "Wasting", "Attitude Towards Violence", "Overall", 0.98, 0.95, 1.01,
  "Wasting", "Social Independence", "Upper-middle", 1.17, 0.98, 1.39,
  "Wasting", "Social Independence", "Lower-middle", 0.96, 0.94, 0.99,
  "Wasting", "Social Independence", "Low income", 1.01, 0.95, 1.07,
  "Wasting", "Social Independence", "Overall", 0.98, 0.96, 1.00,
  "Wasting", "Decision Making", "Upper-middle", 0.75, 0.62, 0.91,
  "Wasting", "Decision Making", "Lower-middle", 0.95, 0.93, 0.97,
  "Wasting", "Decision Making", "Low income", 0.97, 0.93, 1.01,
  "Wasting", "Decision Making", "Overall", 0.95, 0.93, 0.97,
  "Overweight", "Attitude Towards Violence", "Upper-middle", 1.20, 0.84, 1.70,
  "Overweight", "Attitude Towards Violence", "Lower-middle", 1.07, 1.01, 1.12,
  "Overweight", "Attitude Towards Violence", "Low income", 1.00, 0.93, 1.09,
  "Overweight", "Attitude Towards Violence", "Overall", 1.05, 1.01, 1.09,
  "Overweight", "Social Independence", "Upper-middle", 1.04, 0.89, 1.20,
  "Overweight", "Social Independence", "Lower-middle", 1.11, 1.04, 1.19,
  "Overweight", "Social Independence", "Low income", 1.09, 1.00, 1.19,
  "Overweight", "Social Independence", "Overall", 1.09, 1.04, 1.15,
  "Overweight", "Decision Making", "Upper-middle", 1.07, 0.91, 1.25,
  "Overweight", "Decision Making", "Lower-middle", 1.02, 0.96, 1.09,
  "Overweight", "Decision Making", "Low income", 1.00, 0.91, 1.10,
  "Overweight", "Decision Making", "Overall", 1.02, 0.97, 1.07
)

# Define colors for domains
domain_colors <- c(
  "Attitude Towards Violence" = "#E41A1C", 
  "Social Independence" = "#377EB8", 
  "Decision Making" = "#4DAF4A"
)

# Loop over each outcome to create and save plots
for (outc in unique(df$outcome)) {
  d <- df %>% 
    filter(outcome == outc) %>%
    mutate(
      color = domain_colors[domain],
      point_shape = ifelse(income == "Overall", 23, 21),
      point_fill = ifelse(income == "Overall", color, "#ffffff")
    )
  
  # Initialize table_data without dummy rows
  table_data <- NULL
  
  domains <- unique(d$domain)
  for (dom in domains) {
    sub <- d %>% 
      filter(domain == dom) %>%
      mutate(
        label = income,
        est_text = sprintf("%.2f (%.2f; %.2f)", apr, lower, upper)
      ) %>%
      select(label, apr, lower, upper, est_text, color, point_shape, point_fill)
    
    domain_header <- data.frame(
      label = dom, 
      apr = NA_real_, 
      lower = NA_real_, 
      upper = NA_real_,
      est_text = "aPR (95% CI)",
      color = NA_character_,
      point_shape = NA_real_,
      point_fill = NA_character_
    )
    
    table_data <- bind_rows(table_data, domain_header, sub)
  }
  
  table_data <- table_data %>%
    mutate(y = nrow(.) : 1)  # Reverse y for top-to-bottom order
  
  # Define common y limits for perfect alignment across panels
  y_min <- min(table_data$y) - 0.5
  y_max <- max(table_data$y) + 0.5
  y_limits <- c(y_min, y_max)
  point_ys <- table_data$y[!is.na(table_data$apr)]
  
  # Label plot (left: income groups with bold domain headers)
  p_label <- ggplot(table_data, aes(y = y)) +
    geom_text(
      aes(x = 1, label = label, fontface = ifelse(is.na(apr), "bold", "plain")), 
      hjust = 1
    ) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(breaks = table_data$y, labels = NULL, limits = y_limits, expand = c(0, 0)) +
    theme_void() +
    theme(
      plot.margin = margin(5.5, 0, 5.5, 5.5),
      text = element_text(size = 14)
    )
  
  # Main plot (middle: aPR with CI and midline at 1)
  min_x <- min(d$lower) * 0.95
  max_x <- max(d$upper) * 1.05
  p_plot <- ggplot(table_data %>% filter(!is.na(apr)), aes(y = y)) +
    geom_vline(xintercept = 1, lty = 2) +
    geom_pointrange(
      aes(x = apr, xmin = lower, xmax = upper, shape = point_shape, fill = point_fill), 
      colour = "black", 
      size = 0.8
    ) +
    scale_shape_identity() +
    scale_fill_identity() +
    scale_y_continuous(breaks = point_ys, labels = NULL, limits = y_limits, expand = c(0, 0)) +
    scale_x_continuous() +
    coord_cartesian(xlim = c(min_x, max_x)) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5.5, 5.5, 5.5, 0)
    ) +
    labs(x = "adjusted Prevalence Ratio", title = outc)
  
  # Text plot (right: aPR with 95% CI)
  p_text <- ggplot(table_data, aes(y = y)) +
    geom_text(
      aes(
        x = 0, 
        label = est_text, 
        fontface = ifelse(is.na(apr), "bold", "plain")
      ), 
      hjust = 0, 
      na.rm = TRUE
    ) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(breaks = table_data$y, labels = NULL, limits = y_limits, expand = c(0, 0)) +
    theme_void() +
    theme(
      plot.margin = margin(5.5, 5.5, 5.5, 0),
      text = element_text(size = 14)
    )
  
  # Combine plots
  p <- p_label | p_plot | p_text
  p <- p + plot_layout(widths = c(4, 4, 3))  # Adjust widths as needed
  
  # Save the plot
  ggsave(filename = paste0(outc, "_plot.png"), plot = p, width = 10, height = 8, dpi = 300)
  ggsave(filename = paste0(outc, "_plot.pdf"), plot = p, width = 8, height = 8, dpi = 600)
  
}