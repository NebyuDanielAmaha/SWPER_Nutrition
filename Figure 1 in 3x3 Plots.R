library(tidyverse)
library(ggrepel)
library(patchwork)

# 1. Data with income groups (same as before)
data <- tribble(
  ~Code, ~Country, ~Attitude, ~Social, ~Decision, ~HAZ, ~WAZ, ~WHZ,
  "AL", "Albania", 0.65, 0.94, 0.82, -0.398, 0.413, 0.864,
  "AO", "Angola", 0.35, -0.25, 0.59, -1.681, -1.179, -0.294,
  "AM", "Armenia", 0.60, 1.00, 0.70, -0.125, 0.414, 0.653,
  "BD", "Bangladesh", 0.57, -0.19, 0.28, -1.217, -1.190, -0.753,
  "BJ", "Benin", 0.07, -0.36, -0.32, -1.515, -1.074, -0.303,
  "BF", "Burkina Faso", -0.14, -0.41, -0.78, -1.123, -1.095, -0.664,
  "BU", "Burundi", -0.37, -0.10, 0.30, -2.283, -1.535, -0.335,
  "KH", "Cambodia", 0.18, 0.44, 0.80, -1.097, -0.964, -0.488,
  "CM", "Cameroon", 0.19, -0.29, -0.22, -1.199, -0.411, 0.366,
  "TD", "Chad", -0.97, -0.81, -0.60, -1.707, -1.374, -0.560,
  "CD", "Congo, DR", -0.47, -0.19, 0.06, -1.857, -1.226, -0.180,
  "CI", "Cote d'Ivoire", 0.19, -0.34, -0.51, -1.130, -0.860, -0.313,
  "ET", "Ethiopia", -0.75, -0.57, 0.48, -1.586, -1.266, -0.491,
  "GA", "Gabon", 0.31, 0.38, 0.33, -0.606, -0.288, 0.062,
  "GM", "Gambia, The", -0.28, -0.27, -0.45, -1.087, -0.899, -0.441,
  "GH", "Ghana", 0.37, 0.20, 0.24, -0.982, -0.872, -0.476,
  "GU", "Guatemala", 0.59, 0.06, 0.56, -1.950, -0.911, 0.268,
  "GN", "Guinea", -0.87, -0.62, -0.42, -1.223, -0.868, -0.242,
  "HT", "Haiti", 0.49, 0.20, 0.51, -1.004, -0.580, -0.026,
  "IA", "India", 0.19, 0.33, 0.47, -1.401, -1.466, -0.858,
  "KE", "Kenya", 0.21, 0.23, 0.52, -0.924, -0.650, -0.219,
  "LS", "Lesotho", 0.44, 0.51, 0.78, -1.568, -0.750, 0.152,
  "LB", "Liberia", 0.02, -0.32, 0.41, -1.371, -0.795, -0.036,
  "MD", "Madagascar", 0.18, -0.30, 0.73, -1.718, -1.342, -0.539,
  "MW", "Malawi", 0.48, -0.28, 0.18, -1.589, -0.843, 0.084,
  "ML", "Mali", -0.79, -0.52, -0.95, -1.291, -1.022, -0.433,
  "MR", "Mauritania", 0.15, -0.35, 0.10, -1.185, -0.972, -0.445,
  "MZ", "Mozambique", 0.32, -0.46, 0.02, -1.712, -0.974, -0.007,
  "MM", "Myanmar", 0.07, 0.48, 0.54, -1.441, -1.196, -0.553,
  "NP", "Nepal", 0.55, 0.08, -0.11, -1.292, -1.132, -0.599,
  "NG", "Nigeria", 0.18, -0.19, -0.37, -1.716, -1.369, -0.528,
  "PK", "Pakistan", -0.21, 0.07, -0.47, -1.647, -1.151, -0.266,
  "PG", "Papua New Guinea", -0.71, 0.14, 0.50, -1.787, -1.131, -0.079,
  "RW", "Rwanda", -0.24, 0.55, 0.50, -1.576, -0.610, 0.379,
  "SN", "Senegal", 0.26, -0.28, -0.61, -0.905, -0.993, -0.708,
  "SL", "Sierra Leone", -0.36, -0.38, -0.46, -1.334, -0.856, -0.145,
  "ZA", "South Africa", 0.68, 0.88, 0.90, -1.161, -0.193, 0.585,
  "TJ", "Tajikistan", -0.37, 0.54, -0.35, -0.730, -0.371, 0.056,
  "TZ", "Tanzania", -0.21, -0.06, 0.16, -1.471, -0.869, -0.071,
  "TL", "Timor-Leste", -0.97, 0.36, 0.86, -1.614, -1.720, -1.015,
  "UG", "Uganda", -0.06, -0.18, 0.23, -1.311, -0.656, 0.103,
  "ZM", "Zambia", 0.03, -0.10, 0.49, -1.524, -0.853, 0.009,
  "ZW", "Zimbabwe", 0.19, 0.10, 0.70, -1.297, -0.614, 0.142
) %>%
  mutate(income_group = case_when(
    Country %in% c("Burkina Faso","Burundi","Chad","Congo, DR","Ethiopia","Gambia, The",
                   "Liberia","Madagascar","Malawi","Mali","Mozambique","Rwanda",
                   "Sierra Leone","Uganda") ~ "Low income",
    Country %in% c("Angola","Bangladesh","Benin","Cambodia","Cameroon","Cote d'Ivoire",
                   "Ghana","Guinea","Haiti","India","Kenya","Lesotho","Mauritania",
                   "Myanmar","Nepal","Nigeria","Pakistan","Papua New Guinea","Senegal",
                   "Tajikistan","Tanzania","Timor-Leste","Zambia","Zimbabwe") ~ "Lower-middle income",
    Country %in% c("Albania","Armenia","Gabon","Guatemala","South Africa") ~ "Upper-middle income"
  )) %>%
  mutate(income_group = factor(income_group, levels = c("Low income", "Lower-middle income", "Upper-middle income")))

# 2. Plotting function - SIMPLIFIED, no legend title
plot_swper_growth <- function(df, xvar, yvar, xlab) {
  r_val <- cor(df %>% pull({{ xvar }}),
               df %>% pull({{ yvar }}),
               use = "complete.obs")
  
  ggplot(df, aes(x = {{ xvar }}, y = {{ yvar }}, color = income_group)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
    geom_smooth(method = "lm", color = "red", fill = "pink", alpha = 0.1, linewidth = 0.8) + 
    geom_point(size = 2.2, alpha = 0.7) +
    scale_color_manual(values = c("Low income" = "#D55E00", 
                                  "Lower-middle income" = "#0072B2", 
                                  "Upper-middle income" = "#0fff73"),
                       name = element_blank()) +  # No legend title
    annotate("text", x = Inf, y = -Inf, label = paste0("r = ", round(r_val, 2)),
             hjust = 1.1, vjust = -0.6, size = 4.5) +
    labs(x = xlab, y = deparse(substitute(yvar))) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11),
      legend.position = "none"
    )
}

# 3. Create all 9 plots
p1 <- plot_swper_growth(data, Attitude, HAZ, "Attitude Towards Violence")
p2 <- plot_swper_growth(data, Social, HAZ, "Social Independence") 
p3 <- plot_swper_growth(data, Decision, HAZ, "Decision Making")
p4 <- plot_swper_growth(data, Attitude, WAZ, "Attitude Towards Violence")
p5 <- plot_swper_growth(data, Social, WAZ, "Social Independence")
p6 <- plot_swper_growth(data, Decision, WAZ, "Decision Making")
p7 <- plot_swper_growth(data, Attitude, WHZ, "Attitude Towards Violence")
p8 <- plot_swper_growth(data, Social, WHZ, "Social Independence")
p9 <- plot_swper_growth(data, Decision, WHZ, "Decision Making")

# 4. CLEAN FIGURES - NO titles, NO subtitles, ONE legend each
haz_figure <- (p1 + p2 + p3 + 
                 plot_layout(ncol = 3, guides = "collect")) &
  labs(y = "Height-for-Age Z Score") &
  theme(axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

waz_figure <- (p4 + p5 + p6 + 
                 plot_layout(ncol = 3, guides = "collect")) &
  labs(y = "Weight-for-Age Z Score") &
  theme(axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

whz_figure <- (p7 + p8 + p9 + 
                 plot_layout(ncol = 3, guides = "collect")) &
  labs(y = "Weight-for-Height Z Score") &
  theme(axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

# 5. Display
haz_figure
waz_figure  
whz_figure

# 6. Save
ggsave("HAZ_three_panels.pdf", haz_figure, width = 16, height = 6.5, device = "pdf")

ggsave("WAZ_three_panels.pdf", waz_figure, width = 16, height = 6.5, device = "pdf") 

ggsave("WHZ_three_panels.pdf", whz_figure, width = 16, height = 6.5, device = "pdf")
