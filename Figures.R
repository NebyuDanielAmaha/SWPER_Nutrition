#* STUNTING
library(metafor)
library(dplyr)
library(meta)

meta_info <- survey_years %>%
  mutate(
    income_group = case_when(
      country %in% c(
        "Burkina Faso","Burundi","Chad","Congo, DR","Ethiopia","Gambia, The",
        "Liberia","Madagascar","Malawi","Mali","Mozambique","Rwanda",
        "Sierra Leone","Uganda"
      ) ~ "Low income",
      
      country %in% c(
        "Angola","Bangladesh","Benin","Cambodia","Cameroon","Cote d'Ivoire",
        "Ghana","Guinea","Haiti","India","Kenya","Lesotho","Mauritania",
        "Myanmar","Nepal","Nigeria","Pakistan","Papua New Guinea","Senegal",
        "Tajikistan","Tanzania","Timor-Leste","Zambia","Zimbabwe"
      ) ~ "Lower-middle income",
      
      country %in% c(
        "Albania","Armenia","Gabon","Guatemala","South Africa"
      ) ~ "Upper-middle income"
    ),
    study_label = paste0(country, " ", survey_year)
  )


dat <- data.frame(
  att_pr = c(
    0.849,0.996,0.863,0.905,0.965,0.938,0.976,0.930,0.982,0.994,
    0.970,0.900,0.967,0.824,0.897,0.915,0.897,0.985,0.866,0.956,
    0.899,0.839,0.902,0.965,0.932,0.966,1.033,0.984,1.001,0.815,
    0.947,0.941,1.003,0.912,1.028,0.977,0.661,1.003,0.933,1.005,
    0.975,0.939,0.921
  ),
  att_se = c(
    0.147,0.035,0.176,0.062,0.017,0.032,0.012,0.056,0.038,0.014,
    0.017,0.032,0.017,0.080,0.041,0.047,0.016,0.030,0.044,0.012,
    0.025,0.065,0.052,0.029,0.038,0.016,0.037,0.044,0.040,0.055,
    0.015,0.027,0.029,0.022,0.039,0.032,0.097,0.046,0.028,0.019,
    0.033,0.025,0.039
  ),
  soc_pr = c(
    0.810,0.919,1.070,0.805,0.887,0.979,0.931,0.911,0.954,0.946,
    0.900,0.914,0.934,0.823,1.020,0.826,0.846,0.905,0.848,0.843,
    0.805,0.742,0.987,0.922,0.855,0.898,0.939,0.918,0.934,0.837,
    0.830,0.750,0.894,0.877,0.925,0.963,0.778,1.101,0.887,0.955,
    0.838,0.868,0.830
  ),
  soc_se = c(
    0.078,0.037,0.131,0.043,0.021,0.045,0.020,0.061,0.044,0.022,
    0.021,0.046,0.032,0.080,0.076,0.055,0.015,0.037,0.037,0.013,
    0.030,0.069,0.082,0.031,0.037,0.024,0.033,0.045,0.038,0.050,
    0.017,0.035,0.034,0.033,0.056,0.040,0.108,0.079,0.040,0.019,
    0.039,0.040,0.038
  ),
  dec_pr = c(
    0.941,0.934,0.939,0.924,0.959,0.975,0.973,0.976,0.959,0.959,
    0.996,0.941,0.942,0.863,1.071,1.024,0.889,1.028,0.973,0.977,
    0.950,0.949,0.863,0.899,0.943,0.912,0.922,0.976,1.038,0.957,
    0.960,0.895,0.970,0.903,0.940,0.984,0.988,1.022,1.006,0.946,
    0.924,0.906,0.886
  ),
  dec_se = c(
    0.084,0.036,0.143,0.032,0.017,0.033,0.014,0.061,0.030,0.019,
    0.018,0.038,0.024,0.084,0.070,0.054,0.014,0.032,0.045,0.012,
    0.030,0.106,0.049,0.028,0.027,0.023,0.024,0.030,0.044,0.040,
    0.016,0.030,0.040,0.030,0.043,0.029,0.223,0.048,0.036,0.034,
    0.032,0.032,0.040
  )
)

dat$country <- meta_info$country

dat <- dat %>%
  left_join(meta_info, by = "country")



#log transform
dat$TE_att <- log(dat$att_pr)
dat$se_att <- dat$att_se

dat$TE_soc <- log(dat$soc_pr)
dat$se_soc <- dat$soc_se

dat$TE_dec <- log(dat$dec_pr)
dat$se_dec <- dat$dec_se

df <- dat
dat <- dat %>%
  mutate(
    TE_att = log(att_pr),
    se_att = att_se / att_pr
    
  )


# --- 2. Meta-analysis function ---
run_swper_meta <- function(data, pr_col, se_col, plot_title) {
  
  domain_data <- data %>% filter(!is.na(.data[[pr_col]]), !is.na(.data[[se_col]]))
  
  m <- metagen(
    TE = log(domain_data[[pr_col]]),
    seTE = domain_data[[se_col]] / domain_data[[pr_col]],
    sm = "RR",
    method.tau = "DL",
    random = TRUE,
    fixed = FALSE,
    studlab = domain_data$study_label,
    subgroup = domain_data$income_group,
    test.subgroup = FALSE,
    subgroup.name = ""
  )
  
  
  # Forest plot
  forest(
    m,
    leftcols = c("studlab"),
    rightcols = c("effect","ci", "w.random"),
    leftlabs = c("Country Survey Year"),
    rightlabs = c("aPR", "95% Con. Int.","Weight (%)"),
    smlab = "Adjusted Prevalence Ratio (aPR)",
    #xlab = "Adjusted Prevalence Ratio (aPR)",
    squaresize = 1.4,
    #spacing = 0.8,
    col.diamond = "red",
    col.square = "midnightblue",
    fontsize = 8,
    spacing = 0.6,
    print.subgroup.test = FALSE,
    print.subgroup.labels = TRUE,
    text.random = "Pooled aPR",
    fixed = FALSE,
    comb.fixed = FALSE
    
  )
  
  return(m)
}

# --- 3. Save ---

# Attitude towards Violence

tiff("Forest_Attitude_Stunting.tiff",
     width = 180, height = 220, units = "mm",
     res = 600, compression = "lzw")
run_swper_meta(df, "att_pr", "att_se", "Attitude Towards Violence & Stunting")
dev.off()


# Social Independence

tiff("Forest_Social_Stunting.tiff",
     width = 180, height = 220, units = "mm",
     res = 600, compression = "lzw")
run_swper_meta(df, "soc_pr", "soc_se", "Social Independence & Stunting")
dev.off()

# Decision Making

tiff("Forest_Decision_Stunting.tiff",
     width = 180, height = 220, units = "mm",
     res = 600, compression = "lzw")
run_swper_meta(df, "dec_pr", "dec_se", "Decision Making & Stunting")
dev.off()


#*UNDERWEIGHT

dat <- data.frame(
  att_pr = c(
    1.667,1.051,0.765,0.820,0.950,0.965,0.944,0.960,1.018,0.983,
    0.975,0.993,0.938,0.749,0.864,1.064,0.874,1.008,0.912,0.953,
    0.912,1.007,0.813,0.992,0.991,1.000,0.963,1.079,0.979,0.945,
    0.966,0.862,1.006,0.896,1.044,0.917,0.881,0.939,0.866,0.991,
    0.851,0.910,0.977
  ),
  att_se = c(
    1.354,0.062,0.184,0.053,0.030,0.035,0.023,0.067,0.070,0.018,
    0.026,0.051,0.025,0.091,0.045,0.071,0.049,0.046,0.080,0.014,
    0.030,0.174,0.101,0.047,0.075,0.023,0.035,0.087,0.048,0.075,
    0.022,0.032,0.053,0.051,0.040,0.048,0.551,0.077,0.047,0.019,
    0.053,0.050,0.073
  ),
  soc_pr = c(
    0.845,0.922,1.336,0.828,0.915,1.002,1.014,0.928,0.887,0.979,
    0.873,0.923,0.966,0.571,0.925,0.842,0.861,0.893,0.880,0.856,
    0.830,0.593,1.119,0.955,0.742,0.944,0.914,0.971,0.970,0.788,
    0.830,0.837,0.960,0.953,0.928,0.946,1.523,1.165,0.895,0.962,
    0.906,0.936,0.834
  ),
  soc_se = c(
    0.214,0.072,0.250,0.044,0.033,0.047,0.040,0.068,0.079,0.032,
    0.032,0.062,0.048,0.103,0.086,0.072,0.038,0.057,0.065,0.016,
    0.037,0.132,0.191,0.044,0.061,0.031,0.040,0.078,0.053,0.058,
    0.026,0.049,0.065,0.107,0.063,0.062,0.612,0.126,0.066,0.018,
    0.076,0.087,0.086
  ),
  dec_pr = c(
    0.805,0.938,0.519,0.956,0.933,0.923,0.983,0.965,0.926,0.949,
    0.982,0.994,0.895,0.980,1.054,0.932,0.865,1.009,1.004,0.986,
    0.959,0.800,0.860,0.903,0.952,0.906,0.940,0.982,0.993,0.909,
    0.909,0.872,0.942,0.983,0.981,0.922,1.403,1.121,0.980,1.025,
    0.930,0.981,0.999
  ),
  dec_se = c(
    0.239,0.060,0.193,0.034,0.027,0.037,0.026,0.085,0.049,0.024,
    0.027,0.058,0.029,0.160,0.076,0.055,0.033,0.047,0.079,0.013,
    0.040,0.146,0.108,0.045,0.058,0.034,0.034,0.059,0.055,0.046,
    0.022,0.042,0.059,0.095,0.043,0.046,0.958,0.087,0.053,0.040,
    0.060,0.070,0.100
  )
)

dat$country <- meta_info$country

dat <- dat %>%
  left_join(meta_info, by = "country")



#log transform
dat$TE_att <- log(dat$att_pr)
dat$se_att <- dat$att_se

dat$TE_soc <- log(dat$soc_pr)
dat$se_soc <- dat$soc_se

dat$TE_dec <- log(dat$dec_pr)
dat$se_dec <- dat$dec_se

df <- dat
dat <- dat %>%
  mutate(
    TE_att = log(att_pr),
    se_att = att_se / att_pr
    
  )


# --- 2. Meta-analysis function ---
run_swper_meta <- function(data, pr_col, se_col, plot_title) {
  
  domain_data <- data %>% filter(!is.na(.data[[pr_col]]), !is.na(.data[[se_col]]))
  
  m <- metagen(
    TE = log(domain_data[[pr_col]]),
    seTE = domain_data[[se_col]] / domain_data[[pr_col]],
    sm = "RR",
    method.tau = "DL",
    random = TRUE,
    fixed = FALSE,
    studlab = domain_data$study_label,
    subgroup = domain_data$income_group,
    test.subgroup = FALSE,
    subgroup.name = ""
  )
  
  
  # Forest plot
  forest(
    m,
    leftcols = c("studlab"),
    rightcols = c("effect","ci", "w.random"),
    leftlabs = c("Country Survey Year"),
    rightlabs = c("aPR", "95% Con. Int.","Weight (%)"),
    smlab = "Adjusted Prevalence Ratio (aPR)",
    #xlab = "Adjusted Prevalence Ratio (aPR)",
    squaresize = 1.4,
    #spacing = 0.8,
    col.diamond = "red",
    col.square = "midnightblue",
    fontsize = 8,
    spacing = 0.6,
    print.subgroup.test = FALSE,
    print.subgroup.labels = TRUE,
    text.random = "Pooled aPR",
    fixed = FALSE,
    comb.fixed = FALSE
    
  )
  
  return(m)
}

# --- 3. Save ---

# Attitude towards Violence

tiff("Forest_Attitude_Underweight.tiff",
     width = 180, height = 220, units = "mm",
     res = 600, compression = "lzw")
run_swper_meta(df, "att_pr", "att_se", "Attitude Towards Violence & Underweight")
dev.off()


# Social Independence

tiff("Forest_Social_Underweight.tiff",
     width = 180, height = 220, units = "mm",
     res = 600, compression = "lzw")
run_swper_meta(df, "soc_pr", "soc_se", "Social Independence & Underweight")
dev.off()

# Decision Making

tiff("Forest_Decision_Underweight.tiff",
     width = 180, height = 220, units = "mm",
     res = 600, compression = "lzw")
run_swper_meta(df, "dec_pr", "dec_se", "Decision Making & Underweight")
dev.off()

#*WASTING 

dat <- data.frame(
  att_pr = c(
    1.874,0.937,1.363,0.855,0.976,0.967,0.967,1.137,1.127,0.995,
    1.088,1.064,0.883,0.828,0.957,1.090,0.925,1.060,0.709,0.950,
    0.911,0.851,0.715,0.888,1.177,1.061,0.862,0.877,1.081,1.891,
    0.934,0.940,1.024,1.016,1.166,1.079,5.686,1.119,0.779,0.952,
    0.844,0.932,1.082
  ),
  att_se = c(
    0.998,0.105,0.328,0.100,0.058,0.048,0.065,0.119,0.121,0.032,
    0.061,0.085,0.040,0.154,0.089,0.125,0.257,0.075,0.095,0.021,
    0.042,0.228,0.191,0.092,0.191,0.039,0.053,0.133,0.116,0.452,
    0.046,0.085,0.102,0.185,0.073,0.093,7.211,0.094,0.074,0.030,
    0.095,0.130,0.158
  ),
  soc_pr = c(
    1.223,1.211,1.149,0.974,1.044,0.967,1.314,0.923,1.013,1.014,
    0.843,0.925,1.063,1.372,0.864,0.850,1.063,0.937,1.131,0.957,
    0.874,1.368,1.614,0.976,0.937,1.047,0.942,0.799,0.974,0.886,
    0.948,0.967,1.131,1.353,0.947,1.043,1.347,1.059,0.846,0.982,
    0.993,1.067,0.835
  ),
  soc_se = c(
    0.328,0.274,0.158,0.077,0.064,0.061,0.143,0.083,0.178,0.054,
    0.068,0.076,0.100,0.377,0.122,0.114,0.174,0.081,0.119,0.026,
    0.059,0.355,0.455,0.088,0.165,0.061,0.062,0.170,0.083,0.118,
    0.049,0.117,0.146,0.527,0.082,0.086,0.403,0.130,0.142,0.035,
    0.130,0.227,0.135
  ),
  dec_pr = c(
    0.752,1.011,0.671,0.917,0.893,0.925,1.076,0.958,0.854,0.972,
    0.965,0.990,0.943,0.773,0.955,0.844,0.757,0.938,0.968,0.955,
    0.941,0.702,0.614,0.965,1.252,0.947,0.974,0.875,0.967,0.942,
    0.907,1.047,0.838,1.283,1.075,1.037,1.400,1.042,0.934,1.023,
    1.010,0.979,0.906
  ),
  dec_se = c(
    0.198,0.137,0.127,0.051,0.050,0.051,0.081,0.100,0.081,0.047,
    0.055,0.066,0.071,0.157,0.090,0.073,0.132,0.062,0.114,0.019,
    0.053,0.223,0.136,0.085,0.166,0.054,0.057,0.112,0.097,0.082,
    0.048,0.103,0.064,0.484,0.066,0.077,0.861,0.075,0.114,0.074,
    0.131,0.169,0.150
  )
)

dat$country <- meta_info$country

dat <- dat %>%
  left_join(meta_info, by = "country")


#log transform
dat$TE_att <- log(dat$att_pr)
dat$se_att <- dat$att_se

dat$TE_soc <- log(dat$soc_pr)
dat$se_soc <- dat$soc_se

dat$TE_dec <- log(dat$dec_pr)
dat$se_dec <- dat$dec_se

df <- dat
dat <- dat %>%
  mutate(
    TE_att = log(att_pr),
    se_att = att_se / att_pr
    
  )


# --- 2. Meta-analysis function ---
run_swper_meta <- function(data, pr_col, se_col, plot_title) {
  
  domain_data <- data %>% filter(!is.na(.data[[pr_col]]), !is.na(.data[[se_col]]))
  
  m <- metagen(
    TE = log(domain_data[[pr_col]]),
    seTE = domain_data[[se_col]] / domain_data[[pr_col]],
    sm = "RR",
    method.tau = "DL",
    random = TRUE,
    fixed = FALSE,
    studlab = domain_data$study_label,
    subgroup = domain_data$income_group,
    test.subgroup = FALSE,
    subgroup.name = ""
  )
  
  
  # Forest plot
  forest(
    m,
    leftcols = c("studlab"),
    rightcols = c("effect","ci", "w.random"),
    leftlabs = c("Country Survey Year"),
    rightlabs = c("aPR", "95% Con. Int.","Weight (%)"),
    smlab = "Adjusted Prevalence Ratio (aPR)",
    #xlab = "Adjusted Prevalence Ratio (aPR)",
    squaresize = 1.4,
    #spacing = 0.8,
    col.diamond = "red",
    col.square = "midnightblue",
    fontsize = 8,
    spacing = 0.6,
    print.subgroup.test = FALSE,
    print.subgroup.labels = TRUE,
    text.random = "Pooled aPR",
    fixed = FALSE,
    comb.fixed = FALSE
    
  )
  
  return(m)
}

# --- 3. Save ---

# Attitude towards Violence

tiff("Forest_Attitude_Wasting.tiff",
     width = 180, height = 220, units = "mm",
     res = 600, compression = "lzw")
run_swper_meta(df, "att_pr", "att_se", "Attitude Towards Violence & Wasting")
dev.off()


# Social Independence

tiff("Forest_Social_Wasting.tiff",
     width = 180, height = 220, units = "mm",
     res = 600, compression = "lzw")
run_swper_meta(df, "soc_pr", "soc_se", "Social Independence & Wasting")
dev.off()

# Decision Making

tiff("Forest_Decision_Wasting.tiff",
     width = 180, height = 220, units = "mm",
     res = 600, compression = "lzw")
run_swper_meta(df, "dec_pr", "dec_se", "Decision Making & Wasting")
dev.off()

#*OVERWEIGHT

dat <- data.frame(
  att_pr = c(
    0.973,1.102,0.851,0.951,1.048,0.717,0.961,1.524,0.997,1.170,
    1.010,1.160,0.877,1.569,1.931,0.865,1.381,1.070,1.237,1.031,
    1.002,1.255,1.001,0.815,0.920,1.152,0.968,1.050,0.981,3.342,
    1.023,1.429,1.050,1.007,1.163,1.037,33.078,1.165,1.202,1.050,
    1.028,0.998,0.968
  ),
  att_se = c(
    0.160,0.263,0.135,0.334,0.114,0.101,0.162,0.323,0.078,0.094,
    0.097,0.164,0.098,0.407,0.684,0.299,0.225,0.098,0.231,0.065,
    0.160,0.461,0.195,0.150,0.161,0.122,0.153,0.195,0.238,4.158,
    0.139,0.245,0.097,0.076,0.489,0.130,45.496,0.104,0.159,0.077,
    0.146,0.159,0.123
  ),
  soc_pr = c(
    0.939,1.126,0.898,1.144,1.328,1.448,0.885,1.245,1.182,0.962,
    1.287,1.291,1.260,1.223,1.421,0.852,1.208,1.076,1.031,1.046,
    1.044,1.460,1.066,0.815,0.981,1.088,0.838,0.940,0.793,1.201,
    1.081,1.488,1.083,1.131,2.123,1.018,1.000,0.851,1.438,1.090,
    1.087,0.843,1.193
  ),
  soc_se = c(
    0.057,0.384,0.085,0.219,0.156,0.261,0.144,0.159,0.093,0.124,
    0.161,0.233,0.219,0.205,0.492,0.139,0.084,0.109,0.112,0.078,
    0.126,0.292,0.349,0.186,0.204,0.173,0.112,0.231,0.155,0.360,
    0.194,0.207,0.111,0.128,0.506,0.153,0.264,0.138,0.250,0.093,
    0.159,0.140,0.141
  ),
  dec_pr = c(
    1.018,0.798,0.870,1.061,1.024,0.980,0.911,0.683,1.016,1.006,
    1.114,1.160,1.304,1.102,1.110,0.928,1.284,0.980,0.895,0.998,
    0.966,2.740,0.854,1.755,0.876,0.763,1.096,1.208,0.742,1.575,
    1.150,1.014,1.520,0.909,1.362,0.840,1.457,1.157,0.869,0.902,
    1.150,1.405,1.021
  ),
  dec_se = c(
    0.088,0.189,0.085,0.206,0.100,0.168,0.129,0.106,0.065,0.095,
    0.113,0.140,0.203,0.159,0.246,0.211,0.147,0.090,0.117,0.069,
    0.098,1.687,0.184,0.403,0.093,0.120,0.185,0.186,0.181,0.573,
    0.148,0.166,0.260,0.098,0.478,0.088,0.456,0.106,0.105,0.107,
    0.169,0.242,0.169
  )
)

dat$country <- meta_info$country

dat <- dat %>%
  left_join(meta_info, by = "country")



#log transform
dat$TE_att <- log(dat$att_pr)
dat$se_att <- dat$att_se

dat$TE_soc <- log(dat$soc_pr)
dat$se_soc <- dat$soc_se

dat$TE_dec <- log(dat$dec_pr)
dat$se_dec <- dat$dec_se

df <- dat
dat <- dat %>%
  mutate(
    TE_att = log(att_pr),
    se_att = att_se / att_pr
    
  )


# --- 2. Meta-analysis function ---
run_swper_meta <- function(data, pr_col, se_col, plot_title) {
  
  domain_data <- data %>% filter(!is.na(.data[[pr_col]]), !is.na(.data[[se_col]]))
  
  m <- metagen(
    TE = log(domain_data[[pr_col]]),
    seTE = domain_data[[se_col]] / domain_data[[pr_col]],
    sm = "RR",
    method.tau = "DL",
    random = TRUE,
    fixed = FALSE,
    studlab = domain_data$study_label,
    subgroup = domain_data$income_group,
    test.subgroup = FALSE,
    subgroup.name = ""
  )
  
  
  # Forest plot
  forest(
    m,
    leftcols = c("studlab"),
    rightcols = c("effect","ci", "w.random"),
    leftlabs = c("Country Survey Year"),
    rightlabs = c("aPR", "95% Con. Int.","Weight (%)"),
    smlab = "Adjusted Prevalence Ratio (aPR)",
    #xlab = "Adjusted Prevalence Ratio (aPR)",
    squaresize = 1.4,
    #spacing = 0.8,
    col.diamond = "red",
    col.square = "midnightblue",
    fontsize = 8,
    spacing = 0.6,
    print.subgroup.test = FALSE,
    print.subgroup.labels = TRUE,
    text.random = "Pooled aPR",
    fixed = FALSE,
    comb.fixed = FALSE
    
  )
  
  return(m)
}

# --- 3. Save PDFs ---

# Attitude towards Violence

tiff("Forest_Attitude_Overweight.tiff",
     width = 180, height = 220, units = "mm",
     res = 600, compression = "lzw")
run_swper_meta(df, "att_pr", "att_se", "Attitude Towards Violence & Overweight")
dev.off()


# Social Independence

tiff("Forest_Social_Overweight.tiff",
     width = 180, height = 220, units = "mm",
     res = 600, compression = "lzw")
run_swper_meta(df, "soc_pr", "soc_se", "Social Independence & Overweight")
dev.off()

# Decision Making

tiff("Forest_Decision_Overweight.tiff",
     width = 180, height = 220, units = "mm",
     res = 600, compression = "lzw")
run_swper_meta(df, "dec_pr", "dec_se", "Decision Making & Overweight")
dev.off()
