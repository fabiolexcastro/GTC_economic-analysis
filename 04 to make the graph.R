
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, geodata, raster, glue, tidyverse, readxl, xlsx, openxlsx)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
tble <- read_csv('./tbl/result.csv', show_col_types = FALSE)
crps <- unique(tble$Crop)

# To make the density graph  ----------------------------------------------

##
make.graph <- function(crp){
  
  ## Filtering
  cat('To start the process: ', crp, '\n')
  tbl <- filter(tble, Crop == crp)
  
  ## To build the table 
  fnl <- tibble(value = c(pull(tbl, dfr_low), pull(tbl, dfr_mid), pull(tbl, dfr_hig)), type = 'A')
  avg <- tibble(avg = mean(fnl$value))
  
  data <- fnl
  data <- setNames(data, 'cost')
  
  ## Compute density
  dens <- density(fnl$value)
  df_dens <- data.frame(x = dens$x, y = dens$y)
  
  # Calculate quantiles for 5% and 95%
  ci <- quantile(data$cost, probs = c(0.05, 0.95))
  
  # Categorize the density data into sections: lower, middle, upper
  df_dens <- df_dens %>%
    mutate(
      section = case_when(
        x < ci[1] ~ "lower",
        x > ci[2] ~ "upper",
        TRUE ~ "middle"
      )
    )
  
  ## To make the graph 
  g.dns <- ggplot() +
    geom_area(data = df_dens %>% filter(section == "lower"), 
              aes(x = x, y = y), fill = "#fef8dd", alpha = 0.5) + # Lower tail
    geom_area(data = df_dens %>% filter(section == "upper"), 
              aes(x = x, y = y), fill = "#fef8dd", alpha = 0.5) + # Upper tail
    geom_area(data = df_dens %>% filter(section == "middle"), 
              aes(x = x, y = y), fill = "#e6e6c5", alpha = 0.8) + # Middle section
    geom_vline(xintercept = mean(fnl$value), linetype = "dotted", color = "black") + # Mean line
    ggtitle(label = crp) +
    annotate("text", x = mean(data$cost), y = max(df_dens$y) * 0.8, 
             label = paste0("Mean: ", round(mean(data$cost), 1), " Unt"), 
             hjust = 0, size = 4) + # Mean annotation
    annotate("segment", x = ci[1], xend = ci[2], y = -0.01, yend = -0.01, 
             arrow = arrow(length = unit(0.2, "cm")), color = "black") + # 90% CI line
    annotate("text", x = mean(ci), y = -0.02, 
             label = "90%", hjust = 0.5, size = 4) + # 90% CI annotation
    labs(
      x = "XXXXX", 
      y = "Density"
    ) +
    theme_minimal(base_size = 14) + 
    theme(
      text = element_text(family = 'Segoe UI'), 
      plot.title = element_text(face = 'bold', hjust = 0.5, size = 15)
    )
  
  ## To save the final maps
  ggsave(plot = g.dns, filename = glue('./png/density_{crp}.jpg'), units = 'in', width = 7, height = 5, dpi = 300, create.dir = TRUE)
  cat('Done!\n')

}

##
map(crps, make.graph)
