#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#### SCRIPT INTRODUCTION ####
#
#' @name functions_plot.R  
#' @description R script containing all functions relative to data
#               visualisation
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 1. Generic functions ------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#' Function to get the path of a file, and create directories if they don't exist
#' @param file.in character: path of the file, filename included (ex: "plot/plot.png")
create_dir_if_needed <- function(file.in){
  
  path.in <- strsplit(file.in, "/")[[1]]
  if(length(path.in) > 1){
    for(i in 1:(length(path.in)-1)){
      if(i == 1) path.in_i <- path.in[i]
      else path.in_i <- paste(path.in_i, path.in[i], sep = "/")
      if(!dir.exists(path.in_i)) dir.create(path.in_i)
    }
  }
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 2. exploratory plots full data ------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




#' Plot recruitment with density and browsing
#' @param simulation_output_formatted output of the simulations formatted
#' @param year.in Numeric indicating after how many years should we count recruits (10, 15 or 20)
#' @param file.in Name and location of the file to save
plot_recruitment_density_browsing <- function(simulation_output_formatted, year.in, file.in){
  
  
  ## - Create directories if needed
  create_dir_if_needed(file.in)
  
  ## - make the plot
  plot.out <- simulation_output_formatted %>%
    mutate(sp.composition = case_when(
      sapling.in == "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.5" ~ "50% beech",
      sapling.in == "QUERCUS_ROBUR*0.5-CARPINUS_BETULUS*0.5" ~ "50% hornbeam",
      sapling.in == "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.25-CARPINUS_BETULUS*0.25" ~ "25% beech - 25% hornbeam")) %>%
    rename(Browsing = saplingBrowsedBiomass_kg_ha_year) %>%
    rename("recruitment" = paste0("density.oak130.y", year.in, "_ha")) %>%
    ggplot(aes(x = saplingDensity.in, y = recruitment/10000, group = Browsing, color = Browsing)) + 
    geom_line() + 
    facet_wrap(~ sp.composition) + 
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          strip.background = element_blank(), 
          panel.grid = element_blank()) + 
    xlab("Initial sapling density (m-2)") + 
    ylab(paste0("Oak recruitment after ", year.in, " years (m-2)"))
  
  
  ## - save the plot
  ggsave(file.in, plot.out, width = 23, height = 8, units = "cm", dpi = 600)
  return(file.in)
  
}

#' Plot recruitment with density and browsing
#' @param simulation_output_formatted output of the simulations formatted
#' @param year.in Numeric indicating after how many years should we count recruits (10, 15 or 20)
#' @param file.in Name and location of the file to save
plot_recruitment_density_browsing2 <- function(simulation_output_formatted, year.in, file.in){
  
  
  ## - Create directories if needed
  create_dir_if_needed(file.in)
  
  ## - make the plot
  plot.out <- simulation_output_formatted %>%
    mutate(sp.composition = case_when(
      sapling.in == "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.5" ~ "50% beech",
      sapling.in == "QUERCUS_ROBUR*0.5-CARPINUS_BETULUS*0.5" ~ "50% hornbeam",
      sapling.in == "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.25-CARPINUS_BETULUS*0.25" ~ "25% beech - 25% hornbeam")) %>%
    rename("recruitment" = paste0("density.oak130.y", year.in, "_ha")) %>%
    mutate(recruitment_percent = (recruitment/10000)/(0.5*saplingDensity.in)*100) %>%
    ggplot(aes(x = saplingDensity.in, y = recruitment_percent, color = browsing.density_m2)) + 
    geom_point(size = 1) + 
    facet_wrap(~ sp.composition) + 
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          strip.background = element_blank(), 
          panel.grid = element_blank()) + 
    xlab("Initial sapling density (m-2)") + 
    ylab(paste0("Percentage of oak recruited after ", year.in, " years"))
  
  
  ## - save the plot
  ggsave(file.in, plot.out, width = 23, height = 8, units = "cm", dpi = 600)
  return(file.in)
  
}


#' Plot recruitment with density and browsing
#' @param simulation_output_formatted output of the simulations formatted
#' @param year.in Numeric indicating after how many years should we count recruits (10, 15 or 20)
#' @param file.in Name and location of the file to save
plot_recruitment_density_browsing3 <- function(simulation_output_formatted, year.in, file.in){
  
  
  ## - Create directories if needed
  create_dir_if_needed(file.in)
  
  ## - make the plot
  plot.out <- simulation_output_formatted %>%
    mutate(sp.composition = case_when(
      sapling.in == "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.5" ~ "50% beech",
      sapling.in == "QUERCUS_ROBUR*0.5-CARPINUS_BETULUS*0.5" ~ "50% hornbeam",
      sapling.in == "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.25-CARPINUS_BETULUS*0.25" ~ "25% beech - 25% hornbeam")) %>%
    rename("recruitment" = paste0("density.oak130.y", year.in, "_ha")) %>%
    mutate(recruitment_percent = (recruitment/10000)/(0.5*saplingDensity.in)*100, 
           browsing.density_m2 = round(browsing.density_m2, digits = 0)) %>%
    filter(browsing.density_m2 %in% c(0, 2, 5)) %>%
    mutate(browsing = paste0(browsing.density_m2, " saplings browsed / m2")) %>%
    ggplot(aes(x = saplingDensity.in, y = recruitment_percent, color = sp.composition)) + 
    geom_point(size = 1) + 
    facet_wrap(~ browsing) + 
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          strip.background = element_blank(), 
          panel.grid = element_blank()) + 
    xlab("Initial sapling density (m-2)") + 
    ylab(paste0("Percentage of oak recruited after ", year.in, " years"))
  
  
  ## - save the plot
  ggsave(file.in, plot.out, width = 23, height = 8, units = "cm", dpi = 600)
  return(file.in)
  
}



#' Plot recruitment with density and browsing
#' @param simulation_output_formatted output of the simulations formatted
#' @param year.in Numeric indicating after how many years should we count recruits (10, 15 or 20)
#' @param file.in Name and location of the file to save
plot_recruitment_clearing_browsing <- function(simulation_output_formatted, year.in, file.in){
  
  
  ## - Create directories if needed
  create_dir_if_needed(file.in)
  
  ## - make the plot
  plot.out <- simulation_output_formatted %>%
    mutate(sp.composition = case_when(
      sapling.in == "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.5" ~ "50% beech",
      sapling.in == "QUERCUS_ROBUR*0.5-CARPINUS_BETULUS*0.5" ~ "50% hornbeam",
      sapling.in == "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.25-CARPINUS_BETULUS*0.25" ~ "25% beech - 25% hornbeam"), 
      density = paste0(saplingDensity.in, " saplings / m2"), 
      clearing.frequency = as.numeric(gsub("freq\\_", "", clearing_scenario)), 
      clearings = case_when(clearing.frequency == 0 ~ paste0("0 clearings"), 
                            clearing.frequency > 0 ~ paste0(
                              floor(15/clearing.frequency), " clearings (every ",
                              clearing.frequency, " years)"))) %>%
    rename(Browsing = saplingBrowsedBiomass_kg_ha_year) %>%
    rename("recruitment" = paste0("density.oak130.y", year.in, "_ha")) %>%
    ggplot(aes(x = browsing.density_m2, y = recruitment/10000, group = clearings, color = clearings)) + 
    geom_line() + 
    facet_grid(density ~ sp.composition) + 
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          strip.background = element_blank(), 
          panel.grid = element_blank(), 
          legend.key = element_blank(), 
          legend.title = element_blank()) + 
    xlab("Density of saplings browsed (m-2)") + 
    ylab(paste0("Oak recruitment after ", year.in, " years (m-2)"))
  
  
  ## - save the plot
  ggsave(file.in, plot.out, width = 20, height = 9, units = "cm", dpi = 600)
  return(file.in)
  
}


#' Plot recruitment with density, browsing and clearing scenario
#' @param simulation_output_formatted output of the simulations formatted
#' @param year.in Numeric indicating after how many years should we count recruits (10, 15 or 20)
#' @param file.in Name and location of the file to save
plot_recruitment_clearing_browsing2 <- function(simulation_output_formatted, year.in, file.in){
  
  
  ## - Create directories if needed
  create_dir_if_needed(file.in)
  
  ## - make the plot
  plot.out <- simulation_output_formatted %>%
    mutate(density = paste0(saplingDensity.in, " saplings / m2"), 
           clearing.frequency = gsub("\\..+", "", clearing_scenario), 
           clearing.frequency = as.numeric(gsub("freq", "", clearing.frequency)), 
           clearings = case_when(clearing.frequency == 0 ~ paste0("0 clearings"), 
                                 clearing.frequency > 0 ~ paste0(
                                   floor(15/clearing.frequency), " clearings (every ",
                                   clearing.frequency, " years)")), 
           clearing_target = gsub(".+\\.", "", clearing_scenario), 
           clearing_target = gsub("\\-", " & ", clearing_target), 
           clearing_target = gsub("\\_", " ", clearing_target)) %>%
    rename(browsing = browsing.density_m2) %>%
    rename("recruitment" = paste0("density.oak130.y", year.in, "_ha")) %>%
    dplyr::select(recruitment, browsing, density, clearings, clearing_target) %>%
    ggplot(aes(x = browsing, y = recruitment/10000, group = clearings, color = clearings)) + 
    geom_line() + 
    facet_grid(density ~ clearing_target) + 
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          strip.background = element_blank(), 
          panel.grid = element_blank(), 
          legend.key = element_blank(), 
          legend.title = element_blank()) + 
    xlab("Density of saplings browsed (m-2)") + 
    ylab(paste0("Oak recruitment after ", year.in, " years (m-2)"))
  
  
  ## - save the plot
  ggsave(file.in, plot.out, width = 30, height = 13.5, units = "cm", dpi = 600)
  return(file.in)
  
}


#' Plot percentage of saplings recruited with density, browsing and clearing scenario
#' @param simulation_output_formatted output of the simulations formatted
#' @param year.in Numeric indicating after how many years should we count recruits (10, 15 or 20)
#' @param file.in Name and location of the file to save
plot_recruitment_clearing_browsing3 <- function(simulation_output_formatted, year.in, file.in){
  
  
  ## - Create directories if needed
  create_dir_if_needed(file.in)
  
  ## - make the plot
  plot.out <- simulation_output_formatted %>%
    mutate(density = paste0(saplingDensity.in, " saplings / m2"), 
           clearing.frequency = gsub("\\..+", "", clearing_scenario), 
           clearing.frequency = as.numeric(gsub("freq", "", clearing.frequency)), 
           clearings = case_when(clearing.frequency == 0 ~ paste0("0 clearings"), 
                                 clearing.frequency > 0 ~ paste0(
                                   floor(15/clearing.frequency), " clearings (every ",
                                   clearing.frequency, " years)")), 
           clearing_target = gsub(".+\\.", "", clearing_scenario), 
           clearing_target = gsub("\\-", " & ", clearing_target), 
           clearing_target = gsub("\\_", " ", clearing_target)) %>%
    rename(browsing = browsing.density_m2) %>%
    rename("recruitment" = paste0("density.oak130.y", year.in, "_ha")) %>%
    mutate(init.oak.prop = as.numeric(gsub("\\-.+", "", gsub("QUERCUS\\_ROBUR\\*", "", sapling.in))), 
           percent.sapling.recruited = ((recruitment/10000)/(saplingDensity.in*init.oak.prop))*100) %>%
    dplyr::select(percent.sapling.recruited, browsing, density, clearings, clearing_target) %>%
    ggplot(aes(x = browsing, y = percent.sapling.recruited, group = clearings, color = clearings)) + 
    geom_line() + 
    facet_grid(density ~ clearing_target) + 
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          strip.background = element_blank(), 
          panel.grid = element_blank(), 
          legend.key = element_blank(), 
          legend.title = element_blank()) + 
    xlab("Density of saplings browsed (m-2)") + 
    ylab(paste0("Percentage of oak recruited after ", year.in, " years (%)"))
  
  
  ## - save the plot
  ggsave(file.in, plot.out, width = 30, height = 13.5, units = "cm", dpi = 600)
  return(file.in)
  
}

#' Plot differences in growth and palatability between the three studied species
#' @param file.in Character: name and location of the file to save
plot_palatability_growth <- function(file.in){
  
  ## - Create directories if needed
  create_dir_if_needed(file.in)
  
  ## - make the plot
  # --- Initialize the dataset for the plot
  data <- expand.grid(species = c("oak", "beech", "hornbeam"), 
                      H0 = c(1:200), 
                      PACL = c(15, 30)) %>%
    # --- Add coeffients
    left_join(data.frame(species = c("oak", "beech", "hornbeam"), 
                         slope.br = c(0.00278, 0.00072, 0.00408), 
                         intercept.br = c(-0.02474, 0.03158, 0.07188), 
                         a.growth = c(2.059, 2.431, 3.646), 
                         b.growth = c(6.058, 3.964, 7.577)), 
              by = "species") %>%
    # --- Calculate browsing probability and height growth
    mutate(p.browsing = ifelse(H0 %in% c(20:130), (intercept.br + slope.br*H0), 0), 
           iH = (a.growth*sqrt(H0))/(1 + exp(1 - PACL/b.growth)))
  # --- Plot of browsing probability
  plot.browsing <- data %>%
    filter(PACL == unique(data$PACL)[1]) %>%
    ggplot(aes(x = H0, y = p.browsing, group = species, color = species)) + 
    geom_line() + 
    xlab("Initial height (cm)") + ylab("Browsing probability") + 
    scale_color_manual(values = c("#95B5D2", "#52B788", "#2D6A4F")) + 
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          panel.grid = element_blank(), 
          legend.position = "none") + 
    ylim(0, 1)
  # --- Plot of height growth
  plot.growth <- data %>%
    mutate(PACL = paste0("PACL = ", PACL, "%")) %>%
    ggplot(aes(x = H0, y = iH, group = interaction(species, PACL), 
               color = species, linetype = PACL)) + 
    geom_line() + 
    xlab("Initial height (cm)") + ylab("Annual height growth (cm)") + 
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_color_manual(values = c("#95B5D2", "#52B788", "#2D6A4F")) + 
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          panel.grid = element_blank(), 
          legend.title = element_blank(), 
          legend.key = element_blank())
  # --- Final plot
  plot.out <- cowplot::plot_grid((ggplot() + theme_void()), plot.browsing, 
                                 (ggplot() + theme_void()), plot.growth, 
                                 rel_widths = c(0.12, 1, 0.12, 1.4), 
                                 labels = c("(a)", "", "(b)", ""), nrow = 1)
  
  ## - save the plot
  ggsave(file.in, plot.out, width = 22, height = 8, units = "cm", dpi = 600)
  return(file.in)
}

