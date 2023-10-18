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


#' Function to get the number of clearing operations performed based on the frequancy
#' @param cl.freq numeric vector with the frequencies to extract
#' @param min.year simulation year from which to start performing clearing operations
#' @param max.year Last simulation year
get_number_from_frequency <- function(cl.freq, min.year = 5, max.year = 20){
  
  # Initialize output
  out <- c()
  
  # Loop on all frequencies
  for(i in 1:length(cl.freq)){
    # Number of clearing operations
    out.i <- 1
    
    # Counter of year
    k = min.year
    
    # If frequency is 0, number of operations is 0 as well
    if(cl.freq[i] == 0) out.i <- 0
    # Otherwise, while loop to count the operations
    else{
      while((k + cl.freq[i]) <= max.year){
        out.i <- out.i + 1
        k = k + cl.freq[i]
      }
    }
    
    # Add to the output
    out <- c(out, out.i)
  }
  
  # Return output
  return(out)
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 2. Plots for manuscript ------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






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
          legend.position = "none", 
          axis.text = element_text(size = 14), 
          axis.title = element_text(size = 17)) + 
    ylim(0, 1)
  # --- Plot of height growth
  plot.growth <- data %>%
    mutate(PACL = paste0("PACL = ", PACL, "%")) %>%
    ggplot(aes(x = H0, y = iH, group = interaction(species, PACL), 
               color = species, linetype = PACL)) + 
    geom_line() + 
    xlab("Initial height (cm)") + ylab("Annual height\ngrowth (cm)") + 
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_color_manual(values = c("#95B5D2", "#52B788", "#2D6A4F")) + 
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          panel.grid = element_blank(), 
          legend.title = element_blank(), 
          legend.key = element_blank(), 
          legend.text = element_text(size = 14), 
          axis.text = element_text(size = 14), 
          axis.title = element_text(size = 17))
  # --- Final plot
  plot.out <- cowplot::plot_grid((ggplot() + theme_void()), plot.browsing, 
                                 (ggplot() + theme_void()), plot.growth, 
                                 rel_widths = c(0.12, 1, 0.12, 1.57), 
                                 labels = c("(a)", "", "(b)", ""), nrow = 1)
  
  ## - save the plot
  ggsave(file.in, plot.out, width = 24, height = 8, units = "cm", dpi = 600)
  return(file.in)
}


#' Plot the metrics extracted using the data
#' @param simulation_output_formatted dataframe containing output of simulations
#' @param selected.lines simulation number to extract for the plot (one needed)
#' @param file.in Name of the file to save (including path)
plot_metrics <- function(simulation_output_formatted, selected.lines = 47, file.in){
  
  # Create directory if needed
  create_dir_if_needed(file.in)
  
  # Format the data
  data <- simulation_output_formatted %>%
    filter(sim.number == selected.lines) %>%
    dplyr::select("density" = "saplingDensity.in", paste0("Y", c(1:20))) %>%
    gather(key = "time", value = "value", paste0("Y", c(1:20))) %>%
    mutate(R = value/(27^2), 
           time = as.numeric(gsub("Y", "", time)))
  
  # Calculate max recruitment
  Rmax <- max(data$R)
  
  # Calculate thalf
  t.before <- max((data %>% filter(R < Rmax/2))$time)
  R.before <- max((data %>% filter(R < Rmax/2))$R)
  R.after <- min((data %>% filter(R > Rmax/2))$R)
  t.half <- t.before + abs((Rmax/2 - R.before)/(R.after - R.before))
  
  # Plot metrics
  plot.out <- data %>%
    ggplot(aes(x = time, y = R, group = 1)) + 
    geom_line() + 
    # Rmax horizontal
    geom_segment(x = 0, xend = 20, y = Rmax, yend = Rmax, 
                 linetype = "dashed", color = "#E63946") +
    geom_text(data = data.frame(time = 1, R = Rmax*1.05), hjust = 0, 
              color = "#E63946", label = "DRmax", size = 6) + 
    # Rmax/2 horizontal
    geom_segment(x = 0, xend = t.half, y = Rmax/2, yend = Rmax/2, 
                 linetype = "dashed", color = "#457B9D") +
    geom_text(data = data.frame(time = 1, R = Rmax*1.1/2), hjust = 0,
              color = "#457B9D", label = "DRmax/2", size = 6) + 
    # Thalf vertical
    geom_segment(x = t.half, xend = t.half, y = 0, yend = Rmax/2, 
                 linetype = "dashed", color = "#457B9D") +
    geom_text(data = data.frame(time = t.half*1.05, R = Rmax*0.05), hjust = 0, 
              color = "#457B9D", label = "Thalf", size = 6) + 
    # Axis label
    xlab("Time (year)") + ylab("Oak recruitment \n(saplings/m2)") +
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          panel.grid = element_blank(), 
          axis.text = element_text(size = 14), 
          axis.title = element_text(size = 17))
  
  ## - save the plot and return the name of the file
  ggsave(file.in, plot.out, width = 12, height = 10, units = "cm", dpi = 600)
  return(file.in)
  
}


#' Plot the effect of browsing, species and density on max recruitment and recruitment speed
#' @param simulation_output_formatted output of the simulations formatted
#' @param file.in Name and location of the file to save
plot_H3 <- function(simulation_output_formatted, file.in){
  
  # Create directory if needed
  create_dir_if_needed(file.in)
  
  ## - Format the data
  data <- simulation_output_formatted %>%
    # Format species composition variable
    mutate(clearing.freq = as.numeric(gsub("freq", "", gsub("\\..+", "", clearing_scenario))), 
           cleared.species = gsub("\\-", " and ", gsub(".+\\.", "", clearing_scenario)), 
           cleared.species = gsub("FAGUS_SYLVATICA", "beech", cleared.species), 
           cleared.species = gsub("CARPINUS_BETULUS", "hornbeam", cleared.species), 
           cleared.species = ifelse(clearing.freq == 0, "no clearing", cleared.species)) %>%
    # Rename browsing and recruitment
    rename(browsing = saplingBrowsedBiomass_kg_ha_year, density0 = saplingDensity.in) %>%
    dplyr::select("cleared.species", "browsing", "density0", "sim.number", paste0("Y", c(1:20))) %>%
    # gather each year
    tidyr::gather(key = "year", value = "recruitment", paste0("Y", c(1:20))) %>%
    # Adapt recruitment depending on species composition (and convert from ha to m2)
    mutate(recruitment = recruitment/(27^2)) %>%
    # Format year
    mutate(year = as.numeric(gsub("Y", "", year))) %>%
    # Calculate max recruitment
    group_by(browsing, cleared.species, density0, sim.number) %>%
    mutate(Rmax = max(recruitment)) %>%
    # Determine if recruitment is before or after rmax/2
    mutate(positionRmax2 = ifelse(recruitment <= Rmax/2, "before", "after")) %>%
    group_by(browsing, cleared.species, density0, sim.number, positionRmax2) %>%
    mutate(year.type = case_when(positionRmax2 == "before" & recruitment == max(recruitment) ~ "Ra", 
                                 positionRmax2 == "after" & recruitment == min(recruitment) ~ "Rb", 
                                 TRUE ~ "Rc")) %>%
    filter(year.type %in% c("Ra", "Rb")) %>%
    ungroup() %>%
    group_by(browsing, cleared.species, density0, sim.number) %>%
    mutate(year.a = min(year)) %>%
    ungroup() %>%
    dplyr::select("cleared.species", "browsing", "density0", "sim.number", "Rmax", "year.a", "year.type", "recruitment") %>%
    distinct() %>%
    pivot_wider(id_cols = c("cleared.species", "browsing", "density0", "sim.number", "Rmax", "year.a"), 
                names_from = "year.type", values_from = "recruitment") %>%
    # Calculate time to reach half of Rmax
    mutate(t.half = ifelse(is.na(Rb), NA_real_, year.a + (0.5*Rmax - Ra)/(Rb - Ra))) %>%
    # Finish formatting
    dplyr::select(browsing, cleared.species, density0, sim.number, Rmax, t.half) 
  
  # Initialize final list of plots
  plots.out <- list()
  
  # Loop on each density value to fit model
  for(j in 1:length(unique(data$density0))){
    # Density value j
    density.j <- unique(data$density0)[j]
    
    ## - Plot for Rmax
    
    # Fit a simple model 
    model.rmax.j <- aov(log(Rmax + 0.001) ~ factor(browsing)*cleared.species, data = subset(data, density0 == density.j))
    
    # Plot statistics for rmax model j
    plot.stat.rmax.j <- data.frame(variable = c("Br", "Cl", "Br:Cl"), 
                                   Fval = summary(model.rmax.j)[[1]][c(1:3), 4]) %>%
      mutate(Fval.rel = Fval/sum(Fval)*100) %>%
      ggplot(aes(x = variable, y = Fval.rel)) + 
      geom_bar(color = "grey", fill = "black", stat = "identity") + 
      xlab("") + ylab("Relative F value (%)") + 
      theme(panel.background = element_rect(fill = "white", color = "black"), 
            panel.grid = element_blank()) + 
      ylim(0, 100) + 
      coord_flip()
    
    # Plot rmax value for density j
    plot.rmax.value.j <- data %>%
      filter(density0 == density.j) %>%
      group_by(browsing, cleared.species) %>%
      summarize(Rmax.mean = mean(Rmax, na.rm = TRUE), 
                Rmax.sd = sd(Rmax, na.rm = TRUE), 
                Rmax.low = quantile(Rmax, 0.025, na.rm = TRUE), 
                Rmax.high = quantile(Rmax, 0.975, na.rm = TRUE)) %>%
      mutate(browsing.pos = case_when(cleared.species == "hornbeam" ~ browsing - 6, 
                                      cleared.species == "hornbeam and beech" ~ browsing - 2, 
                                      cleared.species == "beech" ~ browsing + 2, 
                                      cleared.species == "no clearing" ~ browsing + 6),
             cleared.species = factor(cleared.species, levels = c("hornbeam", "hornbeam and beech", 
                                                                  "beech", "no clearing"))) %>%
      ggplot(aes(x = browsing.pos, y = Rmax.mean, fill = cleared.species, group = cleared.species)) + 
      geom_errorbar(aes(ymin = Rmax.low, ymax = Rmax.high), width = 0) + 
      geom_line(aes(color = cleared.species)) +
      geom_point(size = 2, shape = 21, color = "black") +
      scale_fill_manual(values = c("#90BE6D", "#F9C74F", "#F8961E", "#93B1A7")) +
      scale_color_manual(values = c("#90BE6D", "#F9C74F", "#F8961E", "#93B1A7")) +
      xlab("Biomass browsed (kg.ha.year)") + ylab("Rmax \n(saplings/m2)") + 
      scale_x_continuous(breaks = unique(data$browsing)) +
      ggtitle(paste0(density.j, " saplings/m2")) +
      theme(panel.background = element_rect(fill = "white", color = "black"), 
            panel.grid = element_blank(), 
            legend.position = "none")
    
    # Final plot j
    plot.rmax.j <- plot_grid(plot.rmax.value.j, plot.stat.rmax.j, rel_heights = c(1, 0.5), align = "v", ncol = 1)
    
    ## Plot for t.half
    
    # Only if there is enough data
    if(!(0 %in% (subset(data, density0 == density.j) %>%
                 group_by(browsing) %>%
                 summarize(mean = mean(Rmax)))$mean)){
      # Fit a simple model 
      model.t.half.j <- aov(log(t.half + 0.001) ~ factor(browsing)*cleared.species, data = subset(data, density0 == density.j))
      
      # Plot statistics for t.half model j
      plot.stat.t.half.j <- data.frame(variable = c("Br", "Cl", "Br:Cl"), 
                                       Fval = summary(model.t.half.j)[[1]][c(1:3), 4]) %>%
        mutate(Fval.rel = Fval/sum(Fval)*100) %>%
        ggplot(aes(x = variable, y = Fval.rel)) + 
        geom_bar(color = "grey", fill = "black", stat = "identity") + 
        xlab("") + ylab("Relative F value (%)") + 
        theme(panel.background = element_rect(fill = "white", color = "black"), 
              panel.grid = element_blank()) + 
        ylim(0, 100) + 
        coord_flip()
      
      # Plot t.half value for density j
      plot.t.half.value.j <- data %>%
        filter(density0 == density.j) %>%
        group_by(browsing, cleared.species) %>%
        summarize(t.half.mean = mean(t.half, na.rm = TRUE), 
                  t.half.sd = sd(t.half, na.rm = TRUE), 
                  t.half.low = quantile(t.half, 0.025, na.rm = TRUE), 
                  t.half.high = quantile(t.half, 0.975, na.rm = TRUE)) %>%
        mutate(browsing.pos = case_when(cleared.species == "hornbeam" ~ browsing - 6, 
                                        cleared.species == "hornbeam and beech" ~ browsing - 2, 
                                        cleared.species == "beech" ~ browsing + 2, 
                                        cleared.species == "no clearing" ~ browsing + 6),
               cleared.species = factor(cleared.species, levels = c("hornbeam", "hornbeam and beech", 
                                                                    "beech", "no clearing"))) %>%
        ggplot(aes(x = browsing.pos, y = t.half.mean, fill = cleared.species, group = cleared.species)) + 
        geom_errorbar(aes(ymin = t.half.low, ymax = t.half.high), width = 0) + 
        geom_line(aes(color = cleared.species)) +
        geom_point(size = 2, shape = 21, color = "black") +
        scale_fill_manual(values = c("#90BE6D", "#F9C74F", "#F8961E", "#93B1A7")) +
        scale_color_manual(values = c("#90BE6D", "#F9C74F", "#F8961E", "#93B1A7")) +
        xlab("Biomass browsed (kg.ha.year)") + ylab("Thalf \n (year)") + 
        scale_x_continuous(breaks = unique(data$browsing)) +
        ggtitle(paste0(density.j, " saplings/m2")) +
        theme(panel.background = element_rect(fill = "white", color = "black"), 
              panel.grid = element_blank(), 
              legend.position = "none")
      
      # Final plot j
      plot.t.half.j <- plot_grid(plot.t.half.value.j, plot.stat.t.half.j, rel_heights = c(1, 0.5), align = "v", ncol = 1)
    } else {plot.t.half.j <- ggplot() + theme_void()}
    
    plot.j <- plot_grid(plot.rmax.j, (ggplot + theme_void()), plot.t.half.j, nrow = 1, rel_widths = c(1, 0.2, 1), align = "")
    
    
    # Add to the final plot list
    eval(parse(text = paste0("plots.out$density", density.j, " <- plot.j")))
    
  }
  
  # Final plot
  plot.out <- 
    plot_grid(plot_grid(plotlist = plots.out, ncol = 1, labels = paste0("(", letters[c(1:length(unique(data$density0)))], ")"), scale = 0.9), 
              get_legend(plot.rmax.value.j + theme(legend.position = "left", legend.key = element_blank(), legend.title = element_blank())), 
              nrow = 1, rel_widths = c(1, 0.3))
  
  
  ## - save the plot and return the name of the file
  ggsave(file.in, plot.out, width = 20, height = 20, units = "cm", dpi = 600)
  return(file.in)
  
}

#' Plot the effect of browsing, species and density on max recruitment and recruitment speed
#' @param simulation_output_formatted output of the simulations formatted
#' @param file.in Name and location of the file to save
plot_H4 <- function(simulation_output_formatted, density0.in = 30, file.in){
  
  # Create directory if needed
  create_dir_if_needed(file.in)
  
  ## - Format the data
  data <- simulation_output_formatted %>%
    # Format species composition variable
    mutate(sp.composition = case_when(
      sapling.in == "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.5" ~ "50% beech",
      sapling.in == "QUERCUS_ROBUR*0.5-CARPINUS_BETULUS*0.5" ~ "50% hornbeam",
      sapling.in == "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.25-CARPINUS_BETULUS*0.25" ~ "25% beech - 25% hornbeam", 
      TRUE ~ "100% Oak")) %>%
    # Rename browsing and recruitment
    rename(browsing = saplingBrowsedBiomass_kg_ha_year, density0 = saplingDensity.in) %>%
    dplyr::select("sp.composition", "browsing", "density0", "sim.number", paste0("Y", c(1:20))) %>%
    # gather each year
    tidyr::gather(key = "year", value = "recruitment", paste0("Y", c(1:20))) %>%
    # Adapt recruitment depending on species composition (and convert from ha to m2)
    #mutate(recruitment = recruitment/(27^2)) %>%
    mutate(recruitment = ifelse(sp.composition == "100% Oak", recruitment/(2*(27^2)), recruitment/(27^2))) %>%
    # Format year
    mutate(year = as.numeric(gsub("Y", "", year))) %>%
    # Calculate max recruitment
    group_by(browsing, sp.composition, density0, sim.number) %>%
    mutate(Rmax = max(recruitment)) %>%
    # Determine if recruitment is before or after rmax/2
    mutate(positionRmax2 = ifelse(recruitment <= Rmax/2, "before", "after")) %>%
    group_by(browsing, sp.composition, density0, sim.number, positionRmax2) %>%
    mutate(year.type = case_when(positionRmax2 == "before" & recruitment == max(recruitment) ~ "Ra", 
                                 positionRmax2 == "after" & recruitment == min(recruitment) ~ "Rb", 
                                 TRUE ~ "Rc")) %>%
    filter(year.type %in% c("Ra", "Rb")) %>%
    ungroup() %>%
    group_by(browsing, sp.composition, density0, sim.number) %>%
    mutate(year.a = min(year)) %>%
    ungroup() %>%
    dplyr::select("sp.composition", "browsing", "density0", "sim.number", "Rmax", "year.a", "year.type", "recruitment") %>%
    distinct() %>%
    pivot_wider(id_cols = c("sp.composition", "browsing", "density0", "sim.number", "Rmax", "year.a"), 
                names_from = "year.type", values_from = "recruitment") %>%
    # Calculate time to reach half of Rmax
    mutate(t.half = ifelse(is.na(Rb), NA_real_, year.a + (0.5*Rmax - Ra)/(Rb - Ra))) %>%
    # Finish formatting
    dplyr::select(browsing, sp.composition, density0, sim.number, Rmax, t.half) 
  
  # Initialiaze final list of plots
  plots.out <- list()
  
  # Loop on each density value to fit model
  for(j in 1:length(unique(data$density0))){
    # Density value j
    density.j <- unique(data$density0)[j]
    
    ## - Plot for Rmax
    
    # Fit a simple model 
    model.rmax.j <- aov(log(Rmax + 0.001) ~ factor(browsing)*sp.composition, data = subset(data, density0 == density.j))
    
    # Plot statistics for rmax model j
    plot.stat.rmax.j <- data.frame(variable = c("Br", "Sp", "Br:Sp"), 
                                   Fval = summary(model.rmax.j)[[1]][c(1:3), 4]) %>%
      mutate(Fval.rel = Fval/sum(Fval)*100) %>%
      ggplot(aes(x = variable, y = Fval.rel)) + 
      geom_bar(color = "grey", fill = "black", stat = "identity") + 
      xlab("") + ylab("Relative F value (%)") + 
      theme(panel.background = element_rect(fill = "white", color = "black"), 
            panel.grid = element_blank()) + 
      ylim(0, 100) + 
      coord_flip()
    
    # Plot rmax value for density j
    plot.rmax.value.j <- data %>%
      filter(density0 == density.j) %>%
      group_by(browsing, sp.composition) %>%
      summarize(Rmax.mean = mean(Rmax, na.rm = TRUE), 
                Rmax.sd = sd(Rmax, na.rm = TRUE), 
                Rmax.low = quantile(Rmax, 0.025, na.rm = TRUE), 
                Rmax.high = quantile(Rmax, 0.975, na.rm = TRUE)) %>%
      mutate(browsing.pos = case_when(sp.composition == "50% hornbeam" ~ browsing - 6, 
                                      sp.composition == "25% beech - 25% hornbeam" ~ browsing - 2, 
                                      sp.composition == "50% beech" ~ browsing + 2, 
                                      sp.composition == "100% Oak" ~ browsing + 6),
             sp.composition = factor(sp.composition, levels = c("50% hornbeam", "25% beech - 25% hornbeam", 
                                                                "50% beech", "100% Oak"))) %>%
      ggplot(aes(x = browsing.pos, y = Rmax.mean, fill = sp.composition, group = sp.composition)) + 
      geom_errorbar(aes(ymin = Rmax.low, ymax = Rmax.high), width = 0) + 
      geom_line(aes(color = sp.composition)) +
      geom_point(size = 2, shape = 21, color = "black") +
      scale_fill_manual(values = c("#90BE6D", "#F9C74F", "#F8961E", "#93B1A7")) +
      scale_color_manual(values = c("#90BE6D", "#F9C74F", "#F8961E", "#93B1A7")) +
      xlab("Biomass browsed (kg.ha.year)") + ylab("Rmax \n(saplings/m2)") + 
      scale_x_continuous(breaks = unique(data$browsing)) +
      ggtitle(paste0(density.j, " saplings/m2")) +
      theme(panel.background = element_rect(fill = "white", color = "black"), 
            panel.grid = element_blank(), 
            legend.position = "none")
    
    # Final plot j
    plot.rmax.j <- plot_grid(plot.rmax.value.j, plot.stat.rmax.j, rel_heights = c(1, 0.5), align = "v", ncol = 1)
    
    ## Plot for t.half
    
    # Only if there is enough data
    if(!(0 %in% (subset(data, density0 == density.j) %>%
                 group_by(browsing) %>%
                 summarize(mean = mean(Rmax)))$mean)){
      # Fit a simple model 
      model.t.half.j <- aov(log(t.half + 0.001) ~ factor(browsing)*sp.composition, data = subset(data, density0 == density.j))
      
      # Plot statistics for t.half model j
      plot.stat.t.half.j <- data.frame(variable = c("Br", "Sp", "Br:Sp"), 
                                       Fval = summary(model.t.half.j)[[1]][c(1:3), 4]) %>%
        mutate(Fval.rel = Fval/sum(Fval)*100) %>%
        ggplot(aes(x = variable, y = Fval.rel)) + 
        geom_bar(color = "grey", fill = "black", stat = "identity") + 
        xlab("") + ylab("Relative F value (%)") + 
        theme(panel.background = element_rect(fill = "white", color = "black"), 
              panel.grid = element_blank()) + 
        ylim(0, 100) + 
        coord_flip()
      
      # Plot t.half value for density j
      plot.t.half.value.j <- data %>%
        filter(density0 == density.j) %>%
        group_by(browsing, sp.composition) %>%
        summarize(t.half.mean = mean(t.half, na.rm = TRUE), 
                  t.half.sd = sd(t.half, na.rm = TRUE), 
                  t.half.low = quantile(t.half, 0.025, na.rm = TRUE), 
                  t.half.high = quantile(t.half, 0.975, na.rm = TRUE)) %>%
        mutate(browsing.pos = case_when(sp.composition == "50% hornbeam" ~ browsing - 6, 
                                        sp.composition == "25% beech - 25% hornbeam" ~ browsing - 2, 
                                        sp.composition == "50% beech" ~ browsing + 2, 
                                        sp.composition == "100% Oak" ~ browsing + 6),
               sp.composition = factor(sp.composition, levels = c("50% hornbeam", "25% beech - 25% hornbeam", 
                                                                  "50% beech", "100% Oak"))) %>%
        ggplot(aes(x = browsing.pos, y = t.half.mean, fill = sp.composition, group = sp.composition)) + 
        geom_errorbar(aes(ymin = t.half.low, ymax = t.half.high), width = 0) + 
        geom_line(aes(color = sp.composition)) +
        geom_point(size = 2, shape = 21, color = "black") +
        scale_fill_manual(values = c("#90BE6D", "#F9C74F", "#F8961E", "#93B1A7")) +
        scale_color_manual(values = c("#90BE6D", "#F9C74F", "#F8961E", "#93B1A7")) +
        xlab("Biomass browsed (kg.ha.year)") + ylab("Thalf \n (year)") + 
        scale_x_continuous(breaks = unique(data$browsing)) +
        ggtitle(paste0(density.j, " saplings/m2")) +
        theme(panel.background = element_rect(fill = "white", color = "black"), 
              panel.grid = element_blank(), 
              legend.position = "none")
      
      # Final plot j
      plot.t.half.j <- plot_grid(plot.t.half.value.j, plot.stat.t.half.j, rel_heights = c(1, 0.5), align = "v", ncol = 1)
    } else {plot.t.half.j <- ggplot() + theme_void()}
    
    plot.j <- plot_grid(plot.rmax.j, (ggplot + theme_void()), plot.t.half.j, nrow = 1, rel_widths = c(1, 0.2, 1), align = "")
    
    
    # Add to the final plot list
    eval(parse(text = paste0("plots.out$density", density.j, " <- plot.j")))
    
  }
  
  # Final plot
  plot.out <- 
    plot_grid(plot_grid(plotlist = plots.out, ncol = 1, labels = paste0("(", letters[c(1:length(unique(data$density0)))], ")"), scale = 0.9), 
              get_legend(plot.rmax.value.j + theme(legend.position = "left", legend.key = element_blank(), legend.title = element_blank())), 
              nrow = 1, rel_widths = c(1, 0.3))
  
  
  ## - save the plot and return the name of the file
  ggsave(file.in, plot.out, width = 20, height = 20, units = "cm", dpi = 600)
  return(file.in)
  
}

#' Plot the effect of browsing, species and density on max recruitment and recruitment speed
#' @param data_model output of the simulations formatted to fit model
#' @param file.in Name and location of the file to save
plot_H3_simple <- function(data_model, file.in){
  
  # Create directory if needed
  create_dir_if_needed(file.in)
  
  # Plot rmax value
  plot.rmax <- data_model %>%
    group_by(density0, browsing, cleared.species) %>%
    summarize(Rmax.mean = mean(Rmax, na.rm = TRUE), 
              Rmax.sd = sd(Rmax, na.rm = TRUE), 
              Rmax.low = quantile(Rmax, 0.025, na.rm = TRUE), 
              Rmax.high = quantile(Rmax, 0.975, na.rm = TRUE)) %>%
    mutate(browsing.pos = case_when(cleared.species == "hornbeam" ~ browsing - 6, 
                                    cleared.species == "hornbeam and beech" ~ browsing - 2, 
                                    cleared.species == "beech" ~ browsing + 2, 
                                    cleared.species == "no clearing" ~ browsing + 6),
           cleared.species = factor(cleared.species, levels = c("hornbeam", "hornbeam and beech", 
                                                                "beech", "no clearing")), 
           density = paste0(density0, " saplings/m2")) %>%
    # Add significance symbol
    left_join(get_signif(data_model, "rmax"), by = c("density0", "browsing")) %>%
    ungroup() %>% group_by(density0, browsing) %>%
    mutate(text.pos.y = max(Rmax.high)) %>%
    ggplot(aes(x = browsing.pos, y = Rmax.mean, fill = cleared.species, group = cleared.species)) + 
    geom_errorbar(aes(ymin = Rmax.low, ymax = Rmax.high), width = 0) + 
    geom_line(aes(color = cleared.species), show.legend = FALSE) +
    geom_point(size = 1, shape = 21, color = "black") +
    scale_fill_manual(values = c("#01BAEF", "#0B4F6C", "#20BF55", "#757575")) +
    scale_color_manual(values = c("#01BAEF", "#0B4F6C", "#20BF55", "#757575")) +
    xlab("Biomass browsed (kg.ha.year)") + ylab("DRmax \n(saplings/m2)") + 
    geom_text(aes(x = browsing, y = text.pos.y, label = signif), 
              nudge_y = 0.05*diff(range(data_model$Rmax, na.rm = TRUE))) +
    scale_x_continuous(breaks = unique(data_model$browsing)) +
    facet_wrap(~ density, nrow = 1) + 
    guides(fill=guide_legend(title="Species cleared", 
                             nrow=2,byrow=TRUE)) +
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          panel.grid = element_blank(), 
          legend.position = "none", 
          strip.background = element_blank(), 
          strip.text = element_text(face = "bold"))
  
  
  # Plot thalf value
  plot.t.half <- data_model %>%
    filter(Rmax > 0.0005) %>%
    group_by(browsing, cleared.species, density0) %>%
    summarize(t.half.mean = mean(t.half, na.rm = TRUE), 
              t.half.sd = sd(t.half, na.rm = TRUE), 
              t.half.low = quantile(t.half, 0.025, na.rm = TRUE), 
              t.half.high = quantile(t.half, 0.975, na.rm = TRUE), 
              n = length(which(!is.na(t.half)))) %>%
    mutate(browsing.pos = case_when(cleared.species == "hornbeam" ~ browsing - 6, 
                                    cleared.species == "hornbeam and beech" ~ browsing - 2, 
                                    cleared.species == "beech" ~ browsing + 2, 
                                    cleared.species == "no clearing" ~ browsing + 6),
           cleared.species = factor(cleared.species, levels = c("hornbeam", "hornbeam and beech", 
                                                                "beech", "no clearing")), 
           density = paste0(density0, " saplings/m2"), 
           density = factor(density, levels = paste0(c(10, 30, 50), " saplings/m2"))) %>%
    filter(n >= 10) %>%
    # Add significance symbol
    left_join(get_signif(data_model, "thalf"), by = c("density0", "browsing")) %>%
    ungroup() %>% group_by(density0, browsing) %>%
    mutate(text.pos.y = max(t.half.high)) %>%
    ggplot(aes(x = browsing.pos, y = t.half.mean, fill = cleared.species, group = cleared.species)) + 
    geom_errorbar(aes(ymin = t.half.low, ymax = t.half.high), width = 0) + 
    geom_line(aes(color = cleared.species)) +
    geom_point(size = 1, shape = 21, color = "black") +
    geom_text(aes(x = browsing, y = text.pos.y, label = signif), 
              nudge_y = 0.05*diff(range(data_model$t.half, na.rm = TRUE))) +
    scale_fill_manual(values = c("#01BAEF", "#0B4F6C", "#20BF55", "#757575")) +
    scale_color_manual(values = c("#01BAEF", "#0B4F6C", "#20BF55", "#757575")) +
    xlab("Biomass browsed (kg.ha.year)") + ylab("Thalf \n (year)") + 
    scale_x_continuous(breaks = unique(data_model$browsing)) +
    facet_wrap(~ density, nrow = 1, drop = FALSE) +
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          panel.grid = element_blank(), 
          legend.position = "none", 
          strip.background = element_blank(), 
          strip.text = element_text(face = "bold"))
  
  
  
  # Final plot
  plot.out <- 
    plot_grid(plot_grid(plot.rmax, plot.t.half, ncol = 1, scale = 0.9, 
                        labels = c("(a)", "(b)"), align = "v"), get_legend(
                          plot.rmax + theme(legend.position = "top", legend.key = element_blank())), 
              rel_heights = c(1, 0.15), ncol = 1, scale = c(1, 0.7))
  
  
  ## - save the plot and return the name of the file
  ggsave(file.in, plot.out, width = 15, height = 13, units = "cm", dpi = 600)
  return(file.in)
  
}

#' Plot the effect of browsing, species and density on max recruitment and recruitment speed
#' @param data_model output of the simulations formatted
#' @param file.in Name and location of the file to save
plot_H4_simple <- function(data_model, file.in){
  
  # Create directory if needed
  create_dir_if_needed(file.in)
  
  # Plot rmax value
  plot.rmax <- data_model %>%
    group_by(density0, browsing, sp.composition) %>%
    summarize(Rmax.mean = mean(Rmax, na.rm = TRUE), 
              Rmax.sd = sd(Rmax, na.rm = TRUE), 
              Rmax.low = quantile(Rmax, 0.025, na.rm = TRUE), 
              Rmax.high = quantile(Rmax, 0.975, na.rm = TRUE)) %>%
    mutate(browsing.pos = case_when(sp.composition == "50% hornbeam" ~ browsing - 6, 
                                    sp.composition == "25% beech - 25% hornbeam" ~ browsing - 2, 
                                    sp.composition == "50% beech" ~ browsing + 2, 
                                    sp.composition == "100% Oak" ~ browsing + 6),
           sp.composition = factor(sp.composition, levels = c("50% hornbeam", "25% beech - 25% hornbeam", 
                                                              "50% beech", "100% Oak")), 
           density = paste0(density0, " saplings/m2")) %>%
    # Add significance symbol
    left_join(get_signif(data_model, "rmax"), by = c("density0", "browsing")) %>%
    ungroup() %>% group_by(density0, browsing) %>%
    mutate(text.pos.y = max(Rmax.high)) %>%
    ggplot(aes(x = browsing.pos, y = Rmax.mean, fill = sp.composition, group = sp.composition)) + 
    geom_errorbar(aes(ymin = Rmax.low, ymax = Rmax.high), width = 0) + 
    geom_line(aes(color = sp.composition), show.legend = FALSE) +
    geom_point(size = 1, shape = 21, color = "black") +
    geom_text(aes(x = browsing, y = text.pos.y, label = signif), 
              nudge_y = 0.05*diff(range(data_model$Rmax, na.rm = TRUE))) +
    scale_fill_manual(values = c("#81B29A", "#F2CC8F", "#E07A5F", "#3D405B")) +
    scale_color_manual(values = c("#81B29A", "#F2CC8F", "#E07A5F", "#3D405B")) +
    xlab("Biomass browsed (kg.ha.year)") + ylab("DRmax \n(saplings/m2)") + 
    scale_x_continuous(breaks = unique(data_model$browsing)) +
    facet_wrap(~ density, nrow = 1) + 
    guides(fill=guide_legend(title="Species\ncomposition", 
                             nrow=2,byrow=TRUE)) +
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          panel.grid = element_blank(), 
          legend.position = "none", 
          strip.background = element_blank(), 
          strip.text = element_text(face = "bold"))
  
  
  # Plot thalf value
  plot.t.half <- data_model %>%
    filter(Rmax > 0.0005) %>%
    group_by(browsing, sp.composition, density0) %>%
    summarize(t.half.mean = mean(t.half, na.rm = TRUE), 
              t.half.sd = sd(t.half, na.rm = TRUE), 
              t.half.low = quantile(t.half, 0.025, na.rm = TRUE), 
              t.half.high = quantile(t.half, 0.975, na.rm = TRUE), 
              n = length(which(!is.na(t.half)))) %>%
    mutate(browsing.pos = case_when(sp.composition == "50% hornbeam" ~ browsing - 6, 
                                    sp.composition == "25% beech - 25% hornbeam" ~ browsing - 2, 
                                    sp.composition == "50% beech" ~ browsing + 2, 
                                    sp.composition == "100% Oak" ~ browsing + 6),
           sp.composition = factor(sp.composition, levels = c("50% hornbeam", "25% beech - 25% hornbeam", 
                                                              "50% beech", "100% Oak")), 
           density = paste0(density0, " saplings/m2"), 
           density = factor(density, levels = paste0(c(10, 30, 50), " saplings/m2"))) %>%
    filter(n >= 10) %>%
    # Add significance symbol
    left_join(get_signif(data_model, "thalf"), by = c("density0", "browsing")) %>%
    ungroup() %>% group_by(density0, browsing) %>%
    mutate(text.pos.y = max(t.half.high)) %>%
    ggplot(aes(x = browsing.pos, y = t.half.mean, fill = sp.composition, group = sp.composition)) + 
    geom_errorbar(aes(ymin = t.half.low, ymax = t.half.high), width = 0) + 
    geom_line(aes(color = sp.composition)) +
    geom_point(size = 1, shape = 21, color = "black") +
    geom_text(aes(x = browsing, y = text.pos.y, label = signif), 
              nudge_y = 0.05*diff(range(data_model$t.half, na.rm = TRUE))) +
    scale_fill_manual(values = c("#81B29A", "#F2CC8F", "#E07A5F", "#3D405B")) +
    scale_color_manual(values = c("#81B29A", "#F2CC8F", "#E07A5F", "#3D405B")) +
    xlab("Biomass browsed (kg.ha.year)") + ylab("Thalf \n (year)") + 
    scale_x_continuous(breaks = unique(data_model$browsing)) +
    facet_wrap(~ density, nrow = 1, drop = FALSE) +
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          panel.grid = element_blank(), 
          legend.position = "none", 
          strip.background = element_blank(), 
          strip.text = element_text(face = "bold"))
  
  
  
  # Final plot
  plot.out <- 
    plot_grid(plot_grid(plot.rmax, plot.t.half, ncol = 1, scale = 0.9, 
                        labels = c("(a)", "(b)"), align = "v"), get_legend(
      plot.rmax + theme(legend.position = "top", legend.key = element_blank())), 
      rel_heights = c(1, 0.15), ncol = 1, scale = c(1, 0.7))
  
  
  ## - save the plot and return the name of the file
  ggsave(file.in, plot.out, width = 15, height = 13, units = "cm", dpi = 600)
  return(file.in)
  
}




