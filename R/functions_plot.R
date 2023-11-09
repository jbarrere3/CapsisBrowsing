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
              nudge_y = 0.08*diff(range(data_model$Rmax, na.rm = TRUE))) +
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
              nudge_y = 0.06*diff(range(data_model$t.half, na.rm = TRUE))) +
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
              nudge_y = 0.08*diff(range(data_model$Rmax, na.rm = TRUE))) +
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
              nudge_y = 0.06*diff(range(data_model$t.half, na.rm = TRUE))) +
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



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 3. Plots for the appendix ------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#' Plot browsing rates per browsing pressure
#' @param capsis_dir directory of capsis simulations
#' @param herbivory_table Table listing all herbivory scenarios
#' @param inventory_table
#' @param cmd_file
#' @param simulations_output
#' @param file.out
plot_br_rate = function(capsis_dir, herbivory_table, inventory_table, cmd_file, 
                        simulations_output, file.out){
  
  # Create output directory if needed
  create_dir_if_needed(file.out)
  
  # Extract the herbivore and inventory scenarios per simulation
  simulation.table <- read.table(cmd_file, col.names = c("invFile", "years", "herbFile"))
  simulation.table <- simulation.table[c(3:dim(simulation.table)[1]), ] %>%
    mutate(sim.number = c(1:dim(.)[1])) %>%
    left_join((herbivory_table %>% rename(herbFile = filename)), by = "herbFile") %>%
    left_join((inventory_table %>% rename(invFile = filename)), by = "invFile")
  
  # Calculate output variables
  for(i in 1:dim(simulation.table)[1]){
    if(floor(i/20) == i/20) print(paste0("Extracting results for simulation ", i, "/", dim(simulation.table)[1]))
    
    # Read the saplings output
    data.i <- read.table(
      simulations_output[grep(paste0("sim_", i, "_saplingExport"), simulations_output)], 
      col.names = c("year", "cell", "id", "species", "age", "diameter_mm", "height_cm", "browsed", "cleared")) %>%
      # Calculate quadratic diameter and n of seedling browsed per year
      mutate(diam2 = diameter_mm^2, 
             count = ifelse(!is.na(diam2), 1, 0)) %>%
      group_by(year) %>%
      summarize(dqm = sqrt(mean(diam2, na.rm = T)), 
                br_ha = sum(browsed, na.rm = T)*10000/(27^2), 
                sapling_ha = sum(count)*10000/(27^2))  %>%
      # Add information on the simulation
      mutate(sim.number = simulation.table$sim.number[i], 
             Br_kg_ha_year = simulation.table$saplingBrowsedBiomass_kg_ha_year[i], 
             D0 = simulation.table$saplingDensity.in[i])
    
    # Add to final dataframe
    if(i == 1) data = data.i
    else data = rbind(data, data.i)
    
  }
  
  
  # Restrict to 100% oak
  plot.out = data %>%
    filter(Br_kg_ha_year > 0 & year > 0) %>%
    mutate(br_m2 = br_ha/10000, 
           browsing = paste0(Br_kg_ha_year, " kg/ha/year"), 
           density.init = paste0(D0, " saplings/m2"), 
           br_prop = br_ha/sapling_ha) %>%
    left_join((simulation.table %>% dplyr::select(sim.number, sapling.in)), 
              by = "sim.number") %>%
    filter(sapling.in == "QUERCUS_ROBUR*1") %>%
    ggplot(aes(x = dqm, y = br_prop, color = browsing)) +
    geom_point() +
    facet_wrap(~ density.init) + 
    xlab("Sapling quadratic diameter in the plot (mm)") + 
    ylab("Proportion of saplings\n browsed (%)")
  
  
  ## - save the plot and return the name of the file
  ggsave(file.out, plot.out, width = 20, height = 8, units = "cm", dpi = 600)
  return(file.out)
  
  
}




