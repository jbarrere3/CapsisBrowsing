# _targets.R file
library(targets)
lapply(grep("R$", list.files("R"), value = TRUE), function(x) source(file.path("R", x)))
packages.in <- c("dplyr", "ggplot2", "readxl", "DescTools", "stringr", "R2jags", 
                 "rstan", "ggmcmc", "cowplot")
for(i in 1:length(packages.in)) if(!(packages.in[i] %in% rownames(installed.packages()))) install.packages(packages.in[i])
options(tidyverse.quiet = TRUE, clustermq.scheduler = "multiprocess")
tar_option_set(packages = packages.in)


list(
  # Load raw data
  tar_target(seedlings_file, "data/seedlings_survey.xlsx", format = "file"), 
  tar_target(vegelayer_file, "data/vegetation_surveys.xlsx", format = "file"),
  
  # Compute height - diameter distribution of seedlings and vegetation layers
  tar_target(seedlings_distribution, get_seedlings_distribution(seedlings_file)),
  tar_target(vegelayer_distribution, get_vegelayer_distribution(vegelayer_file)),
  
  # Plot differences in height growth and palatability
  tar_target(fig_palatability_growth, plot_palatability_growth("fig/fig_palatability_growth.pdf"), 
             format = "file"),
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # H1 : browsing vs competition in recruitment
  
  # Directory where all files should be placed
  tar_target(capsis_dir, "C:/capsis4/data/rreshar/herbivory-jb/H1"),
  
  # Build inventory scenario files
  tar_target(inventory_table, 
             build_inventory_scenario_table(sapling.in = c("QUERCUS_ROBUR*1", 
                                                           "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.5", 
                                                           "QUERCUS_ROBUR*0.5-CARPINUS_BETULUS*0.5", 
                                                           "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.25-CARPINUS_BETULUS*0.25"), 
                                            saplingDensity.in = c(10, 30, 50))), 
  tar_target(inventory_files, 
             build_inventory_files(inventory_table, vegelayer_distribution,  seedlings_distribution, 
                                   capsis_dir), format = "file"),
  
  # Build a clearing list
  tar_target(clearing_list, build_clearing_list(cl.freq = c(0))), 
  
  # Build herbivory scenario files
  tar_target(herbivory_table, 
             build_herbivory_scenario_table(saplingBrowsedBiomass_kg_ha_year = c(0:15)*10, 
                                            clearing_list = clearing_list)), 
  tar_target(herbivory_files, 
             build_herbivory_files(herbivory_table, clearing_list, capsis_dir), 
             format = "file"), 
  
  # Build a script
  tar_target(cmd_file, build_script(herbivory_files, inventory_files, capsis_dir), format = "file"), 
  
  # Launch simulations and get output
  tar_target(simulations_output, run_simulations(capsis_dir, cmd_file), format = "file"),
  
  # Format the outputs
  tar_target(simulation_output_formatted_H1, format_simulation_outputs(
    capsis_dir, herbivory_table, inventory_table, cmd_file, simulations_output)), 
  tar_target(simulation_output_formatted_H1.3, format_simulation_outputs3(
    capsis_dir, herbivory_table, inventory_table, cmd_file, simulations_output)), 
  
  # Plot the outputs
  tar_target(fig_H1, plot_H1(simulation_output_formatted_H1.3, file.in = "fig/fig_H1.pdf"), format = "file"),
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # H2: Browsing effect vs seedling density
  
  # Directory where all files should be placed
  tar_target(capsis_dir.H2, "C:/capsis4/data/rreshar/herbivory-jb/H2"),
  
  # Build inventory scenario files
  tar_target(inventory_table.H2, 
             build_inventory_scenario_table(sapling.in = c("QUERCUS_ROBUR*1", 
                                                           "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.5", 
                                                           "QUERCUS_ROBUR*0.5-CARPINUS_BETULUS*0.5", 
                                                           "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.25-CARPINUS_BETULUS*0.25"), 
                                            saplingDensity.in = c(2:10)*5)), 
  tar_target(inventory_files.H2, 
             build_inventory_files(inventory_table.H2, vegelayer_distribution,  seedlings_distribution, 
                                   capsis_dir.H2), format = "file"),
  
  # Build a clearing list
  tar_target(clearing_list.H2, build_clearing_list(cl.freq = c(0))), 
  
  # Build herbivory scenario files
  tar_target(herbivory_table.H2, 
             build_herbivory_scenario_table(saplingBrowsedBiomass_kg_ha_year = c(0, 150), 
                                            clearing_list = clearing_list.H2)), 
  tar_target(herbivory_files.H2, 
             build_herbivory_files(herbivory_table.H2, clearing_list.H2, capsis_dir.H2), 
             format = "file"), 
  
  # Build a script
  tar_target(cmd_file.H2, build_script(herbivory_files.H2, inventory_files.H2, capsis_dir.H2), format = "file"), 
  
  # Launch simulations and get output
  tar_target(simulations_output.H2, run_simulations(capsis_dir.H2, cmd_file.H2), format = "file"),
  
  # Format the outputs
  tar_target(simulation_output_formatted.H2, format_simulation_outputs(
    capsis_dir.H2, herbivory_table.H2, inventory_table.H2, cmd_file.H2, simulations_output.H2)), 
  tar_target(simulation_output_formatted_H2.3, format_simulation_outputs3(
    capsis_dir.H2, herbivory_table.H2, inventory_table.H2, cmd_file.H2, simulations_output.H2)), 
  
  # Plot the outputs
  tar_target(fig_H2, plot_H2(simulation_output_formatted_H2.3, file.in = "fig/fig_H2.pdf"), format = "file"),
  
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # H3: Management of browsing in different seedling density
  
  # Directory where all files should be placed
  tar_target(capsis_dir.H3, "C:/capsis4/data/rreshar/herbivory-jb/H3"),
  
  # Build inventory scenario files
  tar_target(inventory_table.H3, 
             build_inventory_scenario_table(sapling.in = c("QUERCUS_ROBUR*0.33-FAGUS_SYLVATICA*0.33-CARPINUS_BETULUS*0.33"), 
                                            saplingDensity.in = rep(c(10, 30, 50), each = 10))), 
  tar_target(inventory_files.H3, 
             build_inventory_files(inventory_table.H3, vegelayer_distribution,  seedlings_distribution, 
                                   capsis_dir.H3), format = "file"),
  
  # Build a clearing list
  tar_target(clearing_list.H3, build_clearing_list2(
    sp.compet = c("CARPINUS_BETULUS-FAGUS_SYLVATICA", "CARPINUS_BETULUS", "FAGUS_SYLVATICA"),
    cl.freq = c(0, 4))), 
  
  # Build herbivory scenario files
  tar_target(herbivory_table.H3, 
             build_herbivory_scenario_table(saplingBrowsedBiomass_kg_ha_year = c(0, 75, 150), 
                                            clearing_list = clearing_list.H3)), 
  tar_target(herbivory_files.H3, 
             build_herbivory_files(herbivory_table.H3, clearing_list.H3, capsis_dir.H3), 
             format = "file"), 
  
  # Build a script
  tar_target(cmd_file.H3, build_script(herbivory_files.H3, inventory_files.H3, capsis_dir.H3), format = "file"), 
  
  # Launch simulations and get output
  tar_target(simulations_output.H3, run_simulations(capsis_dir.H3, cmd_file.H3), format = "file"),
  
  # Format the outputs
  tar_target(simulation_output_formatted_H3, format_simulation_outputs3(
    capsis_dir.H3, herbivory_table.H3, inventory_table.H3, cmd_file.H3, simulations_output.H3)), 
  
  # Plot the outputs
  tar_target(fig_H3, plot_H3(simulation_output_formatted_H3, file.in = "fig/fig_3.pdf"), format = "file"), 
  tar_target(fig_H3_jpg, plot_H3(simulation_output_formatted_H3, file.in = "fig/fig_H3.jpg"), format = "file"),
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # H4 : H1 and H2 merged and simpler (no linear relations)
  
  # Directory where all files should be placed
  tar_target(capsis_dir_H4, "C:/capsis4/data/rreshar/herbivory-jb/H4"),
  
  # Build inventory scenario files
  tar_target(inventory_table_H4, 
             build_inventory_scenario_table(sapling.in = c("QUERCUS_ROBUR*1", 
                                                           "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.5", 
                                                           "QUERCUS_ROBUR*0.5-CARPINUS_BETULUS*0.5", 
                                                           "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.25-CARPINUS_BETULUS*0.25"), 
                                            saplingDensity.in = rep(c(10, 30, 50), each = 10))), 
  tar_target(inventory_files_H4, 
             build_inventory_files(inventory_table_H4, vegelayer_distribution,  seedlings_distribution, 
                                   capsis_dir_H4), format = "file"),
  
  # Build a clearing list
  tar_target(clearing_list_H4, build_clearing_list(cl.freq = c(0))), 
  
  # Build herbivory scenario files
  tar_target(herbivory_table_H4, 
             build_herbivory_scenario_table(saplingBrowsedBiomass_kg_ha_year = c(0, 75, 150), 
                                            clearing_list = clearing_list_H4)), 
  tar_target(herbivory_files_H4, 
             build_herbivory_files(herbivory_table_H4, clearing_list_H4, capsis_dir_H4), 
             format = "file"), 
  
  # Build a script
  tar_target(cmd_file_H4, build_script(herbivory_files_H4, inventory_files_H4, capsis_dir_H4), format = "file"), 
  
  # Launch simulations and get output
  tar_target(simulations_output_H4, run_simulations(capsis_dir_H4, cmd_file_H4), format = "file"),
  
  # Format the outputs
  tar_target(simulation_output_formatted_H4, format_simulation_outputs3(
    capsis_dir_H4, herbivory_table_H4, inventory_table_H4, cmd_file_H4, simulations_output_H4)), 
  
  # Plot the outputs
  tar_target(fig_H4, plot_H4(simulation_output_formatted_H4, file.in = "fig/fig_H4.pdf"), format = "file"), 
  tar_target(fig_H4_jpg, plot_H4(simulation_output_formatted_H4, file.in = "fig/fig_H4.jpg"), format = "file")
  
)

