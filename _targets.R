# _targets.R file
library(targets)
lapply(grep("R$", list.files("R"), value = TRUE), function(x) source(file.path("R", x)))
packages.in <- c("dplyr", "ggplot2", "readxl", "DescTools", "stringr", "purrr", "cowplot")
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
  tar_target(fig_palatability_growth, plot_palatability_growth("fig/fig_palatability_growth.jpg"), 
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
                                            saplingDensity.in = c(5:80))), 
  tar_target(inventory_files, 
             build_inventory_files(inventory_table, vegelayer_distribution,  seedlings_distribution, 
                                   capsis_dir), format = "file"),
  
  # Build a clearing list
  tar_target(clearing_list, build_clearing_list(cl.freq = c(0))), 
  
  # Build herbivory scenario files
  tar_target(herbivory_table, 
             build_herbivory_scenario_table(saplingBrowsedBiomass_kg_ha_year = c(0, 50, 100, 150, 200), 
                                            clearing_list = clearing_list)), 
  tar_target(herbivory_files, 
             build_herbivory_files(herbivory_table, clearing_list, capsis_dir), 
             format = "file"), 
  
  # Build a script
  tar_target(cmd_file, build_script(herbivory_files, inventory_files, capsis_dir), format = "file"), 
  
  # Launch simulations and get output
  tar_target(simulations_output, run_simulations(capsis_dir, cmd_file), format = "file"),
  
  # Format the outputs
  tar_target(simulation_output_formatted, format_simulation_outputs(
    capsis_dir, herbivory_table, inventory_table, cmd_file, simulations_output)), 
  tar_target(simulation_output_formatted2, format_simulation_outputs2(
    capsis_dir, herbivory_table, inventory_table, cmd_file, simulations_output)), 
  
  # Plot the outputs
  tar_target(fig_recruitment10_density_browsing, plot_recruitment_density_browsing(
    simulation_output_formatted, year.in = 10, "fig/H1/fig_recruitment10_density_browsing.png"), 
    format = "file"), 
  tar_target(fig_recruitment10_density_browsing2, plot_recruitment_density_browsing2(
    simulation_output_formatted, year.in = 10, "fig/H1/fig_recruitment10_density_browsing2.png"), 
    format = "file"), 
  tar_target(fig_recruitment10_density_browsing3, plot_recruitment_density_browsing3(
    simulation_output_formatted, year.in = 10, "fig/H1/fig_recruitment10_density_browsing3.png"), 
    format = "file"), 
  tar_target(fig_recruitment15_density_browsing, plot_recruitment_density_browsing(
    simulation_output_formatted, year.in = 15, "fig/H1/fig_recruitment15_density_browsing.png"), 
    format = "file"), 
  tar_target(fig_recruitment15_density_browsing2, plot_recruitment_density_browsing2(
    simulation_output_formatted, year.in = 15, "fig/H1/fig_recruitment15_density_browsing2.png"), 
    format = "file"), 
  tar_target(fig_recruitment20_density_browsing, plot_recruitment_density_browsing(
    simulation_output_formatted, year.in = 20, "fig/H1/fig_recruitment20_density_browsing.png"), 
    format = "file"), 
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # H2 : browsing vs competition in clearing
  
  # Directory where all files should be placed
  tar_target(capsis_dir.H2, "C:/capsis4/data/rreshar/herbivory-jb/H2"),
  
  # Build inventory scenario files
  tar_target(inventory_table.H2, 
             build_inventory_scenario_table(sapling.in = c("QUERCUS_ROBUR*1", 
                                                           "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.5", 
                                                           "QUERCUS_ROBUR*0.5-CARPINUS_BETULUS*0.5", 
                                                           "QUERCUS_ROBUR*0.5-FAGUS_SYLVATICA*0.25-CARPINUS_BETULUS*0.25"), 
                                            saplingDensity.in = c(5, 20, 40))), 
  tar_target(inventory_files.H2, 
             build_inventory_files(inventory_table.H2, vegelayer_distribution,  seedlings_distribution, 
                                   capsis_dir.H2), format = "file"),
  
  # Build a clearing list
  tar_target(clearing_list.H2, build_clearing_list(cl.freq = c(0, 2, 3, 4))), 
  
  # Build herbivory scenario files
  tar_target(herbivory_table.H2, 
             build_herbivory_scenario_table(saplingBrowsedBiomass_kg_ha_year = c(0:100)*2, 
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
  tar_target(simulation_output_formatted2.H2, format_simulation_outputs2(
    capsis_dir.H2, herbivory_table.H2, inventory_table.H2, cmd_file.H2, simulations_output.H2)), 
  
  # Plot the outputs
  tar_target(fig_recruitment10_clearing_browsing, plot_recruitment_clearing_browsing(
    simulation_output_formatted.H2, year.in = 10, "fig/H2/fig_recruitment10_clearing_browsing.png"), 
    format = "file"), 
  tar_target(fig_recruitment15_clearing_browsing, plot_recruitment_clearing_browsing(
    simulation_output_formatted.H2, year.in = 15, "fig/H2/fig_recruitment15_clearing_browsing.png"), 
    format = "file"), 
  tar_target(fig_recruitment20_clearing_browsing, plot_recruitment_clearing_browsing(
    simulation_output_formatted.H2, year.in = 20, "fig/H2/fig_recruitment20_clearing_browsing.png"), 
    format = "file"), 
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # H3 : browsing vs competition in clearing
  
  # Directory where all files should be placed
  tar_target(capsis_dir.H3, "C:/capsis4/data/rreshar/herbivory-jb/H3"),
  
  # Build inventory scenario files
  tar_target(inventory_table.H3, 
             build_inventory_scenario_table(sapling.in = c("QUERCUS_ROBUR*0.33-FAGUS_SYLVATICA*0.33-CARPINUS_BETULUS*0.33"), 
                                            saplingDensity.in = c(5, 20))), 
  tar_target(inventory_files.H3, 
             build_inventory_files(inventory_table.H3, vegelayer_distribution,  seedlings_distribution, 
                                   capsis_dir.H3), format = "file"),
  
  # Build a clearing list
  tar_target(clearing_list.H3, build_clearing_list2(
    sp.compet = c("CARPINUS_BETULUS-FAGUS_SYLVATICA", "CARPINUS_BETULUS", "FAGUS_SYLVATICA"),
    cl.freq = c(0, 2, 3, 4))), 
  
  # Build herbivory scenario files
  tar_target(herbivory_table.H3, 
             build_herbivory_scenario_table(saplingBrowsedBiomass_kg_ha_year = c(0:100)*2, 
                                            clearing_list = clearing_list.H3)), 
  tar_target(herbivory_files.H3, 
             build_herbivory_files(herbivory_table.H3, clearing_list.H3, capsis_dir.H3), 
             format = "file"), 
  
  # Build a script
  tar_target(cmd_file.H3, build_script(herbivory_files.H3, inventory_files.H3, capsis_dir.H3), 
             format = "file"), 
  
  # Launch simulations and get output
  tar_target(simulations_output.H3, run_simulations(capsis_dir.H3, cmd_file.H3), format = "file"),
  
  # Format the outputs
  tar_target(simulation_output_formatted.H3, format_simulation_outputs(
    capsis_dir.H3, herbivory_table.H3, inventory_table.H3, cmd_file.H3, simulations_output.H3)), 
  tar_target(simulation_output_formatted2.H3, format_simulation_outputs2(
    capsis_dir.H3, herbivory_table.H3, inventory_table.H3, cmd_file.H3, simulations_output.H3)), 
  
  # Plot the outputs
  tar_target(fig_recruitment10_clearing_browsing.H3, plot_recruitment_clearing_browsing2(
    simulation_output_formatted.H3, year.in = 10, "fig/H3/fig_recruitment10_clearing_browsing.png"), 
    format = "file"), 
  tar_target(fig_recruitment15_clearing_browsing.H3, plot_recruitment_clearing_browsing2(
    simulation_output_formatted.H3, year.in = 15, "fig/H3/fig_recruitment15_clearing_browsing.png"), 
    format = "file"), 
  tar_target(fig_recruitment20_clearing_browsing.H3, plot_recruitment_clearing_browsing2(
    simulation_output_formatted.H3, year.in = 20, "fig/H3/fig_recruitment20_clearing_browsing.png"), 
    format = "file"), 
  tar_target(fig_recruitment.percent10_clearing_browsing.H3, plot_recruitment_clearing_browsing3(
    simulation_output_formatted.H3, year.in = 10, "fig/H3/fig_recruitment.percent10_clearing_browsing.png"), 
    format = "file"), 
  tar_target(fig_recruitment.percent15_clearing_browsing.H3, plot_recruitment_clearing_browsing3(
    simulation_output_formatted.H3, year.in = 15, "fig/H3/fig_recruitment.percent15_clearing_browsing.png"), 
    format = "file"), 
  tar_target(fig_recruitment.percent20_clearing_browsing.H3, plot_recruitment_clearing_browsing3(
    simulation_output_formatted.H3, year.in = 20, "fig/H3/fig_recruitment.percent20_clearing_browsing.png"), 
    format = "file")
  
)

