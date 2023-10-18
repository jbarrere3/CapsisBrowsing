#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#### ---------------- SCRIPT INTRODUCTION ------------------- ####
#
#' @name functions_data.R  
#' @description R script containing all functions relative to data
#               formatting
#' @author Julien BARRERE
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# --- Section 1. Process field data         ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Function to compute height-diameter joint distribution of seedlings from field data
#' @param seedlings_file Path to the file containing seedlings data
get_seedlings_distribution <- function(seedlings_file){
  
  # Data containing the vegetation surveys
  data_seedlings <- as.data.frame(read_xlsx(path = seedlings_file, 
                                            sheet = "Data_these", 
                                            col_names = TRUE)) %>%
    filter(Site %in% c("Arc", "TF")) %>%
    data.frame
  
  
  # Modification of the dataset (change column names, restrict to first year in Arc and TF)
  distrib_seedlings <- data_seedlings %>%
    mutate(Link = paste(Site, Campagne, sep = "_")) %>%
    filter(Link %in% c("Arc_2016_1", "TF_2017_1")) %>%
    mutate(Height = RoundTo(as.numeric(Hauteur), multiple = 5), 
           Diameter = round(as.numeric(Diametre), digits = 1)) %>%
    dplyr::select(Height, Diameter) %>%
    filter(Height > 0) %>%
    data.frame
  
  
  ## -  Tables of contigency
  # Format table
  conti.semis <- table(distrib_seedlings$Height, distrib_seedlings$Diameter)
  # In proportion
  conti.semis.prop <- round(prop.table(conti.semis), digits = 4)
  
  return(conti.semis.prop)
}



#' Function to compute height-diameter joint distribution of bramble from field data
#' @param vegelayer_file Path to the file containing bramble data
get_vegelayer_distribution <- function(vegelayer_file){
  # Data containing the vegetation surveys
  data_vegetation <- as.data.frame(read_xlsx(path = vegelayer_file, 
                                             sheet = "Data_these", 
                                             col_names = TRUE))
  
  # Data containing the species names
  data_corresp <- as.data.frame(read_xlsx(path = vegelayer_file, 
                                          sheet = "Species", 
                                          col_names = TRUE))
  
  # List of the categories of plant species
  list_type <- unique(data_corresp$Type[which((data_corresp$Type != "NA"))])
  
  # replace NA's with zeros
  data_vegetation[is.na(data_vegetation)] <- 0
  
  # Loop to convert cover coefficients in median percentage
  for(i in 1:dim(data_vegetation)[2]){
    begin_i <- strsplit(colnames(data_vegetation)[i], split = "_")[[1]][1]
    if(begin_i == "Rec"){
      data_vegetation[which(data_vegetation[,i] == "1"), i] <- "0.025"
      data_vegetation[which(data_vegetation[,i] == "2"), i] <- "0.025"
      data_vegetation[which(data_vegetation[,i] == "3"), i] <- "0.15"
      data_vegetation[which(data_vegetation[,i] == "4"), i] <- "0.375"
      data_vegetation[which(data_vegetation[,i] == "5"), i] <- "0.625"
      data_vegetation[which(data_vegetation[,i] == "6"), i] <- "0.875"
      data_vegetation[,i] <- as.numeric(data_vegetation[,i])
    }
  }
  
  # Identify columns containing cover values
  colREC <- c()
  for(i in 1:(dim(data_vegetation)[2])){
    if(strsplit(colnames(data_vegetation)[i], "_")[[1]][1] == "Rec"){
      colREC <- c(colREC, i)
    } 
  } 
  
  # Convert cover values in numeric
  data_vegetation[, c((colREC[1]):(dim(data_vegetation)[2]))] <- 
    apply(data_vegetation[, c((colREC[1]):(dim(data_vegetation)[2]))],
          2, as.numeric)
  
  
  
  # List of all the rubus species
  shrubsp <- data_corresp$Indicateur[which(data_corresp$Type == "Rubus_sp")]
  
  # Table that will contain all shrub data, formatted in english
  data_shrub <- data.frame(ID_plot = data_vegetation$id_placette, 
                           ID_subplot = data_vegetation$id_quadra, 
                           Country = array("France", dim = dim(data_vegetation)[1]), 
                           Site = data_vegetation$Site, 
                           EU = data_vegetation$N_Dispo, 
                           plot = data_vegetation$N_releve, 
                           subplot = data_vegetation$N_quadra, 
                           Canopy = data_vegetation$Recouvrement, 
                           Cloture = data_vegetation$Fencing, 
                           Year = data_vegetation$Date, 
                           Modalite = data_vegetation$Modalite) %>%
    mutate(Fencing = case_when(Cloture == "enclos" ~ "fenced", 
                               Cloture == "exclos" ~ "unfenced")) %>%
    dplyr::select(ID_plot, ID_subplot, Country, Site, EU, plot, subplot, 
                  Canopy, Fencing, Year, Modalite) %>%
    data.frame
  
  # Initialisation of the final shrub table, with the same structure as data_shrub
  data_shrub_final <- data_shrub
  
  # Columns that will contain cover and height values per shrub species
  for(i in 1:length(shrubsp)) eval(parse(text=paste("data_shrub$Cover_", shrubsp[i], " <- 0",sep="")))
  for(i in 1:length(shrubsp)) eval(parse(text=paste("data_shrub$Height_", shrubsp[i], " <- 0",sep="")))
  
  # Function that returns the column of data_vegetation that contain the cover values of the input species 
  findcol <- function(x) which(str_count(colnames(data_vegetation), x) == 1)
  
  # Loop to fill data_shrub
  for(i in 1:dim(data_vegetation)[1]){
    for(j in 1:length(shrubsp)){
      # First case: the species is only in the herb layer
      if(paste("Rec_", shrubsp[j], sep = "") %in% colnames(data_vegetation)){
        # Fill associated cover value
        data_shrub[i, paste("Cover_", shrubsp[j], sep = "")] <- data_vegetation[i, paste("Rec_", shrubsp[j], sep = "")]
        # If cover is not null, height is the median of herb layer (25 cm)
        if(data_shrub[i, paste("Cover_", shrubsp[j], sep = "")] > 0){
          data_shrub[i, paste("Height_", shrubsp[j], sep = "")] <- 25 
        }
        # Second case: the species is present in several layers  
      }else{
        # Find columns
        layers_ij <- data_vegetation[i, findcol(paste("Rec_", shrubsp[j], sep = ""))]
        # If the species is present
        if(sum(layers_ij > 0)){
          # Mix cover values of the different layers based on Fisher et al. 2010
          data_shrub[i, paste("Cover_", shrubsp[j], sep = "")] <- (1 - prod(c(array(1, dim = length(layers_ij))) - layers_ij))
          # Height meighted by cover values
          hmed_ij <- c(25, 90, 215, 300)[1:length(layers_ij)]
          data_shrub[i, paste("Height_", shrubsp[j], sep = "")] <- sum(layers_ij*hmed_ij)/sum(layers_ij)
        }
      }
    }
  }
  
  ## - Fill the final dataset
  
  # One column for cover, one for height
  data_shrub_final$Cover <- 0
  data_shrub_final$Height <- 0
  
  # Identify cover columns in data_shrub
  colREC_shrub <- which(str_count(colnames(data_shrub), "Cover") == 1)
  
  # Loop to fill these columns
  for(i in 1:dim(data_vegetation)[1]){
    # Identify the four main species, based on cover
    # Create a vector with all cover values of the individual
    cover.i <- as.numeric(data_shrub[i, colREC_shrub])
    # Extract species with four highest cover values
    species.i <- shrubsp[order(cover.i, decreasing = T)][1:3]
    cover.i <- cover.i[order(cover.i, decreasing = TRUE)][1:3]
    # Create a vector with heights of theses species
    height.i <- as.numeric(data_shrub[i, paste("Height_", species.i, sep = "")])
    # Cover column: sum of the four highest values
    data_shrub_final$Cover[i] <- (1 - prod(c(array(1, dim = length(cover.i))) - cover.i))
    # column height: mean of the heights wieghted by cover values
    if(sum(cover.i) > 0) data_shrub_final$Height[i] <- sum(cover.i*height.i)/sum(cover.i)
  }
  
  # Add a time column that indicates since how long (in years) bramble is monitored
  data_shrub_final$Time <- NA
  for(i in 1:dim(data_shrub_final)[1]){
    if(data_shrub_final$Site[i] == "LPP") data_shrub_final$Time[i] <- data_shrub_final$Year[i] - 2013 + 1
    if(data_shrub_final$Site[i] == "Arc") data_shrub_final$Time[i] <- data_shrub_final$Year[i] - 2017 + 1
    if(data_shrub_final$Site[i] == "TF") data_shrub_final$Time[i] <- data_shrub_final$Year[i] - 2016 + 1
  }
  # Adjust canopy openness value based on Baudry et al. 2013
  data_shrub_final$Canopy <- round((3.81 + 0.57*data_shrub_final$Canopy), digits = 2)
  
  ## - Joint distribution of height and cover
  # Restrict to the sites of Arc and TF the first year, and round values
  distrib_shrub <- data_shrub_final %>%
    dplyr::filter(Site %in% c("Arc", "TF"), 
                  Time == 1) %>%
    dplyr::group_by(ID_plot) %>%
    dplyr::summarise(cover = mean(Cover, na.rm = TRUE), 
                     height = mean(Height, na.rm = TRUE)) %>%
    dplyr::mutate(cover = round(cover, digits = 1), 
                  height = round(height, digits = 0)) %>%
    dplyr::mutate(height = case_when(cover == 0 ~ 0, 
                                     cover > 0 ~ RoundTo(height, 5))) %>%
    dplyr::filter(height < 35) %>%
    data.frame
  
  # Tables of contigency
  conti.shrub <- table(distrib_shrub$height, distrib_shrub$cover)
  conti.shrub <- as.matrix(conti.shrub)
  conti.shrub.prop <- round(prop.table(conti.shrub), digits = 4)
  
  # Second table without the option cover and height is null
  conti.shrub.2 <- conti.shrub
  conti.shrub.2[1, 1] <- 0
  conti.shrub.prop.2 <- round(prop.table(conti.shrub.2), digits = 4)
  
  return(conti.shrub.prop.2)
}




#' Predict the height-diameter distribution of a seedling from a contingency table
#' @param n Number of seedlings to predict
#' @param matr Matrix containing height diameter joint distribution
#' @param xname name of the first column of the output
#' @param yname name of the second column of the output
predict_seedling <- function(n, matr, xname, yname){
  Var1 = c()
  Var2 = c()
  for(i in 1:n){
    test <- FALSE
    while(test == FALSE){
      xrand = runif(1, min = 1, max = dim(matr)[1])
      yrand = runif(1, min = 1, max = dim(matr)[2])
      test = rbinom(1, 1, matr[xrand, yrand])
    }
    Var1 <- c(Var1, as.numeric(rownames(matr)[xrand]))
    Var2 <- c(Var2, as.numeric(colnames(matr)[yrand]))
  }
  out <- data.frame(x = Var1, y = Var2)
  colnames(out) <- c(xname, yname)
  return(out)
}



#' Predict the height-diameter distribution of a seedlings from a contingency table
#' @param n Number of seedlings to predict
#' @param matrix.in Matrix containing height diameter joint distribution
predict_bramble <- function(n, matrix.in){
  Var1 = c()
  Var2 = c()
  for(i in 1:n){
    test <- FALSE
    while(test == FALSE){
      xrand = runif(1, min = 1, max = dim(matrix.in)[1])
      yrand = runif(1, min = 1, max = dim(matrix.in)[2])
      test = rbinom(1, 1, matrix.in[xrand, yrand])
    }
    Var1 <- c(Var1, as.numeric(rownames(matrix.in)[xrand]))
    Var2 <- c(Var2, as.numeric(colnames(matrix.in)[yrand]))
  }
  out <- data.frame(x = Var1/100, y = Var2*100)
  colnames(out) <- c("Height_m", "Cover_percent")
  return(out)
}





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# --- Section 2. Building files for capsis ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




#' Function for optimal repartition of trees in the modeled stage
#' @param n.trees.in Number of trees to include in the stage
#' @param xExtension.in Width of the stage (in meters)
#' @param yExtension.in Depth of the stage (in meters)
#' @return a dataframe containing the x and y coordinates of each tree in the stage
repartTrees <- function(n.trees.in, xExtension.in, yExtension.in){
  # For the future disposition of the trees in a square cell (2*2, 3*3, 4*4, etc.)
  i = 1
  while(i^2 < n.trees.in){i = i+1}
  i = i^2
  # Spacing between the trees in x and y
  stepX.in = round((xExtension.in + 1)/(sqrt(i)+1), digits = 2)
  stepY.in = round((yExtension.in + 1)/(sqrt(i)+1), digits = 2)
  #output : position of the trees in x and y
  out <- expand.grid(x = stepX.in*c(1:sqrt(i)) -1, 
                     y = stepY.in*c(1:sqrt(i)) -1)
  out <- out[c(1:n.trees.in),]
  return(out)
}


#' Extend a datafram with empty columns
#' @param tab.in table to extend
#' @param ncol.in number ofcolumns to add
complete.tab <- function(tab.in, ncol.in){
  ncol0 <- dim(tab.in)[2]
  tab.out <- tab.in
  if(ncol0 < ncol.in){
    for(i in (ncol0+1):ncol.in){
      eval(parse(text = paste("tab.out$col", i, " <- array(NA, dim = dim(tab.in)[1])", sep = "")))
      tab.out[, i] <- array("",  dim = dim(tab.in)[1])
    }
  }
  return(tab.out)
}



#' Transform the code specifying the species present and their proportions in two distinct vectors
#' @param sapling.in character indicating the species present and their proportion
#'                  in the inventory (e.g., "QUERCUS_ROBUR0.5-FAGUS_SYLVATICA0.5")
get_proportion_species <- function(sapling.in){
  # Separate the species present (with proportion still attached to species)
  full <- str_split(sapling.in, "-")[[1]]
  # Initialize proportion and species vectors
  prop <- c()
  sp <- c()
  # Loop on all species present
  for(i in 1:length(full)){
    # Separate species name and proportion
    sp <- c(sp, str_split(full[i], "\\*")[[1]][1])
    prop <- c(prop, as.numeric(str_split(full[i], "\\*")[[1]][2]))
  }
  # format the output
  out <- list(species = sp, proportion = as.character(prop))
  return(out)
}



#' Generate an inventory file in the directory requested
#' @param cellWidth.in Width of each cell
#' @param xExtension.in Width of the stage (in meters)
#' @param yExtension.in Depth of the stage (in meters)
#' @param n.trees.in Number of trees to include in the stage
#' @param n_saplingsPerCell.in Number of saplings per cell
#' @param saplingSpecies.in Character vector of sapling species to include in the inventory
#' @param saplingProportion.in Numeric vector indicating the proportion of each sapling species 
#'                             to include in the inventory
#' @param dir.in Directory where to save the file
#' @param nameFile.in Name of the file to save
#' @param absorptionCoefficient.in Absorption coefficient to set to adult oak trees in the canopy
#'                                 (Control the level of transmittance the first year)
#' @param vegelayer.distribution.in matrix: height cover joint distribution of vegelayers
#' @param seedlings.distribution.in matrix: height diameter joint distribution of saplings
invRrsehar <- function(cellWidth.in = 3, xExtension.in = 27, yExtension.in = 27, 
                       n_trees.in = 25, n_saplingsPerCell.in = 40, 
                       saplingSpecies.in = c("QUERCUS_ROBUR", "FAGUS_SYLVATICA"), 
                       saplingProportion.in = c("0.5", "0.5"), 
                       dir.in = "C:/capsis4/data/rreshar/herbivory-jb", 
                       nameFile.in, absorptionCoefficient.in = 1, 
                       seedlings.distribution.in = seedlings_distribution, 
                       vegelayer.distribution.in = vegelayer_distribution){
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Basic parameters section
  # Value of the basic parameters: 
  cohortLAI.in <- -1
  saplingLAI.in <- -1
  # Creation of the dataframe
  tab.init.in <- data.frame(col1 = c("# Inventory file for RReShar", 
                                     "", 
                                     "sceneOrigin = (0, 0)", 
                                     paste("cellWidth = ", cellWidth.in, sep = ""), 
                                     paste("xExtension = ", xExtension.in, sep = ""), 
                                     paste("yExtension = ", yExtension.in, sep = ""), 
                                     paste("cohortLAI = ", cohortLAI.in, sep = ""), 
                                     paste("saplingLAI = ", saplingLAI.in, sep = "")))
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Soil layer section
  tab.soilLayer.in <- data.frame(col1 = c("# Soil Layers", "# id"), 
                                 col2 = c("", "waterQuantity (mm)"), 
                                 col3 = c("", "waterFC (mm)"), 
                                 col4 = c("", "waterWP (mm)"), 
                                 col5 = c("", "thickness (cm)"), 
                                 col6 = c("", "macroPorosity ([0,1])"), 
                                 col7 = c("", "microPorosity ([0,1])"), 
                                 col8 = c("", "treeRootFraction ([0,1])"), 
                                 col9 = c("", "vegetationLayerRootFraction ([0,1])"), 
                                 col10 = c("", "cohortRootFraction ([0,1])"))
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Cell section
  tab.cell.in <- data.frame(col1 = c("# Cells", 
                                     "# Optional: cells are created from xExtension, yExtension and cellWidth. Cells exist only in the case of water transfer.", 
                                     "# The fourth column contains the list of soil layers ids separated by blanks. Theses ids are written in ascending order of depth and are those of", 
                                     "# the soil layer record section (see above).", 
                                     "# code"), 
                            col2 = c("", "", "", "", "cellRow"), 
                            col3 = c("", "", "", "", "cellCol"), 
                            col4 = c("", "", "", "", "layerIdList (separated by blank)"))
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Species for trees, saplings and cohorts
  tab.tree.species.in <- data.frame(col1 = c("#  Species for trees and cohorts", 
                                             "# id", "0"), 
                                    col2 = c("", "name", "QUERCUS_ROBUR"), 
                                    col3 = c("", "heightToTree (m)", "6"), 
                                    col4 = c("", "absorptionCoefficient (-)", absorptionCoefficient.in), 
                                    col5 = c("", "LAD (m2.m-3)", "0.2"))
  
  
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Individual trees 
  # We start by generating the table that contain all trees. We place one tree per intersection. 
  trees.table.beg.in <- data.frame(col1 = array("T", dim = n_trees.in), 
                                   col2 = as.character(c(10800:(10800+n_trees.in-1))), 
                                   col3 = array("0", dim = n_trees.in), 
                                   col4 = array("40", dim = n_trees.in), 
                                   col5 = array("14", dim = n_trees.in), 
                                   col6 = array("17", dim = n_trees.in))
  trees.table.end.in <- data.frame(col7 = as.character(repartTrees(n_trees.in, xExtension.in,  yExtension.in)$x), 
                                   col8 = as.character(repartTrees(n_trees.in, xExtension.in,  yExtension.in)$y))
  trees.table.in <- cbind.data.frame(trees.table.beg.in, trees.table.end.in)
  
  # Final table
  tab.trees.in <- rbind.data.frame(data.frame(col1 = c("# Trees", "# code"), 
                                              col2 = c("", "id"), 
                                              col3 = c("", "speciesId"), 
                                              col4 = c("", "age"), 
                                              col5 = c("", "dbh_cm"), 
                                              col6 = c("", "height_m"), 
                                              col7 = c("", "x"), 
                                              col8 = c("", "y")), 
                                   trees.table.in)
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Individual saplings
  # We start by generating the table with the inidividual saplings
  # initialize table
  saplings.table.1.in <- data.frame(col1 = character(0), 
                                    col2 = character(0), 
                                    col3 = character(0), 
                                    col4 = character(0), 
                                    col5 = character(0), 
                                    col6 = character(0), 
                                    col7 = character(0), 
                                    col8 = character(0))
  saplings.table.0.in <- data.frame(col1 = array("S", dim = n_saplingsPerCell.in), 
                                    col2 = array("", dim = n_saplingsPerCell.in), 
                                    col3 = array("", dim = n_saplingsPerCell.in), 
                                    col4 = array("1", dim = n_saplingsPerCell.in), 
                                    col5 = array("", dim = n_saplingsPerCell.in), 
                                    col6 = array("", dim = n_saplingsPerCell.in), 
                                    col7 = array("", dim = n_saplingsPerCell.in), 
                                    col8 = array("", dim = n_saplingsPerCell.in))
  saplings_count <- 0
  for(i in 0:(xExtension.in/cellWidth.in - 1)){
    for(j in 0:(xExtension.in/cellWidth.in - 1)){
      x_inf <- i*cellWidth.in
      x_sup <- (i + 1)*cellWidth.in
      y_inf <- j*cellWidth.in
      y_sup <- (j + 1)*cellWidth.in
      data.ij <- saplings.table.0.in
      data.ij$col2 <- as.character(c((900 + saplings_count):(900 + saplings_count+(n_saplingsPerCell.in - 1))))
      saplings_count <- saplings_count + n_saplingsPerCell.in
      saplings_ij <- predict_seedling(n_saplingsPerCell.in, seedlings.distribution.in, "col1", "col2")
      data.ij$col3 <- sample(saplingSpecies.in, prob = saplingProportion.in,
                             n_saplingsPerCell.in, replace = TRUE)
      data.ij$col5 <- as.character(saplings_ij[,2]*10)
      data.ij$col6 <- as.character(saplings_ij[,1])
      data.ij$col7 <- as.character(round(runif(n_saplingsPerCell.in, min = x_inf, max = x_sup), digits = 3))
      data.ij$col8 <- as.character(round(runif(n_saplingsPerCell.in, min = y_inf, max = y_sup), digits = 3))
      saplings.table.1.in <- rbind.data.frame(saplings.table.1.in, data.ij)
    }
  }
  
  # Final table
  tab.saplings.in <- rbind.data.frame(data.frame(col1 = c("# Saplings", "# code"), 
                                                 col2 = c("", "id"), 
                                                 col3 = c("", "speciesName"), 
                                                 col4 = c("", "age"), 
                                                 col5 = c("", "diameter_mm"), 
                                                 col6 = c("", "height_cm"), 
                                                 col7 = c("", "x (m)"), 
                                                 col8 = c("", "y (m)")), 
                                      saplings.table.1.in)
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Cohorts
  tab.cohorts.in <- data.frame(col1 = c("# Cohorts", "# cellRow"), 
                               col2 = c("", "cellCol"), 
                               col3 = c("", "speciesId"), 
                               col4 = c("", "year"), 
                               col5 = c("", "heightUp_m"), 
                               col6 = c("", "heightMoy_m"), 
                               col7 = c("", "diamMoy_cm"), 
                               col8 = c("", "number")) 
  
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Vgetation layers
  # We start by generating the table with the data
  n_cells <- (xExtension.in/cellWidth.in)*(yExtension.in/cellWidth.in)
  bramble.table <- predict_bramble(n_cells, vegelayer.distribution.in)
  tab.vegetationLayer.in <- expand.grid(col1 = c("V"), 
                                        col2 = as.character(c(0:(xExtension.in/cellWidth.in - 1))), 
                                        col3 = as.character(c(0:(yExtension.in/cellWidth.in - 1))), 
                                        col4 = c("RUBUS_FRUCTICOSUS"))
  tab.vegetationLayer.in$col5 <- as.character(bramble.table$Height_m)
  tab.vegetationLayer.in$col6 <- as.character(bramble.table$Cover_percent)
  
  # Add text before the data
  tab.vegetationLayer.in <- rbind.data.frame(data.frame(col1 = c("# Vegetation layers", "# code"), 
                                                        col2 = c("", "cellRow"), 
                                                        col3 = c("", "cellCol"), 
                                                        col4 = c("", "speciesClassName"), 
                                                        col5 = c("", "height_m"), 
                                                        col6 = c("", "cover (%)")))
                                             #tab.vegetationLayer.in)
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Compile and export
  
  
  # Assemble the datasets
  out.table <- rbind.data.frame(complete.tab(tab.init.in, 10),
                                complete.tab(data.frame(col1 = c("", "", "", "")), 10),
                                complete.tab(tab.soilLayer.in, 10), 
                                complete.tab(data.frame(col1 = c("", "", "", "")), 10),
                                complete.tab(tab.cell.in, 10), 
                                complete.tab(data.frame(col1 = c("", "", "", "")), 10),
                                complete.tab(tab.tree.species.in, 10), 
                                complete.tab(data.frame(col1 = c("", "", "", "")), 10),
                                complete.tab(tab.trees.in, 10), 
                                complete.tab(data.frame(col1 = c("", "", "", "")), 10),
                                complete.tab(tab.saplings.in, 10), 
                                complete.tab(data.frame(col1 = c("", "", "", "")), 10),
                                complete.tab(tab.cohorts.in, 10), 
                                complete.tab(data.frame(col1 = c("", "", "", "")), 10),
                                complete.tab(tab.vegetationLayer.in, 10))
  
  
  # Export the data frame
  write.table(x = out.table, 
              file = paste(dir.in, nameFile.in, sep = "/"),
              col.names = FALSE, row.names = FALSE, sep = "\t", 
              quote = FALSE, append = FALSE)
  
  # Return the name oif the file written
  return(paste(dir.in, nameFile.in, sep = "/"))
  
}



#' Function to generate herbivory files for RRESHAR
#' @param vegetationLayerBrowsingIntensity.in numeric: intensity of the browsing intensity on vegelayers
#' @param saplingBrowsedBiomass_kg_ha_year.in numeric: sapling biomass browsed / kg / ha
#' @param saplingBrowsingMaxOncePerYear.in binary: saplings are browsed one per year (1) or more (0)
#' @param clearings.in list of clearing operations to perform on saplings 
#' @param clearings.vegeLayers.in list of clearing operations on vegetation layers
#' @param tableName.in name of the file to generate
#' @param dir.in path to the directory where to save the file
#' @return path to the file generated (in character)

herbivoryFileRrsehar <- function(vegetationLayerBrowsingIntensity.in = 0,
                                 saplingBrowsedBiomass_kg_ha_year.in = 7, 
                                 saplingBrowsingMaxOncePerYear.in = 1, 
                                 clearings.in = list(), 
                                 clearings.vegeLayers.in = list(), 
                                 tableName.in, 
                                 dir.in){ 
  
  # Initialize key parameters
  vegetationLayerFieldHeightReduction_cm.in <- 4.27
  vegetationLayerFieldCoverReduction_percentage.in <- 0.30
  saplingBrowsingMinHeight_m.in <- 0.2
  saplingBrowsingMaxHeight_m.in <- 1.3
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Initilization table
  herbivoryFileInitTable.in <- 
    data.frame(col1 = c("# A herbivory file for RRESHAR", "", 
                        paste("vegetationLayerBrowsingIntensity = ", vegetationLayerBrowsingIntensity.in, sep = ""), 
                        paste("vegetationLayerFieldHeightReduction_cm = ", vegetationLayerFieldHeightReduction_cm.in, sep = ""), 
                        paste("vegetationLayerFieldCoverReduction_percentage = ", vegetationLayerFieldCoverReduction_percentage.in, sep = ""), "", 
                        paste("saplingBrowsedBiomass_kg_ha_year = ", saplingBrowsedBiomass_kg_ha_year.in, sep = ""), "", 
                        paste("saplingBrowsingMinHeight_m = ", saplingBrowsingMinHeight_m.in, sep = ""), 
                        paste("saplingBrowsingMaxHeight_m = ", saplingBrowsingMaxHeight_m.in, sep = ""), 
                        paste("saplingBrowsingMaxOncePerYear = ", saplingBrowsingMaxOncePerYear.in, sep = "")))
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Clearing table
  
  # Do the table
  herbivoryFileClearingTable.in <- data.frame(col1 = c("# Sapling clearing", "# date"), 
                                              col2 = c("", "saplingSpeciesName"), 
                                              col3 = c("", "clearingThresholdHeight_m"), 
                                              col4 = c("", "clearingPercentage]0,100]"))
  if(length(clearings.in) > 0){
    for(i in 1:length(clearings.in)){
      herbivoryFileClearingTable.in <- rbind.data.frame(herbivoryFileClearingTable.in, 
                                                        data.frame(col1 = clearings.in[[i]][1], 
                                                                   col2 = clearings.in[[i]][2], 
                                                                   col3 = clearings.in[[i]][3], 
                                                                   col4 = clearings.in[[i]][4]))
    }
  }
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Clearing vegetation layers table
  
  # Do the table
  herbivoryFileClearingTable.vegeLayers.in <- data.frame(col1 = c("# Vegetation Layer clearing", "# date"), 
                                                         col2 = c("", "SpeciesName"))
  if(length(clearings.vegeLayers.in) > 0){
    for(i in 1:length(clearings.vegeLayers.in)){
      herbivoryFileClearingTable.vegeLayers.in <- rbind.data.frame(herbivoryFileClearingTable.vegeLayers.in, 
                                                                   data.frame(col1 = clearings.vegeLayers.in[[i]][1], 
                                                                              col2 = clearings.vegeLayers.in[[i]][2]))
    }
  }
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Sapling table
  
  # Initialize the list of sapling species
  saplingBrowsing.in = list()
  
  
  # Add two lines of sapling species
  saplingBrowsing.in[[1]] <- c("QUERCUS_ROBUR", "0.67311", "0.852", "0.00278", "0")
  saplingBrowsing.in[[2]] <- c("FAGUS_SYLVATICA", "0.6234", "0.8741", "0.00072", "0.03158")
  saplingBrowsing.in[[3]] <- c("CARPINUS_BETULUS", "0.3563", "0.925", "0.00408", "0.07188")
  
  # Do the table
  herbivoryFileSaplingTable.in <- data.frame(col1 = c("# Sapling browsing", "# saplingSpeciesName"), 
                                             col2 = c("", "biomassBeta1"), 
                                             col3 = c("", "biomassBeta2"), 
                                             col4 = c("", "saplingBrowsingProba_a"), 
                                             col5 = c("", "saplingBrowsingProba_b"))
  if(length(saplingBrowsing.in) > 0){
    for(i in 1:length(saplingBrowsing.in)){
      herbivoryFileSaplingTable.in <- rbind.data.frame(herbivoryFileSaplingTable.in, 
                                                       data.frame(col1 = saplingBrowsing.in[[i]][1], 
                                                                  col2 = saplingBrowsing.in[[i]][2], 
                                                                  col3 = saplingBrowsing.in[[i]][3], 
                                                                  col4 = saplingBrowsing.in[[i]][4], 
                                                                  col5 = saplingBrowsing.in[[i]][5]))
    }
  }
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Compile and export
  
  
  # Assemble the datasets
  herbivoryFile.table.in <- rbind.data.frame(complete.tab(herbivoryFileInitTable.in, 10),
                                             complete.tab(data.frame(col1 = c("", "", "", "")), 10),
                                             complete.tab(herbivoryFileClearingTable.in, 10), 
                                             complete.tab(data.frame(col1 = c("", "", "", "")), 10),
                                             complete.tab(herbivoryFileClearingTable.vegeLayers.in, 10), 
                                             complete.tab(data.frame(col1 = c("", "", "", "")), 10),
                                             complete.tab(herbivoryFileSaplingTable.in, 10))
  
  
  # Export the data frame
  write.table(x = herbivoryFile.table.in, 
              file = paste(dir.in, tableName.in, sep = "/"),
              col.names = FALSE, row.names = FALSE, sep = "\t", 
              quote = FALSE, append = FALSE)
  
  return(paste(dir.in, tableName.in, sep = "/"))
}



#' Build inventory scenario table from sapling species composition
#' @param sapling.in Code specifying the species present and their proportions
#' @param saplingDensity.in numeric vector specifying different sapling densities (in m-2)
build_inventory_scenario_table <- function(sapling.in, saplingDensity.in){
  expand.grid(sapling.in = sapling.in, 
              saplingDensity.in = saplingDensity.in) %>%
    mutate(invScenario = c(1:dim(.)[1]), 
           filename = paste0("inventory_", invScenario, ".txt"))
}



#' Build several inventory files based on a scenario table
#' @param inventory_table Table giving the scenario number, filename, and information on sapling species
#' @param vegelayer.distribution.in matrix: height cover joint distribution of vegelayers
#' @param seedlings.distribution.in matrix: height diameter joint distribution of saplings
#' @param capsis_dir Directory where to create the files
build_inventory_files <- function(inventory_table, vegelayer.distribution.in, 
                                  seedlings.distribution.in, capsis_dir){
  # Initialize output
  out <- c()
  
  # Loop on all inventory scenarios
  for(i in 1:dim(inventory_table)[1]){
    # generate the inventory
    out.i <- invRrsehar(nameFile.in = inventory_table$filename[i], dir.in = capsis_dir,
                        seedlings.distribution.in = seedlings.distribution.in,
                        vegelayer.distribution.in = vegelayer.distribution.in, 
                        saplingSpecies.in = get_proportion_species(inventory_table$sapling.in[i])$species,
                        saplingProportion.in = get_proportion_species(inventory_table$sapling.in[i])$proportion, 
                        n_saplingsPerCell.in = inventory_table$saplingDensity.in[i]*9)
    # Store the name of file created in the output
    out <- c(out, out.i)
  }
  return(out)
  
}




#' Build a clearing list for different frequency scenarios
#' @param sp.compet character vector: the competing species to clear
#' @param cl.freq numeric vector: clearing frequencies to include
#' @param cl.first year when the first clearing is performed
#' @param year.max Year above which no clearing can be performed
build_clearing_list <- function(sp.compet = c("CARPINUS_BETULUS", "FAGUS_SYLVATICA"),
                                cl.freq,
                                cl.first = 5,
                                year.max = 20){
  
  # -- Identify years to clear
  years.to.clear <- list()
  for(i in 1:length(cl.freq)){
    if(cl.freq[[i]] == 0){
      years.to.clear[[i]] <- "0"
    }else{
      years.to.clear.i <- cl.first
      k <- cl.first + cl.freq[i]
      while(k <= year.max){
        years.to.clear.i <- c(years.to.clear.i, k)
        k <- k + cl.freq[i]
      }
      years.to.clear[[i]] <- as.character(years.to.clear.i)
    }
  }
  names(years.to.clear) <- paste0("freq_", cl.freq)
  
  # -- Build final list per clearing scenario
  # --- Initialize output
  out <- list()  
  # --- Loop on clearing frequencies
  for(j in 1:length(cl.freq)){
    # Initialize output for clearing frequency j
    out.j <- list()
    # Only run calculation if clearing frequency is not null
    if(!("0" %in% years.to.clear[[j]])){
      # Counter for list elements
      count.j <- 1
      # Loop on all years to clear for frequency j
      for(y in 1:length(years.to.clear[[j]])){
        # One line per competing species to clear
        for(sp in 1:length(sp.compet)){
          out.j[[count.j]] <- c(years.to.clear[[j]][y], sp.compet[sp], "0.2", "80")
          count.j <- count.j + 1
        }
      }
    }
    out[[j]] <- out.j
  }
  # Change the name of the list elements
  names(out) <- paste0("freq_", cl.freq)
  
  return(out)
}


#' Build a clearing list for different scenarios of frequency and species to clear
#' @param sp.compet character vector: the competing species to clear
#' @param cl.freq numeric vector: clearing frequencies to include
#' @param cl.first year when the first clearing is performed
#' @param year.max Year above which no clearing can be performed
build_clearing_list2 <- function(sp.compet = c("CARPINUS_BETULUS-FAGUS_SYLVATICA", "CARPINUS_BETULUS", "FAGUS_SYLVATICA"),
                                 cl.freq,
                                 cl.first = 5,
                                 year.max = 20){
  
  # -- Names of the elements of the list
  name.list <- expand.grid(sp.compet = sp.compet, cl.freq = cl.freq) %>%
    mutate(name = paste0("freq", cl.freq, ".", sp.compet))
  
  # -- Build final list per clearing scenario
  # --- Initialize final output
  out <- list()  
  # --- Loop on all elements of the list
  for(j in 1:dim(name.list)[1]){
    
    # -- Clearing frequencies
    # Clearing frequency for element j
    cl.freq.j <- name.list$cl.freq[j]
    # Identify years to clear for element j
    if(cl.freq.j == 0){
      years.to.clear.j <- "0"
    }else{
      years.to.clear.j <- cl.first
      k <- cl.first + cl.freq.j
      while(k <= year.max){
        years.to.clear.j <- c(years.to.clear.j, k)
        k <- k + cl.freq.j
      }
    }
    
    # -- Competitive and focal species
    # All species present
    all.species <- c("CARPINUS_BETULUS", "QUERCUS_ROBUR", "FAGUS_SYLVATICA")
    # competitive species for element j
    sp.compet.j <- strsplit(as.character(name.list$sp.compet[j]), split = "-")[[1]]
    
    # Initialize output for clearing frequency j
    out.j <- list()
    # Only run calculation if clearing frequency is not null
    if(!("0" %in% years.to.clear.j)){
      # Counter for list elements
      count.j <- 1
      # Loop on all years to clear for frequency j
      for(y in 1:length(years.to.clear.j)){
        # One line per competing species to clear
        for(sp2 in 1:length(sp.compet.j)){
          out.j[[count.j]] <- c(years.to.clear.j[y], sp.compet.j[sp2], "0.2", "80")
          count.j <- count.j + 1
        }
      }
    }
    out[[j]] <- out.j
  }
  
  # Change the name of the list elements
  names(out) <- name.list$name
  
  return(out)
}




#' Build inventory scenario table from sapling species composition
#' @param saplingBrowsedBiomass_kg_ha_year numeric vector
#' @param vegetationLayerBrowsingIntensity numeric vector
#' @param clearing_list List indicating clearing scenarios to apply
build_herbivory_scenario_table <- function(saplingBrowsedBiomass_kg_ha_year = 7, 
                                           vegetationLayerBrowsingIntensity = 0,
                                           clearing_list){
  expand.grid(saplingBrowsedBiomass_kg_ha_year = saplingBrowsedBiomass_kg_ha_year, 
              vegetationLayerBrowsingIntensity = vegetationLayerBrowsingIntensity, 
              clearing_scenario = names(clearing_list)) %>%
    mutate(herbivoryScenario = c(1:dim(.)[1]), 
           filename = paste0("herbivory_", herbivoryScenario, ".txt"))
}





#' Build several herbivory files based on a scenario table
#' @param herbivory_table Table giving the scenario number, filename, and information on herbivory
#' @param capsis_dir Directory where to create the files
build_herbivory_files <- function(herbivory_table, clearing_list, capsis_dir){
  # Initialize output
  out <- c()
  
  # Loop on all inventory scenarios
  for(i in 1:dim(herbivory_table)[1]){
    # generate the inventory
    out.i <- herbivoryFileRrsehar(
      tableName.in = herbivory_table$filename[i], 
      dir.in = capsis_dir,
      vegetationLayerBrowsingIntensity.in = herbivory_table$vegetationLayerBrowsingIntensity[i],
      saplingBrowsedBiomass_kg_ha_year.in = herbivory_table$saplingBrowsedBiomass_kg_ha_year[i],
      clearings.in = clearing_list[[which(names(clearing_list) == herbivory_table$clearing_scenario[i])]])  
    # Store the name of file created in the output
    out <- c(out, out.i)
  }
  return(out)
  
}



#' Build a script to run simulations based on inventory and herbivory files
#' @param herbivory_files character vector of all herbivory files to use for simulations
#' @param inventory_files character vector of all inventory files to use for simulations
#' @param capsis_dir directory where all files are located
build_script <- function(herbivory_files, inventory_files, capsis_dir){
  
  # Initialize the dataframe that will be converted in a cmd file
  cmdFile <- data.frame(col1 = c("# RReShar script command file", 
                                 paste0("# jb - ", Sys.Date()), 
                                 "", "", 
                                 "samsaralightFileName = light_herbivory.txt", 
                                 "climateFileName = climate_herbivory.txt", 
                                 "", "", 
                                 "# inventoryFileName"), 
                        col2 = c("", "", "", "", "", "", "", "", 
                                 "numberOfYears"), 
                        col3 = c("", "", "", "", "", "", "", "", 
                                 "herbivoryFileName"))
  
  
  # We generate a dataframe that contains all possible combinations of herbivory and inventory files
  # Each line of the data frame is a simulation
  simulations <- expand.grid(col1 = gsub(paste0(capsis_dir, "/"), "", inventory_files), 
                             col2 = "20", # Number of years, 20 for all simulations
                             col3 = gsub(paste0(capsis_dir, "/"), "", herbivory_files))
  
  # Add the dataframe to cmdFile to complete the table
  cmdFile <- rbind.data.frame(cmdFile, simulations)
  
  # Generate the txt file
  write.table(x = cmdFile, 
              file = paste(capsis_dir, "cmd.txt", sep = "/"),
              col.names = FALSE, row.names = FALSE, sep = "\t", 
              quote = FALSE, append = FALSE)
  
  return(paste(capsis_dir, "cmd.txt", sep = "/"))
}


#' Launch simulations with the script
#' @param capsis_dir Directory where all files are stored for this set of simulations
#' @param cmd_file rreshar script to run the simulations
run_simulations <- function(capsis_dir, cmd_file){
  # The command line to run (change directory and run the script with capsis)
  command.to.run <- paste0("cd C:/capsis4 && capsis -p script rreshar.pgms.RRSJulienBarrereScript2020 ", 
                           gsub("C:/capsis4/", "", cmd_file))
  
  # Run the command line
  shell(command.to.run, wait = TRUE, intern = FALSE)
  
  # Directory where the outputs are stored
  output.dir <- paste0(capsis_dir, "/output-cmd.txt")
  
  # Return all outputs
  return(paste(output.dir, list.files(output.dir), sep = "/"))
}




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# --- Section 3 - Format the simulations outputs ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#' Extract and format the simulations output with recuitemnt data for each surveyed year
#' @param capsis_dir Directory where the files are stored
#' @param herbivory_table Table containing the herbivory scenarios
#' @param inventory_table Table containing the inventory scenarios
#' @param cmd_file Path to the command file to the script used to launch the simulations
#' @param simulations_output Path to the files containing the simulation outputs
format_simulation_outputs <- function(capsis_dir, herbivory_table, inventory_table, 
                                       cmd_file, simulations_output){
  
  # Extract the herbivore and inventory scenarios per simulation
  simulation.table <- read.table(cmd_file, col.names = c("invFile", "years", "herbFile"))
  simulation.table <- simulation.table[c(3:dim(simulation.table)[1]), ] %>%
    mutate(sim.number = c(1:dim(.)[1])) %>%
    left_join((herbivory_table %>% rename(herbFile = filename)), by = "herbFile") %>%
    left_join((inventory_table %>% rename(invFile = filename)), by = "invFile") %>%
    mutate(browsing.density_m2 = NA_real_)
  # Add one column per sampling year
  for(y in c(1:20)){simulation.table$Yi <- 0; colnames(simulation.table)[dim(simulation.table)[2]] <- paste0("Y", y)}
  
  # Calculate output variables
  for(i in unique(simulation.table$sim.number)){
    if(floor(i/20) == i/20) print(paste0("Extracting results for simulation ", i, "/", dim(simulation.table)[1]))
    
    # Read the saplings output
    saplingExport_i <- read.table(
      simulations_output[grep(paste0("sim_", i, "_saplingExport"), simulations_output)], 
      col.names = c("year", "cell", "id", "species", "age", "diameter_mm", "height_cm", "browsed", "cleared"))
    # Calculate initial number of oak in the simulation
    n.oak.init <- dim(saplingExport_i %>% filter(year == 0 & species == "QUERCUS_ROBUR"))[1]
    # Calculate percentage of oak above 130cm in height per year
    oak130_i <- saplingExport_i %>%
      filter(species == "QUERCUS_ROBUR" & year > 0) %>%
      mutate(year = paste0("Y", year), 
             recruited = ifelse(height_cm >= 130, 1, 0)) %>%
      group_by(year) %>%
      summarize(n = sum(recruited)) %>%
      tidyr::spread(key = year, value = n) 
    simulation.table[i, colnames(oak130_i)] <- oak130_i
    # Calculate density of sapling browsed
    browsing_i <- saplingExport_i %>%
      group_by(year) %>%
      summarize(browsed.density = sum(browsed)/(27^2)) %>%
      ungroup() %>%
      filter(year > 0) %>%
      summarize(browsed.density.mean = mean(browsed.density))
    simulation.table$browsing.density_m2[i] <- browsing_i$browsed.density.mean
    
  }
  
  return(simulation.table)
}


#' Format data before fitting model for hypothesis 3
#' @param simulation_output_formatted 
get_data_model_H3 = function(simulation_output_formatted){
  
  simulation_output_formatted %>%
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
    mutate(recruitment = recruitment/10000) %>%
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
  
  
}


#' Format data before fitting model for hypothesis 3
#' @param simulation_output_formatted 
get_data_model_H4 = function(simulation_output_formatted){
  
  simulation_output_formatted %>%
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
  
  
}