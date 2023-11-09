#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#### SCRIPT INTRODUCTION ####
#
#' @name functions_analysis.R  
#' @description R script containing all functions relative to data
#               analysis
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






#' Fit models testing the effect of browsing, species and density on max 
#' recruitment and recruitment speed
#' @param data_model output of the simulations formatted to fit model
fit.models_H3 <- function(data_model){
  
  # Initialize final list of models
  list.out <- list()
  
  # Loop on each density value to fit model
  for(j in 1:length(unique(data_model$density0))){
    # Density value j
    density.j <- unique(data_model$density0)[j]
    
    # Fit a simple model 
    model.rmax.j <- aov(log(Rmax + 0.001) ~ factor(browsing)*cleared.species, 
                        data = subset(data_model, density0 == density.j))
    
    # Add to the final list
    eval(parse(text = paste0("list.out$rmax.density", density.j, " <- model.rmax.j")))
    
    # For t.half: only if there is enough data
    if(!(0 %in% (subset(data_model, density0 == density.j) %>%
                 group_by(browsing) %>%
                 summarize(mean = mean(Rmax)))$mean)){
      # Fit a simple model 
      model.t.half.j <- aov(log(t.half + 0.001) ~ factor(browsing)*cleared.species, 
                            data = subset(data_model, density0 == density.j))
      
      # Add to the final list
      eval(parse(text = paste0("list.out$thalf.density", density.j, " <- model.t.half.j")))
      
     } 
  }
  
  ## - Returnn the model list
  return(list.out)
  
}



#' Fit models to test the effect of browsing, species and clearing on max recruitment and recruitment speed
#' @param data_model output of the simulations formatted to fit model
#' @param file.in Name and location of the file to save
fit.models_H4 <- function(data_model){
  
 
  # Initialiaze final list of plots
  list.out <- list()
  
  # Loop on each density value to fit model
  for(j in 1:length(unique(data_model$density0))){
    
    # Density value j
    density.j <- unique(data_model$density0)[j]
    
    ## - For Rmax
    
    # Fit a simple model 
    model.rmax.j <- aov(log(Rmax + 0.001) ~ factor(browsing)*sp.composition, 
                        data = subset(data_model, density0 == density.j))
    # Add to the final list
    eval(parse(text = paste0("list.out$rmax.density", density.j, " <- model.rmax.j")))
    
    ## For t.half
    
    # Only if there is enough data
    if(!(0 %in% (subset(data_model, density0 == density.j) %>%
                 group_by(browsing) %>%
                 summarize(mean = mean(Rmax)))$mean)){
      # Fit a simple model 
      model.t.half.j <- aov(log(t.half + 0.001) ~ factor(browsing)*sp.composition, 
                            data = subset(data_model, density0 == density.j))
      # Add to the final list
      eval(parse(text = paste0("list.out$thalf.density", density.j, " <- model.t.half.j")))
      
    } 
    
  }
  
  ## - Return the output list
  return(list.out)
  
}



#' Function to get the significance of the composition / clearing effect for 
#' a given response variable at each level of density and browsing
#' @param models statistical models of a given hypothesis
#' @param var character of the response variable: "rmax" or "thalf"
get_signif = function(data_model, var){
  
  # Format data model
  data.in = data_model
  data.in$var = unlist(ifelse(var == "thalf", data_model[, "t.half"], data_model[, "Rmax"]))
  data.in$exp = unlist(ifelse("sp.composition" %in% colnames(data_model), 
                              data_model[, "sp.composition"], data_model[, "cleared.species"]))
  data.in = data.in %>%
    ungroup() %>%
    dplyr::select(browsing, exp, density0, var) %>%
    mutate(var = log(var + 0.001))
  
  # Identify the value of initial sapling density for which we have data
  out = expand.grid(density0 = unique(data.in$density0), 
                    browsing = unique(data.in$browsing), 
                    signif = "")
  out$signif = as.character(out$signif)
  
  # Loop on all combinations
  for(i in 1:dim(out)[1]){
    # Subset data for combination i
    data.i = subset(
      data.in, density0 == out$density0[i] & browsing == out$browsing[i])
    # Number of individuals per factor level
    n.per.factor.i = data.i %>% 
      group_by(exp) %>% 
      mutate(na = ifelse(is.na(var), 0, 1)) %>% 
      summarize(n = sum(na))
    
    # If enough individuals to run model, test significance
    if(length(which(n.per.factor.i$n >= 5)) > 1){
      aov.i = aov(var ~ exp, data = data.i)
      if(summary(aov.i)[[1]][1, 5] >= 0.05) out$signif[i] = "ns"
      if(summary(aov.i)[[1]][1, 5] < 0.05) out$signif[i] = "*"
      if(summary(aov.i)[[1]][1, 5] < 0.001) out$signif[i] = "***"
    }
    
  }
  # Return final output
  return(out)
}
