options(java.parameters = "-Xmx300g")
rm(list = ls())

library(foreign)
library(ggplot2)
library(ROCR) # for diagnostics and ROC plots/stats
library(bartMachine)

# setwd("/Users/qiangguo/Dropbox/dbart_mid/chipman_replication")
path <- "/scratch/qg251/dbart_mid/chipman_replication"
setwd(path)
load("spam_final_model_5cv.RData")


# path <- "/scratch/qg251/dbart_mid/chipman_replication"
# library(foreign)
# library(bartMachine)
# # library(data.table)
# library(dplyr)
# library(caret)
# library(pROC)



###### dataset with a binary dependent variable used in ESL https://web.stanford.edu/~hastie/ElemStatLearn/
###### 57 predictors and 4601 obs

pd_plot_prob_case_select = function(bart_machine, j, levs = c(0.05, seq(from = 0.10, to = 0.90, by = 0.10), 0.95), lower_ci = 0.025, upper_ci = 0.975, prop_data = 1, choice){
  # check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
  if (class(j) == "integer"){
    j = as.numeric(j)
  }
  if (class(j) == "numeric" && (j < 1 || j > bart_machine$p)){
    stop(paste("You must set j to a number between 1 and p =", bart_machine$p))
  } else if (class(j) == "character" && !(j %in% bart_machine$training_data_features)){
    stop("j must be the name of one of the training features (see \"<bart_model>$training_data_features\")")
  } else if (!(class(j) == "numeric" || class(j) == "character")){
    stop("j must be a column number or column name")
  }
  
  x_j = bart_machine$model_matrix_training_data[, j]
  
  #fail with a warning if there's only one value (we don't want an error because it would fail on loops).
  if (length(unique(na.omit(x_j))) <= 1){
    warning("There must be more than one unique value in this training feature. PD plot not generated.")
    return()
  }
  x_j_quants = unique(quantile(x_j, levs, na.rm = TRUE))
  
  #fail with a warning if there's only one value (we don't want an error because it would fail on loops).
  if (length(unique(x_j_quants)) <= 1){
    warning("There must be more than one unique value among the quantiles selected. PD plot not generated.")
    return()
  }
  
  # n_pd_plot = round(bart_machine$n * prop_data)
  if (choice == "high"){
    n_pd_plot = sum(bart_machine$y == "spam")
    bart_predictions_by_quantile = array(NA, c(length(x_j_quants), n_pd_plot, bart_machine$num_iterations_after_burn_in))
    
    for (q in 1 : length(x_j_quants)){
      #pull out a certain proportion of the data randomly
      # indices = sample(1 : bart_machine$n, n_pd_plot)
      indices = which(bart_machine$y == "spam")
      #now create test data matrix
      test_data = bart_machine$X[indices, ]
      test_data[, j] = rep(x_j_quants[q], n_pd_plot)
      
      bart_predictions_by_quantile[q, , ] = 1 - bart_machine_get_posterior(bart_machine, test_data)$y_hat_posterior_samples
      cat(".")
    }
    cat("\n")  
  }
  if (choice == "low"){
    n_pd_plot = sum(bart_machine$y == "not")
    bart_predictions_by_quantile = array(NA, c(length(x_j_quants), n_pd_plot, bart_machine$num_iterations_after_burn_in))
    
    for (q in 1 : length(x_j_quants)){
      #pull out a certain proportion of the data randomly
      # indices = sample(1 : bart_machine$n, n_pd_plot)
      indices = which(bart_machine$y == "not")
      #now create test data matrix
      test_data = bart_machine$X[indices, ]
      test_data[, j] = rep(x_j_quants[q], n_pd_plot)
      
      bart_predictions_by_quantile[q, , ] = 1 - bart_machine_get_posterior(bart_machine, test_data)$y_hat_posterior_samples
      cat(".")
    }
    cat("\n")  
  }
  
  
  if (bart_machine$pred_type == "classification"){ ##convert to probits
    bart_predictions_by_quantile = bart_predictions_by_quantile
  }
  
  bart_avg_predictions_by_quantile_by_gibbs = array(NA, c(length(x_j_quants), bart_machine$num_iterations_after_burn_in))
  for (q in 1 : length(x_j_quants)){
    for (g in 1 : bart_machine$num_iterations_after_burn_in){
      bart_avg_predictions_by_quantile_by_gibbs[q, g] = mean(bart_predictions_by_quantile[q, , g])
    }
  }
  
  bart_avg_predictions_by_quantile = apply(bart_avg_predictions_by_quantile_by_gibbs, 1, mean)
  bart_avg_predictions_lower = apply(bart_avg_predictions_by_quantile_by_gibbs, 1, quantile, probs = lower_ci)
  bart_avg_predictions_upper = apply(bart_avg_predictions_by_quantile_by_gibbs, 1, quantile, probs = upper_ci)
  
  var_name = ifelse(class(j) == "character", j, bart_machine$training_data_features[j])
  # if (var_name == "gdpgrowth"){
  #   var_name = "GDP growth"
  # }
  # if (var_name == "lmtnest"){
  #   var_name = "Mountaineous terrain"
  # }
  # if (var_name == "life"){
  #   var_name = "Life expectancy"
  # }
  # if (var_name == "infant"){
  #   var_name = "Infant mortality"
  # }
  # if (var_name == "illiteracy"){
  #   var_name = "Illiteracy rate"
  # }
  # if (var_name == "lpopns"){
  #   var_name = "Logged Population"  
  # }
  # if (var_name == "elfo"){
  #   var_name = "Ethnolinguistic diversity"
  # }
  # if (var_name == "ln_gdpen"){
  #   var_name = "GDP per Capita"
  # }
  ylab_name = ifelse(bart_machine$pred_type == "classification", "Probabilities", "Partial Effect")
  par(mar=c(5,5,5,5))
  plot(x_j_quants, bart_avg_predictions_by_quantile, 
       type = "o", 
       # main = "Partial Effect vs. Conditional Effect",
       # ylim = c(min(bart_avg_predictions_lower, bart_avg_predictions_upper), max(bart_avg_predictions_lower, bart_avg_predictions_upper)),
       ylim = c(0, 1),
       # ylab = "",
       ylab = ylab_name,
       # mtext("Probabilities", side=2, line=2.2, cex=2),
       xlab = paste(var_name, "plotted at specified quantiles"),
       # xlab = " Growth plotted at specified quantiles",
       cex.lab = 2, cex.axis = 1.5)
  
  # polygon(c(x_j_quants, rev(x_j_quants)), c(bart_avg_predictions_upper, rev(bart_avg_predictions_lower)), col = "gray87", border = NA)
  lines(x_j_quants, bart_avg_predictions_lower, type = "o", pch = 19, col = "blue", lwd = 1, lty = 2)
  lines(x_j_quants, bart_avg_predictions_upper, type = "o", pch = 19, col = "blue", lwd = 1, lty = 2)
  lines(x_j_quants, bart_avg_predictions_by_quantile, type = "o", pch = 19, lwd = 2, col = "blue")
  
  invisible(list(x_j_quants = x_j_quants, bart_avg_predictions_by_quantile = bart_avg_predictions_by_quantile, prop_data = prop_data))
}


cond_plot_prob_case_select = function(bart_machine, j, levs = c(0.05, seq(from = 0.10, to = 0.90, by = 0.10), 0.95), lower_ci = 0.025, upper_ci = 0.975, prop_data = 1, choice){
  # check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
  if (class(j) == "integer"){
    j = as.numeric(j)
  }
  if (class(j) == "numeric" && (j < 1 || j > bart_machine$p)){
    stop(paste("You must set j to a number between 1 and p =", bart_machine$p))
  } else if (class(j) == "character" && !(j %in% bart_machine$training_data_features)){
    stop("j must be the name of one of the training features (see \"<bart_model>$training_data_features\")")
  } else if (!(class(j) == "numeric" || class(j) == "character")){
    stop("j must be a column number or column name")
  }
  
  x_j = bart_machine$model_matrix_training_data[, j]
  
  #fail with a warning if there's only one value (we don't want an error because it would fail on loops).
  if (length(unique(na.omit(x_j))) <= 1){
    warning("There must be more than one unique value in this training feature. PD plot not generated.")
    return()
  }
  x_j_quants = unique(quantile(x_j, levs, na.rm = TRUE))
  print(x_j_quants)
  #fail with a warning if there's only one value (we don't want an error because it would fail on loops).
  if (length(unique(x_j_quants)) <= 1){
    warning("There must be more than one unique value among the quantiles selected. PD plot not generated.")
    return()
  }
  
  # n_pd_plot = round(bart_machine$n * prop_data)
  # n_pd_plot = sum(bart_machine$y == "war")
  bart_predictions_by_quantile = array(NA, c(length(x_j_quants), bart_machine$num_iterations_after_burn_in))
  
  if (choice == "high"){
    for (q in 1 : length(x_j_quants)){
      # pull out a certain proportion of the data randomly
      # indices = sample(1 : bart_machine$n, n_pd_plot)
      indices = which(bart_machine$y == "spam")
      # now create test data matrix
      test_data = bart_machine$X[1, ]
      test_data[1, ] = apply(bart_machine$X[indices,], 2, median)
      test_data[, j] = x_j_quants[q]
      
      bart_predictions_by_quantile[q, ] = 1 - bart_machine_get_posterior(bart_machine, test_data)$y_hat_posterior_samples
      cat(".")
    }
    cat("\n") 
  }
  if (choice == "low"){
    for (q in 1 : length(x_j_quants)){
      # pull out a certain proportion of the data randomly
      # indices = sample(1 : bart_machine$n, n_pd_plot)
      indices = which(bart_machine$y == "not")
      # now create test data matrix
      test_data = bart_machine$X[1, ]
      test_data[1, ] = apply(bart_machine$X[indices,], 2, median)
      test_data[, j] = x_j_quants[q]
      
      bart_predictions_by_quantile[q, ] = 1 - bart_machine_get_posterior(bart_machine, test_data)$y_hat_posterior_samples
      cat(".")
    }
    cat("\n") 
  }
  
  # if (bart_machine$pred_type == "classification"){ ##convert to probits
  #   bart_predictions_by_quantile = bart_predictions_by_quantile
  # }
  #   
  #   
  # bart_avg_predictions_by_quantile_by_gibbs = array(NA, c(length(x_j_quants), bart_machine$num_iterations_after_burn_in))
  # for (q in 1 : length(x_j_quants)){
  #   for (g in 1 : bart_machine$num_iterations_after_burn_in){
  #     bart_avg_predictions_by_quantile_by_gibbs[q, g] = mean(bart_predictions_by_quantile[q, , g])
  #   }
  # }
  
  bart_avg_predictions_by_quantile = apply(bart_predictions_by_quantile, 1, mean)
  bart_avg_predictions_lower = apply(bart_predictions_by_quantile, 1, quantile, probs = lower_ci)
  bart_avg_predictions_upper = apply(bart_predictions_by_quantile, 1, quantile, probs = upper_ci)
  
  var_name = ifelse(class(j) == "character", j, bart_machine$training_data_features[j])
  # if (var_name == "gdpgrowth"){
  #   var_name = "GDP growth"
  # }
  # if (var_name == "lmtnest"){
  #   var_name = "Mountaineous terrain"
  # }
  # if (var_name == "life"){
  #   var_name = "Life expectancy"
  # }
  # if (var_name == "infant"){
  #   var_name = "Infant mortality"
  # }
  # if (var_name == "illiteracy"){
  #   var_name = "Illiteracy rate"
  # }
  # if (var_name == "lpopns"){
  #   var_name = "Logged Population"  
  # }
  # if (var_name == "elfo"){
  #   var_name = "Ethnolinguistic diversity"
  # }
  # if (var_name == "ln_gdpen"){
  #   var_name = "GDP per Capita"
  # }
  ylab_name = ifelse(bart_machine$pred_type == "classification", "Conditional Effect (Probabilities)", "Conditional Effect")
  # plot(x_j_quants, bart_avg_predictions_by_quantile, 
  #      type = "o", 
  #      main = "Conditional Effect Plot",
  #      # ylim = c(min(bart_avg_predictions_lower, bart_avg_predictions_upper), max(bart_avg_predictions_lower, bart_avg_predictions_upper)),
  #      ylim = c(0, 1),
  #      ylab = ylab_name,
  #      xlab = paste(var_name, "plotted at specified quantiles"))
  # polygon(c(x_j_quants, rev(x_j_quants)), c(bart_avg_predictions_upper, rev(bart_avg_predictions_lower)), col = "grey91", border = NA)
  lines(x_j_quants, bart_avg_predictions_lower, type = "o", pch = 19, col = "red", lwd = 1, lty = 2)
  lines(x_j_quants, bart_avg_predictions_upper, type = "o", pch = 19, col = "red", lwd = 1, lty = 2)
  lines(x_j_quants, bart_avg_predictions_by_quantile, type = "o", lwd = 2, pch = 19, col = "red")
  # legend("topright", c("Partial Effect", "Conditional Effect"), lty = 1, pch = 19, col = c("blue", "red"))
  # invisible(list(x_j_quants = x_j_quants, bart_avg_predictions_by_quantile = bart_avg_predictions_by_quantile, prop_data = prop_data))
}




pdf(paste0(path, "/spam_V2.pdf"))
# pdf("~/Dropbox/dbart_mid/Civil_War_PA/partial_vs_conditional_high.pdf", height = 8, width = 12)
pd_plot_prob_case_select(md, levs = c(.01, seq(0.05, 0.95, 0.05), .99), j = "V2", choice = "high")
cond_plot_prob_case_select(md, levs = c(.01, seq(0.05, 0.95, 0.05), .99), j = "V2", choice = "high")
dev.off()