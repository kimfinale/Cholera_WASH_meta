### functions needed for meta-analysis
  
## function to calculate the standard error using the lower and upper bound of the CI
calc_se <- function(lower, upper){
    se <- (upper - lower) / (2 * qnorm(0.975))
    return (se)
  }
  
## function for funnel plotting    
meta_freq_funnel <- function(input_category, image_png){
     if (input_category == "Potential Protective Factors"){
       input_dat_freq <- input_data %>% filter(input_data$category  == "Water management - safe water storage"|
                                                 input_data$category == "Water source - basic"|
                                                 input_data$category == "Water source - limited"|
                                                 input_data$category == "Water source - safely managed"|
                                                 input_data$category == "Water treatment - treated")
      }
      else if (input_category == "Potential Risk Factors"){
        input_dat_freq <- input_data %>% filter(input_data$category == "Water management - unsafe water storage"|
                                                  input_data$category == "Water source - surface water"|
                                                  input_data$category == "Water source - unimproved"|
                                                  input_data$category == "Water management - unsafe water storage"|
                                                  input_data$category == "Water treatment - untreated")
      }
      else{
        input_dat_freq <- input_data %>% filter(input_data$category == input_category)
      }
  
#unique(data5$`JMP WASH Category`)
     
## meta-analysis with RE model using rma-function
  print("startrma")
  res <- rma(yi = log_OR, sei = SE, data = input_dat_freq)
  print("endrma")
  summary <- summary(res)
  
## save funnel plot in folder called "results"
  file = file.path("results", image_png)
  png(file, width = 1600, height = 1400, res = 300)
  
  funnel (res, atransf = exp, xlab = "Odds Ratio", las =1) # x-axis log transformed
  #funnel(res, xlab = "Log Odds Ratio", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0, legend=TRUE, las =1) # contour enhanced funnel plot (with funnel centered at 0)
  
  title(input_category)
  
  dev.off()

  return(summary)
  }

## function for meta-analysis using rma-function: forest plotting (Option 1)
meta_freq_forest <- function(input_category, image_png){
  if (input_category == "Potential Protective Factors"){
    input_dat_freq <- input_data %>% filter(input_data$category  == "Water management - safe water storage"|
                                              input_data$category == "Water source - basic"|
                                              input_data$category == "Water source - limited"|
                                              input_data$category == "Water source - safely managed"|
                                              input_data$category == "Water treatment - treated")
  }
  else if (input_category == "Potential Risk Factors"){
    input_dat_freq <- input_data %>% filter(input_data$category == "Water management - unsafe water storage"|
                                              input_data$category == "Water source - surface water"|
                                              input_data$category == "Water source - unimproved"|
                                              input_data$category == "Water management - unsafe water storage"|
                                              input_data$category == "Water treatment - untreated")
  }
  else{
    input_dat_freq <- input_data %>% filter(input_data$category == input_category)
  }
  
  #unique(dat$`JMP WASH Category`)
  
  ## meta-analysis with RE model using the rma-function
  res <- rma(yi = log_OR, sei = SE, data = input_dat_freq, slab = study, measure ="OR")
  summary <- summary(res)
  
  ## save forest plot in folder called "results"
  png(file = file.path("results", image_png), width = 1600, height = 1400, res = 300)
  
  forest(res, addpred=TRUE, atransf = exp, #addfit=FALSE, 
         header="Authors (Year)", xlab = "Odds Ratio (Log Scale)", 
         mlab=text(-9.25, -0.5, pos=4, cex=0.5, bquote(paste("RE Model (Q = ",.(formatC(res$QE, digits=2, format="f")),  ## add text with Q-value, dfs, p-value, I^2 statistic and tau^2
                                                        ", df = ", .(res$k - res$p),
                                                        ", p = ", .(formatC(res$QEp, digits=3, format="f")), 
                                                        "; ", I^2, " = ",.(formatC(res$I2, digits=1, format="f")), "%, ",
                                                        tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")"))),
         xlim=c(-9,2), alim=c(-5,8), olim=c(-8,5), at=log(c(0.05, 0.25, 1, 4, 20)),
         cex=0.6)
  #forest(res)
  
  title(input_category)
  dev.off()
  
  return(summary)
}

## function for meta-analysis using rma-function: forest plotting with exact CIs (Option 2)
meta_freq_forest <- function(input_category, image_png){
  if (input_category == "Potential Protective Factors"){
    input_dat_freq <- input_data %>% filter(input_data$category  == "Water management - safe water storage"|
                                              input_data$category == "Water source - basic"|
                                              input_data$category == "Water source - limited"|
                                              input_data$category == "Water source - safely managed"|
                                              input_data$category == "Water treatment - treated")
  }
  else if (input_category == "Potential Risk Factors"){
    input_dat_freq <- input_data %>% filter(input_data$category == "Water management - unsafe water storage"|
                                              input_data$category == "Water source - surface water"|
                                              input_data$category == "Water source - unimproved"|
                                              input_data$category == "Water management - unsafe water storage"|
                                              input_data$category == "Water treatment - untreated")
  }
  else{
    input_dat_freq <- input_data %>% filter(input_data$category == input_category)
    
  }
  
  ## meta-analysis with RE model using the rma-function 
  res <- rma.uni(yi = log_OR, sei = SE, data = input_dat_freq, slab = study, measure ="OR")
  summary <- summary(res)
  
  ## save forest plot in folder called "results"
  png(file = file.path("results", image_png), width = 1600, height = 1400, res = 300)
  
  ## create upper part of forest plot
  plot.new()
  forest(input_dat_freq$log_OR, ci.lb=input_dat_freq$log_lower, ci.ub=input_dat_freq$log_upper, atransf=exp, 
         header=FALSE, xlab = "Odds Ratio (Log Scale)", slab=input_dat_freq$study, measure ="OR",
         xlim=c(-9,9), ylim=c(-2,8) ,alim=c(-5,5), olim=c(-10,10), at=log(c(0.05, 0.25, 1, 4, 20)),
         cex=0.6)
  
  ## adjust header of upper part of forest plot
  text(5.15, 6.5, "Odds Ratio [95% CI]", font=2, cex=.65)
  text(-7.5, 6.5, "Authors (Year)", font=2, cex=.65)
  
  ## add diamond and pooled estimates
  addpoly(res, row=-1, atransf=exp)
  abline(h=0)
  
  ## add heterogeneity estimates
  text(-5.6, -1.5, cex=0.5, bquote(paste("(Q = ",.(formatC(res$QE, digits=2, format="f")),  ## add text with Q-value, dfs, p-value, I^2 statistic and tau^2
                                         ", df = ", .(res$k - res$p),
                                         ", p = ", .(formatC(res$QEp, digits=3, format="f")), 
                                         "; ", I^2, " = ",.(formatC(res$I2, digits=1, format="f")), "%, ",
                                         tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")")))
  
  ## add title of forest plot by adjusting "XXX"
  #title(input_category)
  title(main=c(paste("XXX"), paste("XXX")), cex.main=0.75) 
  dev.off()
  
  return(summary)
}