library(rstan)
#library(lme4)
library(ggplot2)
library(tidyr)
library(dplyr)
#library(broom)
#library(gridExtra)
#library(patchwork)
#library(latex2exp)
#library(GGally) # for ggpairs 
#library(readr)



paper_theme <- theme_classic() + theme( axis.title.x = element_text(size=18),
                                        axis.text.x=element_text(colour="black", 
                                                                 size = 14), 
                                        axis.title.y = element_text(size = 18, vjust = 1),
                                        axis.text.y  = element_text(size = 14),
                                        strip.text=element_text(size=16),
                                        legend.title=element_text(size=18),
                                        title=element_text(size=14),
                                        legend.text=element_text(size=16))  

inv_logit <- function(x) { exp(x)/(1+exp(x))}



# Function to extract and add subject-level parameter means to df.blocked_10
add_subject_level_params <- function(fit, df, posterior_samples_lst,
                                     subject_id_var, param_prefix) {
  # Extract posterior samples
  
  # Get the list of parameter names that start with the specified prefix
  param_names <- grep(paste0("^", param_prefix), names(posterior_samples_lst), value = TRUE)
  
  if (length(param_names) > 0) {
    for (param_name in param_names) {
      # Initialize a vector to store means
      subject_means <- rep(NA, nrow(df))
      
      for (s in 1:df$NSUBJ) {
        # Find the rows in df.blocked_10 that correspond to subject s
        subject_rows <- df[[subject_id_var]] == s
        
        # Calculate the mean for this subject from posterior samples
        subject_means[subject_rows] <- mean(posterior_samples_lst[[param_name]][, s])
      }
      
      # Add means to df.blocked_10 as a new column
      df[paste0("mean_", param_name)] <- subject_means
    }
  } else {
    cat("No matching parameters found with prefix '", param_prefix, "'.\n")
  }
  
  return(df)
}




dataset <- "150_0.5" #OR "diff_icpt"

df <- read.csv(paste("datasets/fake_dataset_",".csv", sep=dataset))
df$NSUBJ <- length(unique(df$Subject))

df <- df %>%
  mutate(choseLeft = 1*(NormBeamResponse == "L")) %>%
  mutate(choseBal = 1*(NormBeamResponse == "B")) %>%
  mutate(choseRight = 1*(NormBeamResponse == "R")) 


stan.data <- list(NROWS=nrow(df), 
                           NSUBJ = max(df$Subject),
                           NITEMS = max(df$ItemID),
                           subject = df$Subject, 
                           weightSideLeft = 1*(df$WeightSide=="L"),
                           lowMemLoad = 1*(df$CogLoad == 1),
                           highMemLoad = 1*(df$CogLoad == 2),
                           
                           item = df$ItemID,
                           choseLeft = df$choseLeft,
                           choseBal =  df$choseBal,
                           choseRight = df$choseRight,
                           modLeft = df$ModLeft,
                           modRight = df$ModRight,
                           modBal = df$ModBal)



fit <- stan(file="model_simulation.stan", data=stan.data, iter=500, chains=2, cores=2)
saveRDS(fit,paste0("output/", paste0(paste0("fit.",dataset),".rds")))

