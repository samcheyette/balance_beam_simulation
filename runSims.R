library(rstan)
library(lme4)
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


dataset <- "same_icpt" #OR "diff_icpt"

df.blocked <- read.csv(paste("datasets/fake_dataset_blocked_",".csv", sep=dataset))

df.blocked <- df.blocked %>%
  mutate(choseLeft = 1*(NormBeamResponse == "L")) %>%
  mutate(choseBal = 1*(NormBeamResponse == "B")) %>%
  mutate(choseRight = 1*(NormBeamResponse == "R")) 


df.blocked.50 <- subset(df.blocked, df.blocked$Subject <= 50)
df.blocked.100 <- subset(df.blocked, df.blocked$Subject <= 100)
df.blocked.250 <- subset(df.blocked, df.blocked$Subject <= 250)

df.blocked.50$NSUBJ <- length(unique(df.blocked.50$Subject))
df.blocked.100$NSUBJ <- length(unique(df.blocked.100$Subject))

df.blocked.250$NSUBJ <- length(unique(df.blocked.250$Subject))


df.mixed <- read.csv(paste("datasets/fake_dataset_mixed_",".csv", sep=dataset))

df.mixed <- df.mixed %>%
  mutate(choseLeft = 1*(NormBeamResponse == "L")) %>%
  mutate(choseBal = 1*(NormBeamResponse == "B")) %>%
  mutate(choseRight = 1*(NormBeamResponse == "R")) 


df.mixed.50 <- subset(df.mixed, df.mixed$Subject <= 50)
df.mixed.100 <- subset(df.mixed, df.mixed$Subject <= 100)
df.mixed.250 <- subset(df.mixed, df.mixed$Subject <= 250)

df.mixed.50$NSUBJ <- length(unique(df.mixed.50$Subject))
df.mixed.100$NSUBJ <- length(unique(df.mixed.100$Subject))

df.mixed.250$NSUBJ <- length(unique(df.mixed.250$Subject))


stan.data.mixed.50 <- list(NROWS=nrow(df.mixed.50), 
                           NSUBJ = max(df.mixed.50$Subject),
                           NITEMS = max(df.mixed.50$ItemID),
                           subject = df.mixed.50$Subject, 
                           weightSideLeft = 1*(df.mixed.50$WeightSide=="L"),
                           lowMemLoad = 1*(df.mixed.50$CogLoad == 1),
                           highMemLoad = 1*(df.mixed.50$CogLoad == 2),
                           
                           item = df.mixed.50$ItemID,
                           choseLeft = df.mixed.50$choseLeft,
                           choseBal =  df.mixed.50$choseBal,
                           choseRight = df.mixed.50$choseRight,
                           modLeft = df.mixed.50$ModLeft,
                           modRight = df.mixed.50$ModRight,
                           modBal = df.mixed.50$ModBal)


fit.mixed.50<- stan(file="model_simulation.stan", data=stan.data.mixed.50, warmup=50, iter=500, chains=2, cores=2)
saveRDS(fit.mixed.50,paste0(paste0("output_", dataset), "/fit.mixed.50.rds"))


stan.data.mixed.100 <- list(NROWS=nrow(df.mixed.100), 
                            NSUBJ = max(df.mixed.100$Subject),
                            NITEMS = max(df.mixed.100$ItemID),
                            subject = df.mixed.100$Subject, 
                            weightSideLeft = 1*(df.mixed.100$WeightSide=="L"),
                            lowMemLoad = 1*(df.mixed.100$CogLoad == 1),
                            highMemLoad = 1*(df.mixed.100$CogLoad == 2),
                            
                            item = df.mixed.100$ItemID,
                            choseLeft = df.mixed.100$choseLeft,
                            choseBal =  df.mixed.100$choseBal,
                            choseRight = df.mixed.100$choseRight,
                            modLeft = df.mixed.100$ModLeft,
                            modRight = df.mixed.100$ModRight,
                            modBal = df.mixed.100$ModBal)


fit.mixed.100<- stan(file="model_simulation.stan", data=stan.data.mixed.100, warmup=50, iter=500, chains=2, cores=2)
saveRDS(fit.mixed.100, paste0(paste0("output_", dataset), "/fit.mixed.100.rds"))





stan.data.mixed.250 <- list(NROWS=nrow(df.mixed.250), 
                            NSUBJ = max(df.mixed.250$Subject),
                            NITEMS = max(df.mixed.250$ItemID),
                            subject = df.mixed.250$Subject, 
                            weightSideLeft = 1*(df.mixed.250$WeightSide=="L"),
                            lowMemLoad = 1*(df.mixed.250$CogLoad == 1),
                            highMemLoad = 1*(df.mixed.250$CogLoad == 2),
                            
                            item = df.mixed.250$ItemID,
                            choseLeft = df.mixed.250$choseLeft,
                            choseBal =  df.mixed.250$choseBal,
                            choseRight = df.mixed.250$choseRight,
                            modLeft = df.mixed.250$ModLeft,
                            modRight = df.mixed.250$ModRight,
                            modBal = df.mixed.250$ModBal)


fit.mixed.250<- stan(file="model_simulation.stan", data=stan.data.mixed.250, warmup=50, iter=500, chains=2, cores=2)
saveRDS(fit.mixed.250, paste0(paste0("output_", dataset), "/fit.mixed.250.rds"))




stan.data.blocked.50 <- list(NROWS=nrow(df.blocked.50), 
                             NSUBJ = max(df.blocked.50$Subject),
                             NITEMS = max(df.blocked.50$ItemID),
                             subject = df.blocked.50$Subject, 
                             weightSideLeft = 1*(df.blocked.50$WeightSide=="L"),
                             lowMemLoad = 1*(df.blocked.50$CogLoad == 1),
                             highMemLoad = 1*(df.blocked.50$CogLoad == 2),
                             
                             item = df.blocked.50$ItemID,
                             choseLeft = df.blocked.50$choseLeft,
                             choseBal =  df.blocked.50$choseBal,
                             choseRight = df.blocked.50$choseRight,
                             modLeft = df.blocked.50$ModLeft,
                             modRight = df.blocked.50$ModRight,
                             modBal = df.blocked.50$ModBal)


fit.blocked.50<- stan(file="model_simulation.stan", data=stan.data.blocked.50, warmup=50, iter=500, chains=2, cores=2)
saveRDS(fit.blocked.50, paste0(paste0("output_", dataset), "/fit.blocked.50.rds"))


stan.data.blocked.100 <- list(NROWS=nrow(df.blocked.100), 
                              NSUBJ = max(df.blocked.100$Subject),
                              NITEMS = max(df.blocked.100$ItemID),
                              subject = df.blocked.100$Subject, 
                              weightSideLeft = 1*(df.blocked.100$WeightSide=="L"),
                              lowMemLoad = 1*(df.blocked.100$CogLoad == 1),
                              highMemLoad = 1*(df.blocked.100$CogLoad == 2),
                              
                              item = df.blocked.100$ItemID,
                              choseLeft = df.blocked.100$choseLeft,
                              choseBal =  df.blocked.100$choseBal,
                              choseRight = df.blocked.100$choseRight,
                              modLeft = df.blocked.100$ModLeft,
                              modRight = df.blocked.100$ModRight,
                              modBal = df.blocked.100$ModBal)


fit.blocked.100<- stan(file="model_simulation.stan", data=stan.data.blocked.100, warmup=50, iter=500, chains=2, cores=2)
saveRDS(fit.blocked.100, paste0(paste0("output_", dataset), "/fit.blocked.100.rds"))





stan.data.blocked.250 <- list(NROWS=nrow(df.blocked.250), 
                              NSUBJ = max(df.blocked.250$Subject),
                              NITEMS = max(df.blocked.250$ItemID),
                              subject = df.blocked.250$Subject, 
                              weightSideLeft = 1*(df.blocked.250$WeightSide=="L"),
                              lowMemLoad = 1*(df.blocked.250$CogLoad == 1),
                              highMemLoad = 1*(df.blocked.250$CogLoad == 2),
                              
                              item = df.blocked.250$ItemID,
                              choseLeft = df.blocked.250$choseLeft,
                              choseBal =  df.blocked.250$choseBal,
                              choseRight = df.blocked.250$choseRight,
                              modLeft = df.blocked.250$ModLeft,
                              modRight = df.blocked.250$ModRight,
                              modBal = df.blocked.250$ModBal)


fit.blocked.250<- stan(file="model_simulation.stan", data=stan.data.blocked.250, warmup=50, iter=500, chains=2, cores=2)
saveRDS(fit.blocked.250, paste0(paste0("output_", dataset), "/fit.blocked.250.rds"))




