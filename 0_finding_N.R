#### sampling to find N ####
# These data are of participants making a choice about where to stand 
# when they are tasked with getting a bean bag into a hoop.
# There are two groups: Achievement and Accuracy 
# The Achievement group were told "we want to see how good you are at this"
# and the Accuracy group were told "we want you to get the bean bag into the hoop"
# The idea is that people in the Achievement group should stand further away 
# Each participant completed 1 throw

# The data has 4 columns
# Distance: how far from the hoop they stood (in feet)
# Person: Participant ID
# Group: which instructions the participant was given
# Accuracy: This is an estimate of accuracy based on a small sample

#### Library ####
library(tidyverse)
library(see)
library(effsize)

#### read in data ####
df_choices_overall <- read.csv("data/Pilot/Pilot_data.txt")

# get some descriptives
descs <- df_choices_overall %>% 
  group_by(Group) %>% 
  summarise(N = n(),
            mu = mean(Distance),
            sigma = sd(Distance),
            se = sigma/sqrt(N))
descs

# Look at cohen's d first 
effsize::cohen.d(df_choices_overall$Distance ~ df_choices_overall$Group)
# The effect size is "medium"... -0.55

# if you input these values into this shiny app you get an n of 40.71
# https://designingexperiments.shinyapps.io/power_ttest2group/

#### Start sampling ####
# Setup Ns to look at 
N <- c(8, 10, 15, 20, 30, 50)

# setup emtpy data frame
N_frame <- data.frame(iter = numeric(),
                      N = numeric(),
                      group = character(),
                      mu = numeric(),
                      sigma = numeric(),
                      se = numeric())

# How many times to carry out the sampling
resamples <- 2000

# Loop to draw samples and extract descriptives
for(n in N){ 
  for(group in unique(df_choices_overall$Group)){
    
    # get subset based on group membership
    ss <- df_choices_overall[df_choices_overall$Group == group,]

      for(iter in 1:resamples){
      # sample from data n number of times with replacement
      samples <- sample(ss$Distance, size = n, replace = T)
      
      # get descriptives for the samples
      mu <- mean(samples)
      sigma <- sd(samples)
      se <- sigma/sqrt(n)
      
      # add to dataframe 
      N_frame <- rbind(N_frame, data.frame(iter = iter,
                                           N = n,
                                           group = group,
                                           mu = mu,
                                           sigma = sigma,
                                           se = se))
    }
  }
}
# tidy 
rm(ss, group, iter, mu, n, samples, se, sigma)

#### Plot results ####
# plot this distribution of means from the samples
plt_dist_means <- N_frame %>% 
  ggplot(aes(mu,
             colour = group,
             fill = group)) + 
  geom_histogram(position = "dodge",
                 aes(y = ..density..)) + 
  facet_wrap(~N) +
  theme_bw() +
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  scale_x_continuous("Avergae Distance in Feet")
plt_dist_means


# Plot for the difference in means
# get dataset of differences
N_diff <- N_frame %>%  
  select(iter, N, group, mu) %>%
  spread(group, mu) %>% 
  mutate(diff = Accuracy - Achievement,
         N = as.factor(N),
         below0 = ifelse(diff < 0, 1, 0)) 

# get mean value of how many samples had a difference < 0 
# In this case, diff < 0 means the Accuracy group stood closer to the target
N_below <- N_diff %>% 
  group_by(N) %>% 
  summarise(value = mean(below0))

plt_diff <- N_diff %>%
  ggplot(aes(diff,
             colour = N,
             fill = N)) + 
  geom_histogram(position = "dodge",
                 aes(y = ..density..),
                 alpha = .3) + 
  facet_wrap(~N) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  theme_bw() + 
  geom_vline(xintercept = 0, 
             linetype = "dashed") + 
  geom_text(data = N_below,
            aes(-12, 0.2,
                label = paste("diff = ", value, sep = ""))) + 
  scale_x_continuous("Accuracy - Achievement") + 
  theme(legend.position = "none")
plt_diff

# We probably want to use 20 in each group (so 40 overall)
# pretty close to the value from a power calculation


