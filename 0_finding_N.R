#### sampling to find N ####
# Want to loop through each group
# Select N samples (with replacement)
# Get the mean
# plot these means... 

#### Library ####
library(tidyverse)

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

# if you input these values into this shiny app you get an n of 40.71
# https://designingexperiments.shinyapps.io/power_ttest2group/

# Setup Ns to look at 
N <- c(8, 10, 15, 20, 30, 50)
N_frame <- data.frame(iter = numeric(),
                      N = numeric(),
                      group = character(),
                      mu = numeric(),
                      sigma = numeric(),
                      se = numeric())
resamples <- 2000

for(n in N){
  for(group in unique(df_choices_overall$Group)){
    ss <- df_choices_overall[df_choices_overall$Group == group,]
    for(iter in 1:resamples){
      # get samples
      samples <- sample(ss$Distance, size = n, replace = T)
      # get descriptives
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

# plot this 
N_frame %>% 
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

# get overlap
# Look at cohen's d first 
effsize::cohen.d(df_choices_overall$Distance ~ df_choices_overall$Group)
# The effect size is "medium"... -0.55

# plot difference
N_diff <- N_frame %>%  
  select(iter, N, group, mu) %>%
  spread(group, mu) %>% 
  mutate(diff = Accuracy - Achievement,
         N = as.factor(N),
         below0 = ifelse(diff < 0, 1, 0)) 

N_below <- N_diff %>% 
  group_by(N) %>% 
  summarise(value = mean(below0))

N_diff %>%
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

# We probably want to use 20 in each group (so 40 overall)
# pretty close to the value from a power calculation


