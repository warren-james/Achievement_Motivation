#### Sorting accuracy ####
# Sort out the accuracy data for Connor 
# I think he's done how many people missed out of 12? 
# but we can check if this gives sensible predictions or not.... 

#### Library ####
library(tidyverse)

#### load in data #### 
df_part1 <- read.csv("data/Accuracy/Session 2- Ability.csv")

#### pre process ####
# Connor recorded no. misses... which is weird... but ok
df_part1 <- df_part1 %>% 
  mutate(inhoop = 12 - Total,
         Acc = inhoop/12,
         Participant = as.factor(Participant))

# save 
save(df_part1, file = "scratch/df_part1")

# quick plot 
df_part1 %>%
  ggplot(aes(Distance, Acc)) + 
  geom_point() + 
  geom_smooth(method = glm,
              method.args = list(family = binomial),
              se = F) + 
  facet_wrap(~Participant)
# looks ok

#### Sort out Curves for each participant ####
m <- glm(Acc ~ Distance:Participant,
         data = df_part1,
         family = binomial) 

# now make a dataframe for predictions
slabs <- seq(0,30,1)
df_exp_acc <- tibble(Participant = rep(unique(df_part1$Participant), each = length(slabs)),
                     Distance = rep(slabs, length(unique(df_part1$Participant)))) %>% 
  mutate(p = predict(m, data.frame(Distance = Distance, Participant = Participant), type = "response"))

# looks good now so we can save this 
save(df_exp_acc, file = "scratch/df_exp_acc")

# plot this 
df_part1 %>% 
  mutate(p = predict(m, type = "response")) %>%
  ggplot(aes(Distance, p)) + 
  geom_point() + 
  geom_smooth(method = glm,
              method.args = list(family = binomial),
              se = F) + 
  facet_wrap(~Participant)
