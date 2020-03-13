#### Sorting accuracy ####
# Sort out the accuracy data for Connor 

#### Library ####
library(tidyverse)

#### load in data #### 
df_part1 <- read.csv("data/Accuracy/Session 2- Ability.csv")

#### pre process ####
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
  scale_x_continuous(limits = c(0,20)) +
  facet_wrap(~Participant)
# looks ok

#### Sort out Curves for each participant ####
# this version doesn't work so well...
# so maybe we should just do it by participant
# so we need a loop...
# m <- glm(Acc ~ Distance:Participant,
#          data = df_part1,
#          family = binomial) 
# 
# # now make a dataframe for predictions
# slabs <- seq(0,30,1)
# df_exp_acc <- tibble(Participant = rep(unique(df_part1$Participant), each = length(slabs)),
#                      Distance = rep(slabs, length(unique(df_part1$Participant)))) %>% 
#   mutate(p = predict(m, data.frame(Distance = Distance, Participant = Participant), type = "response"))
# 
# # looks good now so we can save this 
# save(df_exp_acc, file = "scratch/df_exp_acc")

# setup df
df_exp_acc <- tibble(Participant = character(),
                     Distance = numeric(),
                     p = numeric())

# setup slabes to test
slabs <- seq(0,30,1)

# loop to add in data
for(subj in unique(df_part1$Participant)){
  ss <- df_part1 %>% filter(Participant == subj)
  m <- glm(Acc ~ Distance, 
           data = ss, 
           family = binomial)
  df_exp_acc <- rbind(df_exp_acc, tibble(Participant = as.factor(rep(subj, length(slabs))),
                                         Distance = slabs,
                                         p = predict(m, data.frame(Distance = Distance), type = "response")))
}

# save 
save(df_exp_acc, file = "scratch/df_exp_acc2")

# plot this 
df_part1 %>% 
  ggplot(aes(Distance, Acc)) + 
  geom_point() + 
  geom_smooth(data = df_exp_acc,
              aes(y = p), 
              method = glm,
              method.args = list(family = binomial),
              se = F) + 
  scale_x_continuous(limits = c(0,20)) +
  facet_wrap(~Participant)
