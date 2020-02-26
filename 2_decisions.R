#### Plot Second part ####
# this is for Connor's experiment 
# we'll be plotting their standing positions given the condition
# and probably by their expected accuracy from that location

#### Library ####
library(tidyverse)

#### load data ####
load("scratch/df_exp_acc")

#### read in new data ####
df_part2 <- read.csv("data/Decisions/Session 1 - Decisions.csv")

# pre process a bit 
df_part2 <- df_part2 %>% 
  mutate(Participant = as.factor(Participant),
         Distance = Position,
         Distance = 20 - Distance,
         # Hit = ifelse(Accuracy == "Yes", 1, 0),
         Hit = as.factor(ifelse(Accuracy == "Yes", 1, 0))) %>% 
  select(Participant, Trial, Distance, Hit)

# merge these 
df_decisions <- merge(df_part2, df_exp_acc)

# extract the 50% mark 
df_sp <- df_exp_acc %>% 
  group_by(Participant) %>% 
  filter(abs(p - .5) == min(abs(p - .5))) %>% 
  mutate(p_sp = p) %>% 
  select(-p)

#### Plotting ####
# try some "over time" stuff
# could do exp(p) to put everything on the same scale... 
# or just do distance and add a line for the 50% distance
df_decisions %>% 
  ggplot(aes(Trial, Distance)) + 
  geom_point(aes(colour = Hit)) +
  geom_line() + 
  geom_hline(data = df_sp, 
             aes(yintercept =  Distance),
             linetype = "dashed") + 
  facet_wrap(~Participant) + 
  theme_bw() + 
  see::scale_color_flat() 
