#### Script to look at the Accuracy data ####
# This script will pull in all the acc data and fit some curves to see how people 
# in this experiment did 

#### library ####
library(tidyverse)

#### read in data ####
df_acc_Con <- read_csv("OldVersion/data/Accuracy/Session 2- Ability.csv")
# remove weird columns
df_acc_Con <- df_acc_Con %>%
  # not sure where these columns are coming from... 
  select(-c(X4, X5)) 


df_acc_Xia <- read_csv("data/current/Session 2- Ability.csv")
# remove Connor participants for now 
df_acc_Xia <- df_acc_Xia %>% 
  filter(Participant > 11) %>% 
  # remove participants with missing data 
  drop_na()

df_acc <- rbind(df_acc_Con, df_acc_Xia) %>%
  mutate(inhoop = 12 - Total,
         Acc = inhoop/12,
         Participant = as.factor(Participant)) %>% 
  select(-Total)

# save this for later 
save(df_acc, file = "scratch/newData/df_acc")

#### plots ####
# Looking good for now... this is just the "average" participant for now
df_acc %>% 
  ggplot(aes(Distance, Acc)) + 
  geom_smooth(method = glm,
              method.args = list(family = "binomial"))

#### setup models ####
# This way is using our normal approach... 
# but we can do something a bit better later at some other point if we really want to... 
#### > Standard ####
# make the model
m <- glm(Acc ~ Distance:Participant, 
         family = "binomial", 
         data = df_acc)

# get some predictions 
slabs <- seq(0, 25, 1)

df_expacc <- tibble(Participant = rep(unique(df_acc$Participant), each = length(slabs)),
                    Distance = rep(slabs, length(unique(df_acc$Participant))),
                    ExpAcc = predict(m, data.frame(Distance = Distance, Participant = Participant), type = "response"))

# make a nice plot of this 
df_expacc %>% 
  ggplot(aes(Distance, ExpAcc)) + 
  geom_path(aes(group = Participant),
            alpha = .3)

# save this 
save(df_expacc, file = "scratch/newData/df_expacc")

#### get switch points ####
df_SP <- df_expacc %>% 
  group_by(Participant) %>% 
  filter(abs(ExpAcc - .5) == min(abs(ExpAcc - .5))) %>% 
  mutate(p_sp = ExpAcc) %>% 
  select(-ExpAcc)

# save this 
save(df_SP, file = "scratch/newData/df_SP")
