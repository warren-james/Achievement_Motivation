# Sort out Connor's data so it matches properly

#### library ####
library(tidyverse)

#### read in data ####
#### > Session 1 - Decisions ####
df_dec <- read_csv("OldVersion/data/Decisions/Session 1 - Decisions.csv")

# process the data 
df_dec <- df_dec %>% 
  # rename the Posisition column 
  rename(Distance = Position) %>% 
  # create new columns and tidy others
  mutate(
    Hit = ifelse(Accuracy == "Yes", 1, 0),
    cond = ifelse(Participant %% 2 == 0, "goal", "performance"),
    Participant = as.factor(Participant)
  ) %>% 
  select(Participant, cond, Trial, Distance, Hit)

# write a csv file 
write.csv(df_dec, file = "scratch/newData/ConnorFixed.csv", row.names = F)



