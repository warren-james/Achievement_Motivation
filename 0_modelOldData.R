# This script reads in previous data from the accuracy 
# measuring session. It then models the data to get some 
# estimate for where the switch point is likely to be. 
# Using this, we can estimate where people should have 
# switched if we don't have their data.


#### Library ####
library(tidyverse)
library(tidybayes)
library(brms)

#### Functions ####
squash <- function(y, max, min, squash){
  y <- y * ((max-squash) - (min + squash)) + (min + squash)
}

#### read data ####
# we have several experiments to read the data from 
# so we'll do them one by one 

#### > old Achievement Motivation ####
# df_AM <- read_csv("OldVersion/data/Accuracy/Session 2- Ability.csv", col_types = cols())
# 
# # tidy up 
# df_AM <- df_AM %>% 
#   select(Participant, Distance, Total) %>% 
#   mutate(inhoop = 12 - Total,
#          Acc = inhoop/12,
#          Participant = paste(Participant, "AM", sep ="")) %>% 
#   select(-Total)

#### > Asymmetry ####
#### > > Awareness ####
df_aware <- read_csv("data/previous/Asym/Aware/Part1_session_data.csv", col_types = cols())

# tidy 
df_aware <- df_aware %>%
  select(-Direction) %>%
  rename(Distance = Slab, 
         inhoop = Acc) %>% 
  mutate(Acc = inhoop/12,
         Participant = paste(Participant, "aware", sep = ""))

#### > > Hoop size ####
# Might skip this one... but I could just ignore hoop size and then we'll get something
# that kind of works?
df_HS <- readxl::read_xlsx("data/previous/Asym/HoopSize/Part_1_measures.xlsx")

# tidy 
df_HS <- df_HS %>% 
  select(-c(experimenter, direction, hoop_size)) %>% 
  rename(Participant = participant,
         Distance = slab) %>% 
  mutate(Acc = inhoop/12,
         Participant = paste(Participant, "HS", sep = ""))

#### > > Two Throw #### 
df_TT <- read_csv("data/previous/Asym/TwoThrow/Part1_session_data.csv", col_types = cols())

# tidy 
df_TT <- df_TT %>% 
  select(-Direcion) %>% 
  rename(Distance = Slab, 
         inhoop = Acc) %>% 
  mutate(Acc = inhoop/12,
         Participant = paste(Participant, "TT", sep = ""))

#### > > Unequal ####
path <- c("data/previous/Asym/Unequal/")
results_files <- dir(path)

df_unequal <- tibble()
for(f in results_files){
  d <- read_csv(paste(path, f, sep = ""), col_types = cols())
  d$Participant <- strsplit(f, '[_]')[[1]][2]
  
  d <- d %>% 
    rename(Distance = Slab,
           inhoop = InHoop) %>% 
    mutate(Acc = inhoop/12,
           Participant = paste(Participant, "TT", sep = "")) %>% 
    select(Participant, Distance, inhoop, Acc)
  
  df_unequal <- rbind(df_unequal, d)
}

# tidy 
rm(d, f, path, results_files)

#### > Athletes ####
path <- c("data/previous/Athletes/")
results_files <- dir(path)

df_athletes <- tibble()

for(f in results_files){
  d <- read_csv(paste(path, f, sep = ""), col_types = cols())
  d <- d %>% 
    select(-c(Direction, Athlete)) %>% 
    rename(Distance = Slab,
           inhoop = Accuracy) %>% 
    mutate(Acc = inhoop/12,
           Participant = paste(Participant, "ath", sep = ""))
  df_athletes <- rbind(df_athletes, d)
}

# tidy 
rm(d, f, path, results_files)

#### > Clarke and Hunt ####
df_CH <- read_csv("data/previous/ClarkeHunt/Part1_session_data_CH.csv", col_types = cols())

# tidy
df_CH <- df_CH %>% 
  select(-Direction) %>%
  rename(Participant = `Subject Code`,
         Distance = Slab,
         inhoop = `Number in hoop`) %>% 
  mutate(Acc = inhoop/12,
         Participant = paste(Participant, "CH", sep = ""))

#### > Non-Naive ####
df_NN <- read_csv("data/previous/NonNaive/Part_1.csv", col_types = cols())

# tidy 
df_NN <- df_NN %>% 
  select(-Direction) %>% 
  rename(Distance = Slab,
         inhoop = InHoop) %>% 
  mutate(Acc = inhoop/12,
         Participant = paste(Participant, "NN", sep = ""))
  
 
#### > Misc ####
df_misc <- read_tsv("data/previous/Misc/Part1_session_data_Anca.txt", col_types = cols())

# tidy 
df_misc <- df_misc %>% 
  select(-c(Direction, Session)) %>% 
  rename(inhoop = Accuracy) %>% 
  mutate(Acc = inhoop/12,
         Participant = paste(Participant, "CH", sep = ""))

#### > Current AM data ####
df_AM <- read_csv("data/current/Session 2- Ability.csv", col_types = cols())

df_AM <- df_AM %>% 
  rename(inhoop = Total) %>%
  mutate(inhoop = 12 - inhoop,
         Acc = inhoop/12)

#### combine all data ####
df <- rbind(df_AM, df_athletes) %>% 
  rbind(df_aware) %>% 
  rbind(df_CH) %>% 
  rbind(df_HS) %>% 
  rbind(df_misc) %>% 
  rbind(df_NN) %>% 
  rbind(df_TT) %>% 
  rbind(df_unequal) 

# tidy 
rm(df_athletes, df_aware, df_CH, df_HS, df_misc, df_NN, df_TT, df_unequal)

# sort out the Acc column to not include 1 or 0 
df <- df %>% 
  mutate(beta_Acc = squash(Acc, 1, 0, 1e-5))

# maybe we average some values since some people did throws in each direction? 
# leave it for now... but it's something to think about

#### Initial plots ####
# make a plot to look at the overall trends in performance
df %>% 
  ggplot(aes(Distance, Acc)) +
  # inidividual participants
  geom_line(aes(fill = Participant),
            stat = "smooth", 
            method = glm,
            method.args = list(family = "binomial"),
            alpha = .3,
            fullrange = T) +
  # overall
  geom_smooth(method = glm,
              method.args = list(family = "binomial"),
              se = F,
              linetype = "dashed",
              size = 1.5) +
  theme_bw() + 
  theme(legend.position = "none")

#### Model ####
#### > Priors ####
m_priors <- c(set_prior("student_t(3, 4, 3)",
                        class = "Intercept"),
              set_prior("student_t(3, -10, 3)",
                        class = "b",
                        coef = "scaled_D"))

#### > Model ####
max_d <- max(df$Distance)
model_data <- df %>%
  drop_na() %>%
  mutate(scaled_D = Distance/max_d)

# m <- brm(beta_Acc ~ scaled_D + (scaled_D|Participant),
#          data = model_data,
#          family = "beta",
#          prior = m_priors,
#          chains = 1,
#          cores = 1,
#          iter = 2000,
#          warmup = 1000)
# 
# save(m, file = "scratch/models/m")
load("scratch/models/m")

#### Plot ####
#### > brms version ####
# plot(marginal_effects(m))

# Ok plot... but we can do better... 
# we should look at all the different participants and get estimates for where 
# they were roughly 50% accurate

#### > using tidybayes to get predictions ####
#### > > For everyone ####
# this is just who we have data for just now
# make a dataset with all distances 
# df_preds <- tibble(Participant = rep(unique(model_data$Participant), each = max_d),
#                    scaled_D = rep(seq(1,max_d,1)/max_d, length(unique(model_data$Participant))))
# df_preds <- df_preds %>% 
#   add_predicted_draws(m) %>% 
#   mutate(Distance = scaled_D * max_d)
# 
# # make a plot of this
# test <- df_preds %>% 
#   # create avg column 
#   group_by(Distance) %>%
#   mutate(
#     mu = mean(.prediction),
#     # upper = HDInterval::hdi(.prediction)[2],
#     # lower = HDInterval::hdi(.prediction)[1]
#   ) %>% 
#   group_by(Participant, Distance) %>% 
#   summarise(
#     mu_p = mean(.prediction),
#     mu = mean(mu),
#     # upper = mean(upper),
#     # lower = mean(lower)
#   )
# 
# test %>% 
#   ggplot(aes(Distance, mu_p, group = Participant)) + 
#   geom_path(alpha = .3,
#             aes(group = Participant)) + 
#   geom_path(colour = "blue",
#             aes(Distance, mu),
#             size = 1.5,
#             linetype = "dashed")

#### > > Just the current study ####
## read in session 2 data so we have a list of all participants 
df_part2 <- read_csv("data/current/Session 1 - Decisions.csv", col_types = cols())
df_preds <- tibble(Participant = rep(unique(df_part2$Participant), each = max_d + 1),
                   scaled_D = rep(seq(0, max_d, 1)/max_d, length(unique(df_part2$Participant))))
df_preds <- df_preds %>% 
  add_predicted_draws(m, allow_new_levels = T) %>% 
  mutate(Distance = scaled_D * max_d)

# create a new file 
df_exp_acc <- df_preds %>% 
  group_by(Distance) %>% 
  mutate(mu = mean(.prediction)) %>%
  group_by(Participant, Distance) %>% 
  summarise(mu_p = mean(.prediction),
            mu = mean(mu)) 

# Make a plot of this 
df_exp_acc %>% 
  ggplot(aes(Distance, mu_p, group = Participant)) + 
  geom_path(alpha = .3) + 
  geom_path(colour = "blue",
            aes(Distance, mu),
            size = 1.5,
            linetype = "dashed") + 
  theme_bw() + 
  scale_x_continuous("Estimated Accuracy")

# save this 
df_exp_acc <- df_exp_acc %>% 
  rename(p = mu_p) %>% 
  select(-mu)

save(df_exp_acc,
     file = "scratch/newData/df_bayes_expAcc")
