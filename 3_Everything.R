#### Achievement Motivation ####
# This script produces plots and summaries for the Achievement Motivation study 

#### library ####
# install.packages("tidyverse")
library(tidyverse)

#### load in data ####
# Accuracy estimation part 
# load is used for files that were created in R
load("scratch/df_part1")

# Decision part
# read in a csv file
df_part2 <- read.csv("data/Decisions/Session 1 - Decisions.csv")
# load an R file
load("scratch/df_exp_acc")

df_exp_acc <- df_exp_acc %>% 
  mutate(Distance_m = Distance * 0.46,
         Distance_ft = Distance_m * 3.2808)

#### Process data ####
# combine expected accuracy with decision data to get a measure of how likely particiapnts 
# were to hit the target given their standing position
df_part2 %<>% 
  mutate(
    # get absolute value for distance from target
    Distance = abs(Position - 20),
    Distance_m = Distance * 0.46, # multiply by size of the slabs to get metres
    Distance_ft = Distance_m  * 3.2808,
    # define the Groups
    Group = ifelse(Participant %% 2 == 0, "Best", "Performance"), 
    # Sorts the labelling so it matches across data frames
    Participant = as.factor(Participant),
    Hit = ifelse(Accuracy == "Yes", "True", "False")) %>% 
  # merge with expected accuracy dataframe
  merge(df_exp_acc) %>% 
  # make a sensible label
  mutate(Expected_Acc = p) %>% 
  # remove bad label
  select(-p, -Accuracy)

# Find the point at which each participant was ~50% accuracte
d_switch_line <- df_exp_acc %>%
  # Group so we find values for each participant
  group_by(Participant) %>% 
  # keep only the rows that are closest to 50% accuracy
  filter(abs(p - .5) == min(abs(p - .5)))


#### Plotting ####
# NB: anytime "Distance" is type, this can be changed to;
#     - Distance_m = Distance in metres
#     - Distance_ft = Ditsance in feet
#     - Expected_Acc = Chance of success from that distance
# This might be useful when comparing to the original paper since that was in feet?
# Also, if you do this, you will need to change the names of the axes

# plot accuracy over distance for first part 
# using ggplot, we can build up a plot by adding different layers 

# first, create an object that has the axes of the plot defined
plt_accuracy <- ggplot(data = df_part1,    # give it the data frame
                       aes(Distance, Acc)) # define the x and y axes
# now add the other details to the plt object we created
# add in dots to show raw accuracy over distance
plt_accuracy <- plt_accuracy + geom_point()
# add in a smoothed line based on a logistic regression 
# the logistic regression was used to estimate accuracy for the distances we didn't test 
plt_accuracy <- plt_accuracy + geom_smooth(data = df_exp_acc, # specify new data
                                           aes(y = p),        # define the y axis
                                           method = glm,      # setup the model part
                                           method.args = list(family = binomial),
                                           se = F)
# make a facet for each participant
plt_accuracy <- plt_accuracy + facet_wrap(~Participant)
# do some housekeeping to tidy up the plots 
plt_accuracy <- plt_accuracy + 
  # relabel the axes to make sense
  scale_y_continuous("Accuracy",
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous("Distance from Target (slabs)", 
                     limits = c(0,20)) +
  # change the theme
  theme_bw()

# show the plot 
plt_accuracy

# save this 
ggsave(file = "scratch/plots/plt_accuracy.png",
       width = 5, 
       height = 6)

# Plot decision data 
plt_decisions_trial <- ggplot(data = df_part2,
                              aes(x = Trial,
                                  y = Distance)) # y can be changed between Distance and Expected_Acc
                                   
# add in our dots and line
plt_decisions_trial <- plt_decisions_trial +
  geom_line() +
  geom_point(aes(colour = Hit,
                 shape = Group)) 
# add dashed line to show where 50% was
plt_decisions_trial <- plt_decisions_trial + geom_hline(data = d_switch_line, 
                                                        aes(yintercept = Distance),
                                                        linetype = "dashed")
# make facet for each participant
plt_decisions_trial <- plt_decisions_trial + facet_wrap(~Participant)
# tidy up a bit 
plt_decisions_trial <- plt_decisions_trial + 
  # relabel the axes to make sense
  scale_y_continuous("Distance from Target (slabs)") +
  scale_x_continuous("Trial") +
  theme_bw()
# show plot
plt_decisions_trial

# save this 
ggsave(file = "scratch/plots/plt_decisions_trial.png",
       width = 8, 
       height = 6)

# make a histogram for all trials 
plt_hist <- ggplot(data = df_part2, 
                   aes(Distance, 
                       colour = Group,
                       fill = Group)) 
# add in bars 
# the default is to stack the bars, but we don't want that here
# ignore the warnings... it's just saying you should define the binwidth
# but the default looks nice enough
plt_hist <- plt_hist + geom_histogram(position = "dodge")
# sort labes and tidy 
plt_hist <- plt_hist + 
  theme_bw() + 
  scale_x_continuous("Distance from Target (slabs)")
# show the plot 
plt_hist

# save this 
ggsave(file = "scratch/plots/plt_hist.png",
       width = 8, 
       height = 6)

# recreate the atkinson plot 
# basically just a line version of the histogram plot above 
plt_line <- df_part2 %>% 
  group_by(Group, Distance) %>%
  summarise(count = n()) %>% 
  ggplot(aes(Distance, count, colour = Group)) + 
  geom_line() + 
  see::scale_color_flat() + 
  # scale_x_continuous("Expected Accuracy", labels = scales::percent_format(accuracy = 1)) +
  theme_bw()
plt_line

# save this 
ggsave(file = "scratch/plots/Atkinson_version_plot.png",
       width = 8,
       height = 6)

# a version that was averaged for each participant 
# but this probably won't work the way we want it to... 
# plt_line_avg <- df_part2 %>% 
#   group_by(Participant, Group) %>% 
#   summarise(Exp_acc = mean(Expected_Acc)) %>% 
#   ungroup() %>% 
#   group_by(Group, Exp_acc) %>% 
#   summarise(count = n())

#### Basic t tests ####
# first, we need to get some summary data
df_ttest <- df_part2 %>% 
  group_by(Participant, Group) %>% 
  summarise(mu_Distance = mean(Distance),
            mu_Distance_m = mean(Distance_m), 
            mu_Distance_ft = mean(Distance_ft),
            mu_Expected_Acc = mean(Expected_Acc))

# save this for SPSS 
write.table(df_ttest, file = "scratch/df_ttest.txt", row.names = F)

# show the data
df_ttest

# now do the t tests
# might be slightly different to SPSS output since a Welch correction is applied 
# https://en.wikipedia.org/wiki/Welch%27s_t-test
t.test(df_ttest$mu_Distance ~ df_ttest$Group) # with correction
t.test(df_ttest$mu_Distance ~ df_ttest$Group, var.equal = T) # without correction

# again, mu_Ditsance can be swapped out to compare other measures... 
# but they should always come up with the same stuff

