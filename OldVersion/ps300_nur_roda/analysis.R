library(tidyverse)
library(readxl)

read_excel("raw_data_need for achievement.xlsx") %>%
  gather( 'key', 'value', -ps_code, -group, -gender, -age, -handedness) %>%
  glimpse() -> d

unique(d$key)

#####################################
# Measurement Task Data
#####################################
d %>% filter(str_detect(key, 'MT')) %>%
  mutate(
    distance = 0.3048 * as.numeric(str_extract(key, '[0-9]+')),
    accuracy = value / 12) %>%
  glimpse() -> d_mt

ggplot(d_mt, aes(x = distance, y = accuracy) ) + 
  geom_point() + 
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'), se = FALSE) + 
  facet_wrap(~ ps_code) +
  theme_bw()
ggsave("nur_roda_mt.png", width = 8, height = 8)

#####################################
# One Target Task Data
#####################################
d %>% filter(str_detect(key, '1T')) %>%
	mutate(key = if_else(
		str_detect(key, "standpos"), "standing_position", "accuracy")) -> d_1t

d_1t %>% filter(key == "standing_position") %>%
	rename(standing_position = "value") %>%
	select(-key) %>%
	mutate(standing_position = 0.3048 * abs(standing_position - 30)) -> d_1t_s

d_1t %>% filter(key == "accuracy") %>%
	rename(accuracy = "value") %>%
	select(-key) -> d_1t_a

left_join(d_1t_s, d_1t_a) %>%
	group_by(ps_code, group, gender) %>%
	summarise(
		mean_acc = mean(accuracy),
		mean_standing_position = mean(standing_position),
		median_standing_position = median(standing_position),
		first_standing_position = standing_position[1]) -> d_1t

rm(d_1t_a, d_1t_s)

ggplot(d_1t, aes(x = mean_standing_position, fill = gender)) + 
geom_histogram(alpha = 0.5, binwidth= 2, position="dodge", colour = "black") +
scale_fill_viridis_d() + facet_wrap(~ gender)
ggsave("nur_roda_1t1.png", width = 8, height = 8)

ggplot(d_1t, aes(x = mean_standing_position)) + 
geom_histogram(bins = 10, alpha = 0.5, binwidth= 2, position="dodge", colour = "black") +
scale_fill_viridis_d() 
ggsave("nur_roda_1t1_2.png", width = 8, height = 8)

ggplot(d_1t, aes(x = mean_standing_position, y = mean_acc, colour = gender)) + geom_jitter(size = 3, height = 0.02) +
	scale_y_continuous(breaks = c(0, 1/3, 2/3, 1),labels = c("0/3", "1/3", "2/3", "3/3"))
ggsave("nur_roda_1t2.png", width = 8, height = 8)

ggplot(d_1t, aes(x = first_standing_position, y = mean_standing_position)) + geom_point()
#####################################
# Two Target Task Data
#####################################

d %>% filter(str_detect(key, '2T_[YBGR]_standpos')) %>%
	rename(standing_position = "value") %>%
	mutate(
		hoop = str_extract(key, '[RYBG]'),
		hoop_delta = case_when(
			hoop == "R" ~ 1.9,
			hoop == "Y" ~ 3.24,
			hoop == "G" ~ 6.04,
			hoop == "B" ~ 7.04,
			),
		standing_position = 0.3048 * abs(standing_position - 30),
		normalised_position = standing_position/hoop_delta) %>%
	select(-key) -> d_2t_s 

d %>% filter(str_detect(key, '2T_[YBGR]_acc')) %>%
	rename(accuracy = "value") %>%
	select(-key) -> d_2t_a

d_2t <- full_join(d_2t_s, d_2t_a)

ggplot(d_2t, aes(x = hoop, y = normalised_position)) + geom_jitter()

d_2t %>% group_by(ps_code, group, gender, hoop_delta) %>%
	summarise(
		mean_n_postition = mean(normalised_position),
		accuracy = mean(accuracy)) %>%
	full_join(d_1t) -> dc

ggplot(dc, aes(x = mean_standing_position, y = mean_n_postition)) +
	geom_jitter() + facet_wrap(~ hoop_delta) + geom_smooth(method = lm)

ggplot(dc, aes(x = hoop_delta, y = mean_n_postition)) + 
	geom_point() + geom_path() + 
	facet_wrap(~ paste(mean_standing_position, sep = "-"))
ggsave("nur_roda_2t.png")

dc 	%>%
	spread(hoop_delta, mean_n_postition) %>%
	mutate(pos_change = `7.04` - `1.9`) %>%
	full_join(d_1t) -> dc2

ggplot(dc2, 
	aes(x = mean_standing_position, y = pos_change)) + 
geom_jitter(size = 3) + geom_smooth(method = "lm") + theme_bw()
ggsave("nur_roda_2t_change.png")

m <- aov(data = dc, mean_n_postition ~ hoop_delta * mean_standing_position)
summary(m)

summary(lm(data = dc2, pos_change ~ mean_standing_position))


summary(lm(data = dc, pos_change ~ mean_standing_position))

