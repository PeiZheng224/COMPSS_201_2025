# This is a classic example from Wooldridge
# Code by Andrew Heiss (modified by Max)

#In 1980, Kentucky raised its cap on weekly earnings that were covered by worker’s compensation. We want to know if this new policy caused workers to spend more time unemployed. If benefits are not generous enough, then workers could sue companies for on-the-job injuries, while overly generous benefits could cause moral hazard issues and induce workers to be more reckless on the job, or to claim that off-the-job injuries were incurred while at work.

#The main outcome variable we care about is log_duration (in the original data as ldurat, but we rename it to be more human readable), or the logged duration (in weeks) of worker’s compensation benefits. We log it because the variable is fairly skewed—most people are unemployed for a few weeks, with some unemployed for a long time. The policy was designed so that the cap increase did not affect low-earnings workers, but did affect high-earnings workers, so we use low-earnings workers as our control group and high-earnings workers as our treatment group.

rm(list = ls()) # clear memory
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2025/COMPSS_201_2025/lecture_11")
# if you are on jupyter, use this one: 
# setwd("~/COMPSS_201_2025/lecture_11")
library(pacman)
p_load(tidyverse, broom, scales, modelsummary)

# Load the data.
# It'd be a good idea to click on the "injury_raw" object in the Environment
# panel in RStudio to see what the data looks like after you load it
injury_raw <- read_csv("injury.csv")

# Let's look at just Kentucky and rename some things
injury <- injury_raw %>%
  filter(ky == 1) %>%
  # The syntax for rename is `new_name = original_name`
  rename(duration = durat, log_duration = ldurat,
         after_1980 = afchnge)

# Look at distribution of UE benefits for low versus high earners. 
ggplot(data = injury, aes(x = duration)) +
  # binwidth = 8 makes each column represent 2 months (8 weeks)
  # boundary = 0 make it so the 0-8 bar starts at 0 and isn't -4 to 4
  geom_histogram(binwidth = 8, color = "white", boundary = 0) +
  facet_wrap(vars(highearn))

# If we log this thingm things are visually less skewed

ggplot(data = injury, mapping = aes(x = log_duration)) +
  geom_histogram(binwidth = 0.5, color = "white", boundary = 0) +
  # Uncomment this line if you want to exponentiate the logged values on the
  # x-axis. Instead of showing 1, 2, 3, etc., it'll show e^1, e^2, e^3, etc. and
  # make the labels more human readable
  # scale_x_continuous(labels = trans_format("exp", format = round)) +
  facet_wrap(vars(highearn))


# Do the before and after plot
ggplot(data = injury, mapping = aes(x = log_duration)) +
  geom_histogram(binwidth = 0.5, color = "white", boundary = 0) +
  facet_wrap(vars(after_1980))
# Not much to see here

# But what about a before and after distributional graph by group?
ggplot(injury, aes(x = factor(highearn), y = log_duration)) +
  stat_summary(geom = "pointrange", size = 1, color = "red",
               fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  facet_wrap(vars(after_1980))
# That is a nice diff-in-diff plot. 

# But we can make this even nicer!
plot_data <- injury %>%
  # Make these categories instead of 0/1 numbers so they look nicer in the plot
  mutate(highearn = factor(highearn, labels = c("Low earner", "High earner")),
         after_1980 = factor(after_1980, labels = c("Before 1980", "After 1980"))) %>%
  group_by(highearn, after_1980) %>%
  summarize(mean_duration = mean(log_duration),
            se_duration = sd(log_duration) / sqrt(n()),
            upper = mean_duration + (1.96 * se_duration),
            lower = mean_duration + (-1.96 * se_duration))

ggplot(plot_data, aes(x = highearn, y = mean_duration)) +
  geom_pointrange(aes(ymin = lower, ymax = upper),
                  color = "darkgreen", size = 1) +
  facet_wrap(vars(after_1980))

# Prof. Max likes the following way even better. 
ggplot(plot_data, aes(x = after_1980, y = mean_duration, color = highearn)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  # The group = highearn here makes it so the lines go across categories
  geom_line(aes(group = highearn))

# Now we can do the diff in diff by hand!
diffs <- injury %>%
  group_by(after_1980, highearn) %>%
  summarize(mean_duration = mean(log_duration),
            # Calculate average with regular duration too, just for fun
            mean_duration_for_humans = mean(duration))
diffs

# The  next bit is clunky! Pull some numbers out of this table. 
before_treatment <- diffs %>%
  filter(after_1980 == 0, highearn == 1) %>%
  pull(mean_duration)

before_control <- diffs %>%
  filter(after_1980 == 0, highearn == 0) %>%
  pull(mean_duration)

after_treatment <- diffs %>%
  filter(after_1980 == 1, highearn == 1) %>%
  pull(mean_duration)

after_control <- diffs %>%
  filter(after_1980 == 1, highearn == 0) %>%
  pull(mean_duration)

diff_treatment_before_after <- after_treatment - before_treatment
diff_treatment_before_after


diff_control_before_after <- after_control - before_control
diff_control_before_after


diff_diff <- diff_treatment_before_after - diff_control_before_after
diff_diff


# Pain in the neck. Can we do this wit a regression?

model_small <- lm(log_duration ~ highearn + after_1980 + highearn * after_1980,
                  data = injury)
tidy(model_small)

# One advantage to using regression for diff-in-diff is that we can include control variables to help isolate the effect. For example, perhaps claims made by construction or manufacturing workers tend to have longer duration than claims made workers in other industries. Or maybe those claiming back injuries tend to have longer claims than those claiming head injuries. We might also want to control for worker demographics such as gender, marital status, and age.
# Let's include male, married, age, hosp (dummy),, indust (categ 1, 2, 3), injtype (1-8), lprewage (log of wage prior to filing  a claim). 
# Important: indust and injtype are in the dataset as numbers (1-3 and 1-8), but they’re actually categories. We have to tell R to treat them as categories (or factors), otherwise it’ll assume that you can have an injury type of 3.46 or something impossible.

injury_fixed <- injury %>%
  mutate(indust = as.factor(indust),
         injtype = as.factor(injtype))

model_big <- lm(log_duration ~ highearn + after_1980 + highearn * after_1980 +
                  male + married + age + hosp + indust + injtype + lprewage,
                data = injury_fixed)
tidy(model_big)

diff_diff_controls <- tidy(model_big) %>%
  filter(term == "highearn:after_1980") %>%
  pull(estimate)

modelsummary(list("Simple" = model_small, "Full" = model_big))


