#------------------------------------------#
#       HOMEWORK 4: Graphic Language       #
#------------------------------------------#


# Author: Matias Strehl
# Perm number: 6811913

# Load packages 
library(tidyverse)
library(janitor)
library(dplyr)
library(lubridate)
library(magrittr)
library(ggplot2)

#-------------------#
#      PART 1       #
#-------------------#

# Read the data 

polls_adjusted <- read_csv("covid_approval_polls_adjusted.csv")

#-------------------#
#      PART 2       #
#-------------------#

# Part 2.a
# Create average approval by subject
polls_adjusted <- polls_adjusted %>%
                    filter(subject != "=======") %>%
                    group_by(subject) %>%
                    mutate(average_approval = mean(approve_adjusted, na.rm = T)) %>%
                    ungroup()

# Part 2.b
# First histogram of approval
approval_graph <- polls_adjusted %>%
  ggplot(aes(x = approve_adjusted )) +
  geom_histogram() +
  xlab("Approval") +
  ylab("Count") +
  labs(title = "Adjusted Approval Ratings") + 
  theme_minimal()
  
approval_graph

# Part 2.c
# Separated by subject
approval_graph_facet <- polls_adjusted %>%
  ggplot(aes(x = approve_adjusted, fill = subject )) +
  geom_histogram() +
  facet_wrap(vars(subject)) +
  xlab("Approval") +
  ylab("Count") +
  labs(title = "American approval of Biden and Trump’s response to coronavirus",
       subtitle = "From 2020-2022") + 
  labs(fill = "President") +
  theme_minimal()
  
approval_graph_facet

# Part 2.d
# Add vertical lines at the mean
mean_line <- approval_graph_facet +
  geom_vline(data = polls_adjusted, aes(xintercept = average_approval), linetype = "dashed"   )

mean_line

# Part 2.e 
# Changing colors
approval_final <- mean_line +
  scale_fill_manual(values=c("#008FD5", "#FF2700")) +
  theme(legend.position="bottom",
        legend.direction = "horizontal")

approval_final


#-------------------#
#      PART 3       #
#-------------------#

# Part 3.a
polls_q3 <- polls_adjusted %>%
  mutate(end_date = lubridate::mdy(enddate)) %>%
  mutate(approve_fraction = approve_adjusted/100) %>%
  filter(party == "D" | party == "R" | party == "I")

# Part 3.b
polls_q3 %>%
  ggplot(aes(x = end_date, y = approve_fraction, color = party)) +
  geom_point(alpha = 0.2) +
  theme_minimal() +
  labs(color = "Party") +
  labs(title = "MATIAS: Approval of President’s Handling of Covid-19 Pandemic",
       subtitle = "From 2020−2022") +
  xlab("") +
  ylab("") +
  scale_color_manual(values=c("#008FD5", "#77AB43", "#FF2700")) +
  geom_vline(data = polls_q3, aes(xintercept = lubridate::as_date("2021-01-20")), lty = "dashed") +
  geom_smooth() +
  scale_y_continuous(labels = scales::percent)



#-------------------#
#      PART 4       #
#-------------------#  
  
  
# Part 4.a
toplines <- read_csv("covid_approval_toplines.csv")

# Part 4.b
toplines_biden <- toplines %>%
  filter(subject == "Biden", 
         party == "D" | party == "R" | party == "I") %>%
  mutate(model_date = lubridate::mdy(modeldate))

# Part 4.c  
toplines_biden <- toplines_biden %>%
  mutate(party_description = 
           ifelse(party == "D", "Democrats", ifelse(party == "R", "Republicans", "Independents") ) ) %>%
  mutate(approve_estimate_frac = approve_estimate/100)

# Part 4.d
## loading in libraries in case they are not loaded in. ## install if necessary
library(ggrepel)
library(scales)

toplines_biden %>%
  mutate(label = ifelse(model_date ==  max(model_date),
                        party_description, NA_character_)) %>% ## do not need to modify 
  ggplot(aes(x = model_date, y = approve_estimate_frac, color = party)) + ## need to fill in
  geom_line() + ## do not need to modify
  geom_text_repel(aes(label = label),
                  nudge_x = 10, na.rm = T,
                  xlim = as_date(c("2022-07-01", "2022-10-01"))) + ## do not need to modify
  geom_vline(aes(xintercept = lubridate::as_date("2021-01-20")), lty = "dashed") + ## need to fill in
  annotate("text", x = as_date("2021-01-20"), y = 0.05,
           label = "Biden sworn into office", size = 3,
           hjust = -0.1) + ## do not need to modify
  scale_color_manual(values = c("#008FD5", "#77AB43", "#FF2700") ) + ## need to fill in
  scale_y_continuous(labels = scales::percent) + ## do not need to modify
  coord_cartesian(ylim = c(.1,1), clip = "off") + ## do not need to modify
  scale_x_date(limits = c(as_date("2020-12-01"), as_date("2022-10-01"))) + ## do not need to modify
  labs(title = "Do Americans approve of Biden response to the coronavirus crisis?",
       subtitle = "A calculation of the share of all Americans who approve of the handling of the coronavirus",
       x = "",
       y = "") + ## need to fill in
  theme_minimal() + ## do not need to modify
  theme(legend.position = "none") ## need to fill in






























