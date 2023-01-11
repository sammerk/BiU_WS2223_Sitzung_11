# Import der Daten #############################################################
library(haven)
data_troeger_etal <- read_sav("data_troeger_etal.sav")

# Rekodierung der UV `bed` als Faktor
library(tidyverse)
data_troeger_etal <- data_troeger_etal %>% 
  mutate(bes = as.factor(bed))

# Übung 1 ######################################################################

# Visueller Überblick `income.t1`
ggplot(data_troeger_etal, aes(bed, income.t1)) +
  geom_boxplot() +
  geom_jitter()

# Visueller Überblick `pol_orient.t1`
library(ggplot2)
ggplot(data_troeger_etal, aes(bed, pol_orient.t1)) +
  geom_boxplot() +
  geom_jitter()

# Effektstärken
library(effsize)
cliff.delta(income.t1 ~ bed, data = data_troeger_etal)
cliff.delta(pol_orient.t1 ~ bed, data = data_troeger_etal)

# p-Werte
t.test(income.t1 ~ bed, data = data_troeger_etal)
t.test(pol_orient.t1 ~ bed, data = data_troeger_etal)

# BF
library(BayesFactor)
ttestBF(formula = income.t1 ~ bed, 
        data = data_troeger_etal %>% 
          select(bed, income.t1) %>% 
          na.omit() # ttestBF()  erfordert non-missing data
        )

ttestBF(formula = pol_orient.t1 ~ bed, 
        data = data_troeger_etal %>% 
          select(bed, pol_orient.t1) %>% 
          na.omit() # ttestBF()  erfordert non-missing data
)


# Übung 2 ######################################################################
# Grafischer Überblick
ggplot(data_troeger_etal %>% 
         filter(bed == 1), aes(s.t1.raw)) +
  geom_histogram()
ggplot(data_troeger_etal %>% 
         filter(bed == 1), aes(s.t2.raw)) +
  geom_histogram()
ggplot(data_troeger_etal %>% 
         filter(bed == 1), aes(s.t3.raw)) +
  geom_histogram()


# Effektstärke Kendall's W
## Erstellung long data
data_troeger_etal_long <- 
  data_troeger_etal %>% 
  select(ends_with("raw"), case, bed) %>% 
  gather("time", "s", -case, -bed) 


## Erstellung complete data
data_troeger_etal_long_complete <- data_troeger_etal_long %>% 
  na.omit() %>%  # remove NA
  # count measurment occasion per person 
  group_by(case) %>%
  mutate(occasions_per_person = n()) %>% 
  ungroup() %>% 
  # exclude non complete measurements 
  filter(occasions_per_person == 3) %>% 
  # filter only intervention group
  filter(bed == 1) %>% 
  # coerce case and time to factor
  mutate(case = as.factor(case),
         time = as.factor(time))

# Computing Effsize
## Kendall's W
library(rstatix)
friedman_effsize(data = data_troeger_etal_long_complete,
                 s ~ time | case)

# p- value
## rm ANOVA
summary(
  aov(s ~ time + Error(case / time),
      data = data_troeger_etal_long_complete)
)

## Friedman Test
friedman.test(data = data_troeger_etal_long_complete,
              s ~ time | case)


# Übung 3 ######################################################################
# Grafischer Überblick
ggplot(data_troeger_etal,
       aes(as.factor(bed), s.t2.raw)) +
  geom_boxplot() +
  geom_jitter()

# Effektstärke
cliff.delta(s.t2.raw ~ bed, data = data_troeger_etal)

# p-Wert
wilcox.test(s.t2.raw ~ bed, data = data_troeger_etal)
