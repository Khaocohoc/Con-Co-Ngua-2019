
# To check frequency of type of stone artefacts from Con Co Ngua site 2013
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)

ccn1 <- read_excel(here("data", "Concongua.xlsx"), sheet = 'Sheet2')

ggplot(ccn1, aes(x = reorder(types, numbers),
               y = numbers, label = numbers)) +
  geom_bar(stat = "identity", color = "black", fill = "white") +
  geom_text(size = 5, color = "black", position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(text = element_text(size = 25)) +
  xlab("") +
  ylab("Frequency") +
  coord_flip()

# Mass correspoding to each kind of raw materials at Con Co Ngua site 2013
ccn <- read_excel(here("data", "Concongua.xlsx"), sheet = 'Sheet1')

ggplot(ccn, aes(x = reorder(Class, 
                       Mass),
                        y = Mass)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(position = position_jitter(width = 0.1, 
                                         height = 0)) +
  ylab(" Mass (g)") +
  theme_bw(base_size = 30)  +  
  xlab("") +
  coord_flip()

# Reordering varibales from smallest to biggiest values Mass correspoding to each kind of raw materials at Mau A 2015

ggplot(ccn, aes(x = reorder(Class, Mass),
               y = Mass)) + 
  geom_boxplot(outlier.shape = NA, size = 0.8) + 
  geom_sina(alpha = 0.9) +
  ylab("Mass (g)") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(angle = , 
                                   vjust = 0.1, 
                                   hjust = 0.05)) +
  scale_y_log10() +
  xlab("") +
  ylab("Mass (g)") +
  coord_flip()

# Data and plots for polished axes and axe preforms at Con Co Ngua site 2013

riumailuoi <- read_excel(here("data", "riumailuoi.xlsx"), sheet = 'Sheet1')

ggplot(na.omit(riumailuoi), 
       aes(x = Class, y = Mass, 
           fill = State)) +
  geom_boxplot(outlier.shape = NA, size = 0.9) +
  geom_jitter(size = 2) +
  theme_bw() +
  theme(text = element_text(size=30)) +
  geom_sina(alpha = 0.1) +
  ylim(0, 1500) +
  scale_y_log10() +
  xlab("Types of raw materials") +
  ylab("Mass (g)")

# To compare average MASS of polished axes and axe preforms at CCN 2013
polaxes <- read_excel(here("data", "riumailuoi.xlsx"), sheet = 'Sheet1')

polaxes %>% 
  filter(is.na(Context) == FALSE) %>% 
  ggplot(aes(x = reorder(Context, 
                         desc(Context)),
             y = Mass)) +
  geom_boxplot(outlier.shape = NA, size = 0.8) +
  geom_jitter(position=position_jitter(width=.1, height=0)) +
  theme_bw() +
  theme(text = element_text(size=30)) +
  geom_sina(alpha = 0.4) +
  xlab("Excavation contexts") +
  ylab("Mass (g)")  +
  coord_flip() 

# Summarising all metric attributes of polished axes and axe proforms at CCN 2013- PHASE 1
library(tidyverse)
p1 <- read_excel(here("data", "riumailuoi.xlsx"), sheet = 'phase1')

summary_stats_table <- function(x) {
  x %>% 
    select(Mass, 
           Length,  
           Width,     
           Thickness,     
           Scars) %>% 
    filter_all(all_vars(!is.na(.))) %>% 
    summarise_all(c("min", 
                    "mean", 
                    "max", 
                    "median", 
                    "IQR")) %>% 
    gather(key = "key", 
           value = "value") %>%
    separate(key, 
             into = c("measurement", 
                      "statistic"), 
             sep = "[_]") %>% 
    spread(statistic, 
           value)
  
}

summary_stats_table(p1)
  
# Summarising all metric attributes of Polished axes and Axe proforms at CCN 2013- PHASE 2

p2 <- read_excel(here("data", "riumailuoi.xlsx"), sheet = 'phase2')

summary_stats_table(p2)

# Boxplot of mass and state of Pestles
df <- read_excel(here("data", "chaynghien.xlsx"), sheet = 'Sheet1')
ggplot(df, aes(x = reorder(Class, 
                            Mass),
                y = Mass, fill = `State`)) + 
  geom_boxplot(outlier.shape = NA, size = 0.9) +
  geom_jitter(size = 2) +
  theme_bw() +
  theme(text = element_text(size=25)) +
  geom_sina(alpha = 0.1) +
  ylim(0, 10000) +
  scale_y_log10() +
  ylab("Mass (g)") +
  xlab("") +
  coord_flip()

# Reorder for Mass of Pestles and Layers

ggplot(df, 
       aes(x = reorder(Unit, desc(Unit)),
           y = Mass)) +
  geom_boxplot(size = 1) +
  geom_jitter() +
  geom_sina(alpha = 0.8) +
  theme_bw() +
  theme(text = element_text(size=28)) +
  ylab("Mass (g)") +
  xlab("Excavation contexts") +
  coord_flip()

# Summarising all metric attributes of polished axes and axe proforms at CCN 2013- PHASE 1
library(tidyverse)
p1 <- read_excel(here("data", "chaynghien.xlsx"), sheet = 'phase1')
p1$Scars <- p1$Number_of_scars

summary_stats_table(p1)

# Summarising all metric attributes of Pestles at CCN 2013- PHASE 2

p2 <- read_excel(here("data", "chaynghien.xlsx"), sheet = 'phase2')

summary_stats_table(p2)

# Summarising all metric attributes of Pestles at CCN 2013- PHASE 3

p3 <- read_excel(here("data", "chaynghien.xlsx"), sheet = 'phase3')
p3$Scars <- p3$Number_of_scars

summary_stats_table(p3)


### ANOVA FOR WIDTH OF PESTLES
# read in data
library(tidyverse)
df <- read_excel(here("data", "chaynghien.xlsx"), sheet = 'Sheet1')
# inspect data

names(df) <- make.names(names(df))
# Compute ANOVA
Width_Class_aov <- aov(Width ~ Class, data = df)
Width_Class_aov_tidy <- tidy(Width_Class_aov)

# Compute post-hoc test
Width_Class_aov_posthoc <- tidy(TukeyHSD(Width_Class_aov))

# Plot
Width_Class_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  coord_flip() +
  theme_bw()

### P_value of Mass
Width_Class_aov_tidy$p.value[1]

### The P_value of Mass
results = aov(Width ~ Class, data = df)
summary(results)

###ANOVA FOR THCIKNESS OF PESTLES

# read in data
df <- read_excel(here("data", "chaynghien.xlsx"), sheet = 'Sheet1')

names(df) <- make.names(names(df))

# Compute ANOVA
Thickness_Class_aov <- aov(Thickness ~ Class, data = df)
Thickness_Class_aov_tidy <- tidy(Thickness_Class_aov)

# Compute post-hoc test
Thickness_Class_aov_posthoc <- tidy(TukeyHSD(Thickness_Class_aov))

# Plot
Thickness_Class_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  coord_flip() +
  theme_bw()

### P_value of Mass
Thickness_Class_aov_tidy$p.value[1]

### The P_value of Mass
results = aov(Thickness ~ Class, data = df)
summary(results)

# read in data
df <- read_excel(here("data", "chaynghien.xlsx"), sheet = 'Sheet1')

names(df) <- make.names(names(df))

# Compute ANOVA
Length_Class_aov <- aov(Length ~ Class, data = df)
Length_Class_aov_tidy <- tidy(Length_Class_aov)

# Compute post-hoc test
Length_Class_aov_posthoc <- tidy(TukeyHSD(Length_Class_aov))

# Plot
Length_Class_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  coord_flip() +
  theme_bw()

### P_value of Mass
Length_Class_aov_tidy$p.value[1]

### The P_value of Mass
results = aov(Length ~ Class, data = df)
summary(results)

## ANOVA FOR LENGTH OF PESTLES: Phase II

# read in data
df <- read_excel(here("data", "chaynghien.xlsx"), sheet = 'phase2')

names(df) <- make.names(names(df))

# Compute ANOVA
Length_Class_aov <- aov(Length ~ Class, data = df)
Length_Class_aov_tidy <- tidy(Length_Class_aov)

# Compute post-hoc test
Length_Class_aov_posthoc <- tidy(TukeyHSD(Length_Class_aov))
# Plot
Length_Class_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  coord_flip() +
  theme_bw()

### P_value of Mass
Length_Class_aov_tidy$p.value[1]

### The P_value of Mass
results = aov(Length ~ Class, data = df)
summary(results)


### ANOVA FOR MORTAS OF CCN 2013

# read in data
ban <- read_excel(here("data", "bannghien.xlsx"), sheet = 'Sheet1') # BM: Error: does not exist!

names(ban) <- make.names(names(ban))

# Compute ANOVA
Length_Class_aov <- aov(Length ~ Class, data = ban)
Length_Class_aov_tidy <- tidy(Length_Class_aov)

# Compute post-hoc test
Length_Class_aov_posthoc <- tidy(TukeyHSD(Length_Class_aov))

# Plot
Length_Class_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  coord_flip() +
  theme_bw()

### P_value of Mass
Length_Class_aov_tidy$p.value[1]

### The P_value of Mass
results = aov(Length ~ Class, data = ban)
summary(results)

### ANOVA FOR WIDTH OF MORTARS

# read in data
ban <- read_excel(here("data", "bannghien.xlsx"), sheet = 'Sheet1') # BM: Error: does not exist!

names(ban) <- make.names(names(ban))

# Compute ANOVA
Width_Class_aov <- aov(Width ~ Class, data = ban)
Width_Class_aov_tidy <- tidy(Width_Class_aov)

# Compute post-hoc test
Width_Class_aov_posthoc <- tidy(TukeyHSD(Width_Class_aov))

# Plot
Width_Class_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  coord_flip()  +
  theme_bw()

### P_value of Mass
Width_Class_aov_tidy$p.value[1]

### The P_value of Mass
results = aov(Width ~ Class, data = ban)
summary(results)

### ANOVA FOR THICKNESS   OF MORTARS

# read in data
ban <- read_excel(here("data", "bannghien.xlsx"), sheet = 'Sheet1')

names(ban) <- make.names(names(ban))

# Compute ANOVA
Thickness_Class_aov <- aov(Thickness ~ Class, data = ban)
Thickness_Class_aov_tidy <- tidy(Thickness_Class_aov)

# Compute post-hoc test
Thickness_Class_aov_posthoc <- tidy(TukeyHSD(Thickness_Class_aov))

# Plot
Thickness_Class_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  coord_flip()   +
  theme_bw()

### P_value of Mass
Thickness_Class_aov_tidy$p.value[1]

### The P_value of Mass
results = aov(Thickness ~ Class, data = ban)
summary(results)

### ANOVA FOR MASS

# read in data
ban <- read_excel(here("data", "bannghien.xlsx"), sheet = 'Sheet1')

names(ban) <- make.names(names(ban))

# Compute ANOVA
Mass_Class_aov <- aov(Mass ~ Class, data = ban)
Mass_Class_aov_tidy <- tidy(Length_Class_aov)

# Compute post-hoc test
Mass_Class_aov_posthoc <- tidy(TukeyHSD(Mass_Class_aov))

# Plot
Mass_Class_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  coord_flip()  +
  theme_bw()

### P_value of Mass
Mass_Class_aov_tidy$p.value[1]

### The P_value of Mass
results = aov(Mass ~ Class, data = ban)
summary(results)

