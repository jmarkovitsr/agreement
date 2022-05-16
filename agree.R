
#agreement

# by Jennifer
library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(here)

# cargar base de datos
df= read_csv('data/qp.csv')
# mean ept

df %>%
  group_by(group) %>%
  summarise(mean_ept = mean(answer, na.rm = T), sd_ept = sd(answer, na.rm = T))

# plot promedios

df %>%
  select(answer, group) %>%
  filter(answer == 1) %>%
  group_by(answer, group) %>%
  summarise(count=n())  %>%
  ggplot(aes(x = group, y = count, fill = group)) +  # cuando estoy en ggplot cambio de la %>% al +
  geom_bar(position="dodge",stat = "identity", width=0.5) 
facet_wrap(~group) +
  theme(legend.position="none")

# mean by frequency condition

df %>% 
  group_by(., group, Frequent) %>% 
  summarize(., accuracy = mean(na.omit(answer)), sd = sd(na.omit(answer)))



# plot by condition 

df %>%
  mutate(., lang = recode(lang, `0` = "h", `1` = "s")) %>%
  ggplot(., aes(x = native, y = answer, shape = Frequent )) + 
  facet_grid(. ~ lang) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  labs(x = 'Frequency', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 1: Proportion of accurate EPT responses across conditions') + 
  theme_grey(base_size = 16, base_family = 'Times')



#modelo

mod1 <- glmer( 
  answer ~ lang + native + Frequent + mass +
    (1 | participant) + 
    (1 | item),
  data = df, 
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(mod1)

# modelo con mint

mylogit = glm(answer ~ 0 + MINT , data = df, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit) 

# fct agreement

# cargar base de datos

df1= read_csv('data/qp2.csv')

df1$answer <- as.numeric(as.character(df1$answer))

# mean ept

df1 %>%
  group_by(group) %>%
  summarise(mean_fct = mean(answer, na.rm = T), sd_fct = sd(answer, na.rm = T))


df1 %>% 
  group_by(., group, Frequent ) %>% 
  summarize(., accuracy = mean(na.omit(answer)), sd = sd(na.omit(answer)))

# plot promedios

df1 %>%
  select(answer, group) %>%
  filter(answer == 1) %>%
  group_by(answer, group) %>%
  summarise(count=n())  %>%
  ggplot(aes(x = group, y = count, fill = group)) +  # cuando estoy en ggplot cambio de la %>% al +
  geom_bar(position="dodge",stat = "identity", width=0.5) 
facet_wrap(~group) +
  theme(legend.position="none")

# mean by frequency condition

df1 %>% 
  group_by(., group, Frequent) %>% 
  summarize(., accuracy = mean(na.omit(answer)), sd = sd(na.omit(answer)))



# plot by condition 

df1 %>%
  mutate(., lang = recode(lang, `0` = "h", `1` = "s")) %>%
  ggplot(., aes(x = native, y = answer, shape = Frequent )) + 
  facet_grid(. ~ lang) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  labs(x = 'Frequency', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 1: Proportion of accurate FCT responses across conditions') + 
  theme_grey(base_size = 16, base_family = 'Times')


df1 %>%
  mutate(., lang = recode(lang, `0` = "h", `1` = "s")) %>%
  ggplot(., aes(x = native, y = answer, shape = Frequent )) + 
  facet_grid(. ~ lang) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +


#modelo

mod <- glmer( 
  answer ~  + mass +
    (1 | participant) + 
    (1 | item),
  data = df,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(mod)

# modelo con mint

mylogit = glm(answer ~ 0 + MINT , data = df, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit) 


