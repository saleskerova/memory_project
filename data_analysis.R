library(boot)
library(Boruta)
library(car)
library(caret)
library(effects)
library(emmeans)
library(flextable)
library(foreign)
library(ggfortify)
library(ggpubr)
library(here)
library(Hmisc)
library(knitr)
library(lme4)
library(MASS)
library(mclogit)
library(mlogit)
library(msm)
library(MuMIn)
library(nlme)
library(ordinal)
library(performance)
library(QuantPsyc)
library(readxl)
library(remotes)
library(report)
library(reshape2)
library(rms)
library(robustbase)
library(sandwich)
library(sjPlot)
library(tibble)
library(tidyverse)
library(vcd)
library(vip)
library(visreg)

library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(car)
library(data.table)


# V1: Object recognition ------------------------------------------------------

# Data Prep ---------------------------------------------------------------
setwd('C:/Users/saral/Google ????/0-Year 2/Thesis/Internship/Experiment/data_collected/retrieval_data_V1')

v1_files <- list.files(pattern="*.csv")

df_v1 <- lapply(v1_files, 
               read.table,
               header = TRUE,
               sep = ';',
               stringsAsFactors = FALSE)

df_v1 <- do.call(rbind, df_v1)

names(df_v1)[1] <- "sid"
# df_v1_m <- df_v1_m %>% relocate('participant', .before = 'obj')


## Sector_memory
df_v1 <- df_v1 %>% 
  mutate(type_m = case_when(
    endsWith(sector, "LM") ~ "low_m",
    endsWith(sector, "MM") ~ "medium_m",
    endsWith(sector, "HM") ~ "high_m"
  ))

# V1: Performance -------------------------------------------------------------

df_v1 <- df_v1 %>% 
  mutate(resp_m.corr = if_else(cor_ans == resp_m, 1, 0))

## via data.table
# df_v1 <- as.data.table(df_v1)
# v1_acc_mean_type <- df_v1[ , sum(acc)*100/length(acc), by = .(type_m)]
# v1_acc_mean_sid <- df_v1[ , sum(acc)*100/length(acc), by = .(sid)]
# v1_acc_mean_all <- df_v1[ , sum(acc)*100/length(acc), by = .(type_m, sid)]

## via dplyr (first arrange by order, then calculate mean)

## arrange in ascending order by type
df_v1 <- df_v1 %>% 
  mutate(type_m = factor(type_m, 
                         levels = c('low_m', 'medium_m', 'high_m'))) %>% 
  arrange(type_m)

## calculate mean by type and sid
v1_acc <- df_v1 %>%
  group_by(sid, type_m) %>%
  summarise(acc = round( (sum(resp_m.corr) / n() )*100, 2))


## Outliers check
v1_acc_sid <- df_v1 %>%
  group_by(sid) %>%
  summarise(acc = round( (sum(resp_m.corr) / n() )*100, 2))

plot(acc ~ sid, data = v1_acc_sid, main="Accuracy by participant",
     xlab= "Paricipants", ylab="Performance rate", pch=19)

# V1: Confidence rating ----------------------------------------------------

#for each sound, a scale of 5. 
#Mean confidence = sum of confidence responses per type and divide by number of trials).

## via data.table
conf_mean <- df_v1_m[ , sum(resp_conf)/length(resp_conf), by = .(type_m)]

## via dplyr
v1_conf <- df_v1_m %>%
  group_by(type_m) %>%
  summarise(conf = round( (sum(resp_conf) / n() ), 2))

## arrange in ascending order by type
v1_conf <- v1_conf %>% 
  mutate(type_m = factor(type_m, 
                         levels = c('low_m', 'medium_m', 'high_m'))) %>% 
  arrange(type_m)




# V2: Free recall ------------------------------------------------------------


# V2: data prep -----------------------------------------------------------

setwd('C:/Users/saral/Google ????/0-Year 2/Thesis/Internship/Experiment/data_collected/retrieval_data_V2')

v2_files <- list.files(pattern="*.csv")


df_v2_m <- lapply(v2_files, 
                read.csv,
                header = T,
                sep = ';',
                stringsAsFactors = FALSE)

df_v2_m <- do.call(rbind, df_v2_m)

names(df_v2_m)[1] <- "mask"


#df_v2_m <-  df_v2_m %>% select('participant', 'mask', 'openq_resp.text', 'resp_m.corr', 'resp_conf.keys')



# V2: Performance ---------------------------------------------------------


df_v2_m <- df_v2_m %>% 
  mutate(type_m = case_when(
    endsWith(sector, "LM") ~ "low_m",
    endsWith(sector, "MM") ~ "medium_m",
    endsWith(sector, "HM") ~ "high_m"
  ))

df_v2_m <- as.data.table(df_v2_m)
#acc_mean_new <- df_fr[ , sum(resp_m.corr)*100/length(resp_m.corr), by = .(participant)]


#final
df_v2_m <- df_v2_m %>% 
  mutate(type_m = factor(type_m, 
                         levels = c('low_m', 'medium_m', 'high_m'))) %>% 
  arrange(type_m)


v2_acc <- df_v2_m %>%
  group_by(participant, type_m) %>%
  summarise(acc = round( (sum(resp_m.corr) / n() )*100, 2))





# by type
v2_acc <- df_v2_m %>%
  group_by(type_m) %>%
  summarise(acc = round( (sum(resp_m.corr) / n() )*100, 2))

# arrange in ascending order by type
v2_acc <- v2_acc %>% 
  mutate(type_m = factor(type_m, 
                         levels = c('low_m', 'medium_m', 'high_m'))) %>% 
  arrange(type_m)

# by participant 
v2_acc_sid <- df_v2_m %>%
  group_by(participant) %>%
  summarise(acc = round( (sum(resp_m.corr) / n() )*100, 2))



## V2: Performance - Free recall 

df_v2_m <- df_v2_m %>% 
  separate(openq_resp.text, c("color", "frame"))

df_v2_m <-df_v2_m %>% 
  mutate(mask_color = case_when(
    grepl('blue',mask) ~ 'blue', 
    grepl('yellow',mask) ~ 'yellow',
    grepl('pink',mask) ~ 'pink', 
    grepl('grey',mask) ~ 'grey',
  ))

df_v2_m <-df_v2_m %>% 
  mutate(mask_frame = case_when(
    grepl('noframe',mask) ~ 'null', 
    grepl('frame',mask) ~ 'straight',
    grepl('frame2',mask) ~ 'wiggly'
  ))

df_v2_m <- df_v2_m %>% 
  mutate(color_acc = if_else(color == mask_color, 1, 0))
df_v2_m <-df_v2_m %>% 
  mutate(frame_acc = if_else(frame == mask_frame, 1, 0))


df_v2_m <- as.data.table(df_v2_m)
acc_color_sid <- df_v2_m[ , sum(color_acc)*100/length(color_acc), by = .(participant)]
acc_frame_sid <- df_v2_m[ , sum(frame_acc)*100/length(frame_acc), by = .(participant)]



# Accuracy for color
acc_color <- df_v2_m %>%
  group_by(type_m) %>%
  summarise(acc_clr = round( (sum(color_acc) / n() )*100, 2))

# arrange in ascending order by type
acc_color <- acc_color %>% 
  mutate(type_m = factor(type_m, 
                         levels = c('low_m', 'medium_m', 'high_m'))) %>% 
  arrange(type_m)


# Accuracy for frame
acc_frame <- df_v2_m %>%
  group_by(type_m) %>%
  summarise(acc_frm = round( (sum(frame_acc) / n() )*100, 2))

# arrange in ascending order by type
acc_frame <- acc_frame %>% 
  mutate(type_m = factor(type_m, 
                         levels = c('low_m', 'medium_m', 'high_m'))) %>% 
  arrange(type_m)



# Correct trials

v2_corr_df = df_v2_m[df_v2_m$resp_m.corr == '1', ]


# Accuracy of colors for correct trials
acc_color_corr <- v2_corr_df %>%
  group_by(type_m) %>%
  summarise(acc_clr = round( (sum(color_acc) / n() )*100, 2))

# arrange in ascending order by type
acc_color_corr <- acc_color_corr %>% 
  mutate(type_m = factor(type_m, 
                         levels = c('low_m', 'medium_m', 'high_m'))) %>% 
  arrange(type_m)


# Accuracy of frames for correct trials
acc_frame_corr <- v2_corr_df %>%
  group_by(type_m) %>%
  summarise(acc_frm = round( (sum(frame_acc) / n() )*100, 2))

# arrange in ascending order by type
acc_frame_corr <- acc_frame_corr %>% 
  mutate(type_m = factor(type_m, 
                         levels = c('low_m', 'medium_m', 'high_m'))) %>% 
  arrange(type_m)




#via data.table
# acc_color <- df_v2_m[ , sum(color_acc)*100/length(color_acc), by = .(type_m)]
# acc_frame <- df_v2_m[ , sum(frame_acc)*100/length(frame_acc), by = .(type_m)]


# V2: Confidence rating ---------------------------------------------------

v2_conf <- df_v2_m %>%
  group_by(type_m) %>%
  summarise(conf = round( (sum(resp_conf.keys) / n() ), 2))


## arrange in ascending order by type
v2_conf <- v2_conf %>% 
  mutate(type_m = factor(type_m, 
                         levels = c('low_m', 'medium_m', 'high_m'))) %>% 
  arrange(type_m)


#via data.table
#conf_mean_new <-df_fr[ , sum(resp_conf.keys)/length(resp_conf.keys), by = .(type_m)]



# V3: Object recognition + source memory --------------------------------------


# V3: Data prep -----------------------------------------------------------


setwd('C:/Users/saral/Google ????/0-Year 2/Thesis/Internship/Experiment/data_collected/retrieval_data_V3')

v3_files <- list.files(pattern="*.csv")


df_v3 <- lapply(v3_files, 
                  read.csv,
                  header = T,
                  sep = ';',
                  stringsAsFactors = FALSE)

df_v3 <- do.call(rbind, df_v3)

names(df_v3)[1] <- "mask"

df_v3 <- df_v3 %>% 
  mutate(type_m = case_when(
    endsWith(sector, "LM") ~ "low_m",
    endsWith(sector, "MM") ~ "medium_m",
    endsWith(sector, "HM") ~ "high_m"
  ))

df_v3 <- df_v3 %>% 
  mutate(type_m = factor(type_m, 
                         levels = c('low_m', 'medium_m', 'high_m'))) %>% 
  arrange(type_m)


# V3: Performance ---------------------------------------------------------

#by type
v3_acc <- df_v3 %>%
  group_by(participant, type_m) %>%
  summarise(acc = round( (sum(resp_m.corr) / n() )*100, 2))

## arrange in ascending order by type
v3_acc <- v3_acc %>% 
  mutate(type_m = factor(type_m, 
                         levels = c('low_m', 'medium_m', 'high_m'))) %>% 
  arrange(type_m)

#by participant 
v3_acc_sid <- df_v3 %>%
  group_by(participant) %>%
  summarise(acc = round( (sum(resp_m.corr) / n() )*100, 2))


plot(acc ~ participant, data = v3_acc_sid, main="Accuracy by participant",
                xlab= "Paricipants", ylab="Performance rate", pch=19)


# Analysis of Performance -------------------------------------------------

# compare performance between conditions between 2 samples 


hist(v1_acc_mean_sid$V1, breaks=5)

shapiro.test(v1_acc_mean_sid$V1)

sd(v1_acc_mean_sid$V1)
var(v1_acc_mean_sid$V1)

hist(v2_acc_sid$acc)
shapiro.test(v2_acc_sid$acc)
sd(v2_acc_sid$acc)
var(v2_acc_sid$acc)


t.test(v1_acc_mean_sid$V1, v2_acc_sid$acc, alternative = "two.sided", var.equal = FALSE)


# LR model ---------------------------------------------------------------

#Mixed effects
V3.acc.null <- lmer( resp_m.corr ~  (1|participant), df_v3)
V3.acc.full <- lmer( resp_m.corr ~  (1+type_m|participant), df_v3)
anova(V3.acc.null, V3.acc.full)
summary(V3.acc.full)





# Simple V1
m1.lm <- lmer(accu ~ type_m + (1 | sid), data = v1_acc)
summary(m1.lm)

autoplot(m1.lm) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

df1 <- data.frame(id = 1:length(resid(m1.lm)),
                  residuals = resid(m1.lm),
                  standard = rstandard(m1.lm),
                  studend = rstudent(m1.lm))
# generate plots
p1 <- ggplot(df1, aes(x = id, y = residuals)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point() +
  labs(y = "Residuals", x = "Index")
p2 <- ggplot(df1, aes(x = id, y = standard)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point() +
  labs(y = "Standardized Residuals", x = "Index")
p3 <- ggplot(df1, aes(x = id, y = studend)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point() +
  labs(y = "Studentized Residuals", x = "Index")
# display plots
ggpubr::ggarrange(p1, p2, p3, ncol = 3, nrow = 1)




# Simple V2

m2.lm <- lm(acc ~ type_m, data = v2_acc)
summary(m2.lm)

autoplot(m2.lm) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

df2 <- data.frame(id = 1:length(resid(m2.lm)),
                  residuals = resid(m2.lm),
                  standard = rstandard(m2.lm),
                  studend = rstudent(m2.lm))
# generate plots
p1 <- ggplot(df2, aes(x = id, y = residuals)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point() +
  labs(y = "Residuals", x = "Index")
p2 <- ggplot(df2, aes(x = id, y = standard)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point() +
  labs(y = "Standardized Residuals", x = "Index")
p3 <- ggplot(df2, aes(x = id, y = studend)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point() +
  labs(y = "Studentized Residuals", x = "Index")
# display plots
ggpubr::ggarrange(p1, p2, p3, ncol = 3, nrow = 1)




# Simple V3

m3.lm <- lm(acc ~ type_m, data = v3_acc)
summary(m3.lm)

autoplot(m3.lm) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


df3 <- data.frame(id = 1:length(resid(m3.lm)),
                  residuals = resid(m3.lm),
                  standard = rstandard(m3.lm),
                  studend = rstudent(m3.lm))
# generate plots
p1 <- ggplot(df3, aes(x = id, y = residuals)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point() +
  labs(y = "Residuals", x = "Index")
p2 <- ggplot(df3, aes(x = id, y = standard)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point() +
  labs(y = "Standardized Residuals", x = "Index")
p3 <- ggplot(df3, aes(x = id, y = studend)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point() +
  labs(y = "Studentized Residuals", x = "Index")
# display plots
ggpubr::ggarrange(p1, p2, p3, ncol = 3, nrow = 1)




# Analysis of sounds -------------------------------------------------------

# Data prep ---------------------------------------------------------------

setwd('C:/Users/saral/Google ????/0-Year 2/Thesis/Internship/Experiment/data_collected/alarm_data')
files_a <- list.files(pattern="*.csv")
df_a <- lapply(files_a, 
               read.table,
               header = FALSE,
               skip =3,
               sep = ';',
               stringsAsFactors = FALSE)

df_a <- do.call(rbind, df_a)

df_a = select(df_a, V1, V2, V37, V41)

df_a <- df_a %>% 
  rename(
    sid = V1,
    alarm_loc = V2,
    ar_resp = V37, 
    val_resp = V41
  )

df_a$val_resp <- round(df_a$val_resp)


df_a <- df_a %>% 
  mutate(type = case_when(
    grepl('BC',alarm_loc) ~ 'low', 
    grepl('CD',alarm_loc) ~ 'medium',
    grepl('CG',alarm_loc) ~ 'high'
  ))


df_a <- df_a %>% 
  mutate(interval = case_when(
    grepl('833',alarm_loc) ~ 'long', 
    grepl('500',alarm_loc) ~ 'medium',
    grepl('167',alarm_loc) ~ 'short'
  ))


df_a  = select(df_a, -2)

df_a <- df_a[, c(1, 4, 5, 2, 3)]

df_a <- df_a %>% 
  mutate(across(c('ar_resp', 'val_resp'), 
            ~as.numeric(.), na.rm=T)
            )
            

mean_ar <- df_a %>%
  as_tibble(df_a) %>%
  group_by(type, interval) %>%
  summarise(across(ar_resp,
                   list(mean = ~mean(.x, na.rm=T), 
                        sd = ~sd(.x, na.rm=T)),
            .names = "{.fn}"),
            )

## By sid and type

mean_ar_sid <- df_a %>%
  as_tibble(df_a) %>%
  group_by(sid, type) %>%
  summarise(across(ar_resp,
                   list(mean = ~mean(.x, na.rm=T), 
                        sd = ~sd(.x, na.rm=T)),
                   .names = "{.fn}"),
            )

mean_ar <- mean_ar %>%
  arrange(match(type, c("low", "medium", "high")),
          match(interval, c('long', 'medium', 'short'))
          )

mean_val <- df_a %>%
  group_by(type, interval) %>%
  summarise(across(val_resp,
                   list(mean = ~mean(., na.rm=T), 
                        sd = ~sd(., na.rm=T)),
                   .names = "{.fn}"),
            )

mean_val <- mean_val %>%
  arrange(match(type, c("low", "medium", "high")),
          match(interval, c('long', 'medium', 'short'))
          )

## By sid and type

mean_val_sid <- df_a %>%
  as_tibble(df_a) %>%
  group_by(sid, type) %>%
  summarise(across(val_resp,
                   list(mean = ~mean(.x, na.rm=T), 
                        sd = ~sd(.x, na.rm=T)),
                   .names = "{.fn}"),
            )


# Plots -------------------------------------------------------------------

## Arousal
mean_ar <- mean_ar %>% 
  mutate(type = factor(type, levels = c('low', 'medium', 'high'))
         )

mean_ar <- mean_ar %>% 
  mutate(interval = factor(interval, levels = c('long', 'medium', 'short'))
        )

ggplot(mean_ar, aes(type, mean)) +
  geom_errorbar(
    aes(ymin = mean-sd, ymax = mean+sd, color = interval),
    position = position_dodge(0.3), width = 0.2
    )+
  geom_point(aes(color = interval), position = position_dodge(0.3)) +
  scale_color_manual(values = c("#ebb434", "#eb8934", "#eb4034")) +
  ggtitle("Mean levels of arousal across types of alarms with different intervals") +
  labs(y="Arousal", x = "Type of alarm (urgency)")


## Valence
mean_val<- mean_val %>% 
  mutate(type = factor(type, levels = c('low', 'medium', 'high'))
        )

mean_val <- mean_val %>% 
  mutate(interval = factor(interval, levels = c('long', 'medium', 'short'))
        )

ggplot(mean_val, aes(type, mean)) +
  geom_errorbar(
    aes(ymin = mean-sd, ymax = mean+sd, color = interval),
    position = position_dodge(0.3), width = 0.2
  )+
  geom_point(aes(color = interval), position = position_dodge(0.3)) +
  scale_color_manual(values = c("#ebb434", "#eb8934", "#eb4034"))+
  ggtitle("Mean levels of valence across types of alarms with different intervals")+
  labs(y="Valence", x = "Type of alarm (urgency)")




# ANOVA Alarms ------------------------------------------------------------

## Arousal
res.ar <- aov(ar_resp ~ type * interval, data = df_a)
summary(res.ar)

TukeyHSD(res.ar, which = "type", conf.level = 0.95, p.adjust.methods = 'bonferroni')

tq_ar <- TukeyHSD(res.ar, conf.level = 0.95, p.adjust.methods = 'bonferroni')

tq_ar$`type:interval`[5, 'p adj']

arousal.lm <- polr(ar_resp ~ type * interval, data = df_a, Hess = T)
summary(alarms.lm)


##Valence
res.val <- aov(val_resp ~ type * interval, data = df_a)
summary(res.val)

TukeyHSD(res.val, which = "interval", conf.level = 0.95, p.adjust.methods = 'bonferroni')

TukeyHSD(res.val, conf.level = 0.95, p.adjust.methods = 'bonferroni')


# Assumptions -------------------------------------------------------------


# Normality ---------------------------------------------------------------

## Arousal
plot(res.ar, 2)
aov_residuals_ar <- residuals(object = res.ar)
shapiro.test(x = aov_residuals_ar) #norm


## Valence 
plot(res.val, 2)
aov_residuals_val <- residuals(object = res.val)
shapiro.test(x = aov_residuals_val) #norm


# Homogeneity -------------------------------------------------------------

## Arousal
plot(res.ar, 1) #plot outliers

leveneTest(ar_resp ~ type*interval, data = df_a) #data are homogeneous

## Valence

plot(res.val, 1) #plot outliers

leveneTest(val_resp ~ type*interval, data = df_a) #data are homogeneous





# Chuncks -----------------------------------------------------------------

autoplot(m1.lm) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

df1 <- data.frame(id = 1:length(resid(m1.lm)),
                  residuals = resid(m1.lm),
                  standard = rstandard(m1.lm),
                  studend = rstudent(m1.lm))
# generate plots
p1 <- ggplot(df1, aes(x = id, y = residuals)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point() +
  labs(y = "Residuals", x = "Index")
p2 <- ggplot(df1, aes(x = id, y = standard)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point() +
  labs(y = "Standardized Residuals", x = "Index")
p3 <- ggplot(df1, aes(x = id, y = studend)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point() +
  labs(y = "Studentized Residuals", x = "Index")
# display plots
ggpubr::ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
