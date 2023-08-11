library(dplyr)
library(magrittr)
library(ggplot2)
library(car) # for levene's test
library(sjstats) # for size effect
library(rstatix)
library(extrafont)
library(ggpubr) #for density plots 
library(MASS)
library(brant) # for Brant test after ordinal regression
library(PupillometryR)
library(ordinal) # cumulative link model
library(VGAM) # proportional odds
library(gtsummary)

font_import() #Register fonts for Windows bitmap output
loadfonts(device="win") #vector of font family names

# Analysis of sounds -------------------------------------------------------

# Data prep ---------------------------------------------------------------

# read csv's
files_alarms <- list.files(pattern="*.csv")


df_a <- lapply(files_alarms, 
               read.table,
               header = T,
               sep = ';',
               stringsAsFactors = FALSE)

df_a <- do.call(rbind, df_a)

# rename the first column
names(df_a)[1] <- "alarm"

# create type column
df_a %<>% 
  mutate(type = case_when(
    grepl('BC', alarm) ~ 'low', 
    grepl('CD', alarm) ~ 'medium',
    grepl('CG', alarm) ~ 'high'
  ))

# create interval column. Files' names contain duration!
df_a %<>% 
  mutate(interval = case_when(
    grepl('833', alarm) ~ 'short', 
    grepl('500', alarm) ~ 'medium',
    grepl('167', alarm) ~ 'long'
  ))

# arrange type and interval
df_a %<>% 
  group_by(participant) %>%
  arrange(factor(type, levels = c('low', 'medium', 'high')),
          factor(interval, levels = c('short', 'medium', 'long')), 
          .by_group=T)


# # create trial column
# df_a %<>% 
#   group_by(participant) %>%
#   mutate(trial = c(1:18))


# remove NAs = 572 rows
df_a <- na.omit(df_a)

# arousal and valence as numeric
df_a %<>% 
  mutate(across(c('ar_resp', 'val_resp'), 
                ~as.numeric(.), na.rm=T)
  )

# Mean, median, SD ------------------------------------------------------------

#### Arousal ####

# mean and sd of arousal by type and interval
mean_sd_ar_type_int <- df_a %>%
  group_by(type, interval) %>%
  dplyr::summarise(across(ar_resp,
                   list(mean = ~mean(.x, na.rm=T), 
                        sd = ~sd(.x, na.rm=T)),
                    .names = "{.fn}")) %>%
  arrange(factor(type, levels=c('low', 'medium', 'high')), 
          factor(interval, levels=c('long', 'medium', 'short')))


# mean and sd of arousal by participant, type and interval
mean_sd_ar_sid_type_int <- df_a %>%
  as_tibble(df_a) %>%
  group_by(participant, type, interval) %>%
  dplyr::summarise(across(ar_resp,
                   list(mean = ~mean(.x, na.rm=T), 
                        sd = ~sd(.x, na.rm=T)),
                   .names = "{.fn}")) %>%
  arrange(factor(type, levels=c('low', 'medium', 'high')), 
          factor(interval, levels=c('short', 'medium', 'long')))

#### Valence ####

# mean and sd of valence by type and interval
mean_sd_val_type_int <- df_a %>%
  group_by(type, interval) %>%
  dplyr::summarise(across(val_resp,
                   list(mean = ~mean(., na.rm=T), 
                        sd = ~sd(., na.rm=T)),
                   .names = "{.fn}")) %>%
  arrange(factor(type, levels=c('low', 'medium', 'high')), 
          factor(interval, levels=c('long', 'medium', 'short')))

# mean and sd of valence by participant, type and interval
mean_sd_val_sid_type_int <- df_a %>%
  as_tibble(df_a) %>%
  group_by(participant, type, interval) %>%
  dplyr::summarise(across(val_resp,
                          list(mean = ~mean(.x, na.rm=T), 
                               sd = ~sd(.x, na.rm=T)),
                          .names = "{.fn}")) %>%
  arrange(factor(type, levels=c('low', 'medium', 'high')), 
          factor(interval, levels=c('long', 'medium', 'short')), 
          .by_group = T)`

# Plots -------------------------------------------------------------------

#### Arousal ####
mean_sd_ar_type_int %<>% 
  mutate(type = factor(type, levels = c('low', 'medium', 'high'))) %>%
  mutate(interval = factor(interval, levels = c('long', 'medium', 'short'))
  )

# Errorbar plot
ar_er <- mean_sd_ar_type_int %>%
  ggplot(aes(x=type, y=mean)) +
    geom_errorbar(width=0.2, size=1.4,
                  aes(ymin = mean-sd, ymax = mean+sd, color = interval),
                  position = position_dodge(0.6)) +
    geom_point(aes(color = interval), size=3, position = position_dodge(0.6)) +
    scale_color_manual(values = c("#ebd934", "#eb8c34", "#eb4034"), 
                       name = 'Time interval') +
    ggtitle("Mean levels of arousal across types of alarms with different intervals") +
    labs(y="Arousal", x = "Harmonic interval (urgency)") +
    theme_light() +
    theme(
      text = element_text(size=12,  family='serif'),
      axis.title = element_text(size = 13, face="bold"),
      plot.title = element_text(size=14, face="bold", family = 'serif'))
    


mean_sd_ar_sid_type_int %<>% 
  mutate(type = factor(type, levels = c('low', 'medium', 'high'))) %>%
  mutate(interval = factor(interval, levels = c('short', 'medium', 'long'))
  )


mean_sd_ar_sid_type_int %<>% 
  mutate(type_nr=case_when(
    grepl('low', type) ~ '1', 
    grepl('medium', type) ~ '2',
    grepl('high', type) ~ '3'
  ))

mean_sd_ar_type_int %<>% 
  mutate(type_nr=case_when(
    grepl('low', type) ~ '1', 
    grepl('medium', type) ~ '2',
    grepl('high', type) ~ '3'
  ))


# Raincloud plot
ggplot(mean_sd_ar_sid_type_int, aes(x = type, y = mean, fill = interval)) +
  geom_flat_violin(aes(fill = interval),
                   position = position_nudge(x = .23, y = 0), adjust = 1.5, 
                   trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(type_nr)-.15, y = mean, colour = interval),
             position = position_jitter(width = .05), size = .25, shape = 20)+
  geom_boxplot(aes(x = type, y = mean, fill = interval),
               outlier.shape = NA, alpha = .5, 
               position=position_dodge(width=.4), width = .3, colour = "black")+
  geom_line(data = mean_sd_ar_type_int, aes(x = as.numeric(type_nr)+.1, y = mean, group = interval, 
                                   colour = interval), linetype = 3)+
  geom_point(data = mean_sd_ar_type_int, aes(x = as.numeric(type_nr)+.1, y = mean, group = interval, 
                                    colour = interval), shape = 18) +
  geom_errorbar(data = mean_sd_ar_type_int,
                aes(x = as.numeric(type_nr)+.1, y = mean, group = interval, colour = interval, 
                    ymin = mean-sd, ymax = mean+sd), width = .05)+
  scale_colour_brewer(palette = "RdYlBu")+
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Mean levels of arousal across types of alarms with different intervals") +
  labs(y="Arousal", x = "Harmonic interval (urgency)") +
  theme_light() +
  theme(
    text = element_text(size=12,  family='serif'),
    axis.title = element_text(size = 13, face="bold"),
    plot.title = element_text(size=14, face="bold", family = 'serif'))



#### Valence ####
mean_sd_val_type_int %<>% 
  mutate(type = factor(type, levels = c('low', 'medium', 'high'))) %>%
  mutate(interval = factor(interval, levels = c('long', 'medium', 'short'))
  )

# Errorbar plot 
val_er <- mean_sd_val_type_int %>%
  ggplot( aes(x=type, y=mean)) +
    geom_errorbar(width=0.2, size=1.4,
     aes(ymin = mean-sd, ymax = mean+sd, color = interval),
      position = position_dodge(0.6))+
    geom_point(aes(color = interval), size=3, position = position_dodge(0.6)) +
    scale_color_manual(values = c("#ebd934", "#eb8c34", "#eb4034"), 
                       name = 'Time interval') +
    ggtitle("Mean levels of valence across types of alarms with different intervals")+
    labs(y="Valence", x = "Harmonic interval (urgency)") +
    theme_light() +
    theme(
    text = element_text(size=12,  family='serif'),
    axis.title = element_text(size = 13, face="bold"), 
    plot.title = element_text(size=14, face="bold", family = 'serif'))

# Multiplot: memory and confidence distr of results
Rmisc::multiplot(ar_er, val_er)



# count 
df_a %>%
  count(ar_resp, type, interval) %>%
  ggplot(aes(x = type, y = n, fill = interval)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  facet_grid(rows = vars(ar_resp), cols = vars(type), space = "free") +
  scale_x_discrete(labels = function (x) str_wrap(x, width = 10)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Taxes too high?",
       subtitle = "Reponse count by type and interval.")


# Assumptions -------------------------------------------------------------


#### Normality ####

# arousal
qqPlot(df_a$ar_resp)
shapiro.test(df_a$ar_resp) #H0 - normally distr
# p value < 0.05 => not normally distr

# plot(res.ar, 2)
# aov_residuals_ar <- residuals(object = res.ar)
# shapiro.test(x = aov_residuals_ar) #H0 - normally distr

# Density plot 
ggdensity(df_a$ar_resp, 
          main = "Density plot of arousal ratings",
          xlab = "Arousal ratings")

# valence
qqPlot(df_a$val_resp)
shapiro.test(df_a$val_resp)
# p value < 0.05 => not normally distr


# plot(res.val, 2)
# aov_residuals_val <- residuals(object = res.val)
# shapiro.test(x = aov_residuals_val)

# Density plot
ggdensity(df_a$val_resp, 
          main = "Density plot of arousal ratings",
          xlab = "Valence ratings")


#### Homogeneity ####

# Levene's  test - are variances equal between 2 samples? H0 - equal
res.ar <- aov(ar_resp ~ type * interval, data = df_a)

## Arousal
plot(res.ar, 1) #plot outliers

leveneTest(ar_resp ~ type*interval, data = df_a) #data are homogeneous

## Valence
plot(res.val, 1) #plot outliers

leveneTest(val_resp ~ type*interval, data = df_a) #data are not homogeneous



# Stats analysis ------------------------------------------------------------

#### ANOVA ####

### Arousal
res.ar <- aov(ar_resp ~ type * interval, data = df_a)
summary(res.ar)

TukeyHSD(res.ar, which = "type", conf.level = 0.95, p.adjust.methods = 'bonferroni')
TukeyHSD(res.ar, which = "interval", conf.level = 0.95, p.adjust.methods = 'bonferroni')

tq_ar <- TukeyHSD(res.ar, conf.level = 0.95, p.adjust.methods = 'bonferroni')

tq_ar$`type:interval`[5, 'p adj']

# Effect size
effectsize::eta_squared(res.ar)

library(lsr)
lsr::etaSquared(res.ar)

### Valence
res.val <- aov(val_resp ~ type * interval, data = df_a)
summary(res.val)

TukeyHSD(res.val, which = "type", conf.level = 0.95, p.adjust.methods = 'bonferroni')
TukeyHSD(res.val, which = "interval", conf.level = 0.95, p.adjust.methods = 'bonferroni')

#### Friedman test #### 
#(The Friedman test is used for one-way repeated measures analysis of variance by ranks)

res.fried <- df_a %>% friedman_test(ar_resp ~ type | participant)

#### Aligned Rank Transform ####

library(ARTool)

df_a <- na.omit(df_a)

df_a$type <- as.factor(df_a$type)
df_a$interval <- as.factor(df_a$interval)

art_m <- art(ar_resp ~ type*interval, data=df_a)
summary(art_m)

anova(art_m)


#### Permutation test ####
library(plyr)

#Compute some useful statistics per cell.
cell_stats = plyr::ddply(
  .data = df_a
  , .variables = .( participant, type , interval)
  , .fun = function(x){
    #Compute error rate as percent.
    #error_rate = mean(x$error)*100
    #Compute mean RT (only accurate trials).
    mean_rt = mean(x$ar_resp)
    #Compute SD RT (only accurate trials).
    sd_rt = sd(x$ar_resp)
    to_return = data.frame(
      #error_rate = error_rate
        mean_rt = mean_rt
      , sd_rt = sd_rt
    )
    return(to_return)
  }
)

#Compute the grand mean RT per Ss.
gmrt = ddply(
  .data = cell_stats
  , .variables = .( subnum , group )
  , .fun = function(x){
    to_return = data.frame(
      mrt = mean(x$mean_rt)
    )
    return(to_return)
  }
)

#Run a purely between-Ss permutation test on the mean_rt data.
mean_rt_perm = ezPerm(
  data = gmrt
  , dv = mrt
  , wid = subnum
  , between = group
  , perms = 1e1 #1e3 or higher is best for publication
)

#Show the Permutation test.
print(mean_rt_perm)

# Ordinal regression ------------------------------------------------------

df_a$ar_resp <- as.ordered(df_a$ar_resp) 
df_a$val_resp <- as.ordered(df_a$val_resp) 


df_a %<>% 
  mutate(type = factor(type, levels = c('low', 'medium', 'high'))) %>%
  mutate(interval = factor(interval, levels = c('long', 'medium', 'short'))
  )

## Model (fit the proportional odds logistic regression model)
fit_ar <- polr(as.factor(ar_resp) ~ type + interval, data = df_a, Hess = T, 
               method = c('logistic'))
summary(fit_ar)

# get the p-values and store the coefficient table
ctable <- round(coef(summary(fit_ar)), 4)
# calculate and store p-values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2
# combine coefficient table and p-values table
(ctable <- cbind(ctable, "p value" = round(p, 4)))

# calculate odds ratios
coefficients <- summary(fit_ar)$coefficients
odds_ratio <- exp(coefficients[ ,"Value"])

# combine with coefficient and p_value
(coefficients <- cbind(
  coefficients[ ,c("Value", "p_value")],
  odds_ratio
))


# get coefficients (it's in matrix form)
coefficients <- summary(fit_ar)$coefficients

# calculate p-values
p_value <- (1 - pnorm(abs(coefficients[ ,"t value"]), 0, 1))*2

# bind back to coefficients
(coefficients <- cbind(coefficients, p_value))


odds_ratio <- exp(coefficients[ ,"Value"])

(coefficients <- cbind(
  coefficients[ ,c("Value", "p_value")],
  odds_ratio
))


ci <- confint(fit_ar)
ci

## Assumptions:
# 1. DV are ordered +
# 2. IV are cont, ordered, categorical +
# 3. No multi-collinearity + 
# 4. Proportional odds


# Assumption 4: proportional odds
# testing parallel regression assumption using Brant's test
brant(fit_ar)

# Above is the Brant Test result for this dataset. 
# We conclude that the parallel assumption holds since the probability (p-values) 
# for all variables are greater than alpha=0.05. 
# The output also contains an Omnibus variable, 
# which stands for the whole model, and it is still greater than 0.05. 
# Therefore the proportional odds assumption is not violated 
# and the model is a valid model for this dataset.


# df_a <- df_a %>% 
#   mutate(across(c('type', 'interval'), 
#                 ~as.factor(.), na.rm=T)
#   )
# 
# df_a$ar_resp = factor (df_a$ar_resp, 
#                        levels = c("1", "2", "3", "4", "5", '6', '7', '8', '9'), 
#                        ordered = TRUE)
# 
# df_a$ar_resp <- as.ordered(df_a$ar_resp) 
# 
# df_a <- df_a %>% 
#   mutate(across(c('ar_resp', 'val_resp'), 
#                 ~as.numeric(.), na.rm=T)
#   )


arousal.lm <- polr(ar_resp ~ type, data = df_a, Hess = T)
summary(arousal.lm)


ctable <- coef(summary(arousal.lm))

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
cbind(ctable, "p value" = p)


test <- chisq.test(table(df_a$type, df_a$ar_resp))
summary(table(df_a$type, df_a$ar_resp))


table(df_a$type, df_a$ar_resp)


ggplot(df_a) +
  aes(x = type, fill = ar_resp) +
  geom_bar()



# Cumulative Link Model ---------------------------------------------------
arousal_av$mean <- factor(arousal_av$mean, ordered = TRUE)

clm_full <- clm(as.factor(mean) ~ type * interval, data = arousal_av)
summary(clm_full)

# Test of the main effect 
anova(clm_full, type="III")

# Compare full model to null model
clm_type <- clm(as.factor(mean) ~ type, data = arousal_av)
anova(clm_type, clm_full)

drop1(clm_full, test = "Chi")

# Likelihood ratio tests of the explanatory variable
# while ignoring the remaining variables
clm_0 <- clm(as.factor(mean) ~ 1, data=arousal_av)
add1(clm_0, scope = ~ type * interval, test = 'Chi')

# confidence intervals 
confint(clm_full)

# The CLM assumes that the thresholds are constant for all values of the 
# remaining IVs, so for type and interval. 
# This is called proportional odds assumptions (POA), or equal slopes assumption. 

# Test POA for all variables 
nominal_test(clm_full) # result: proportional odds are not violated

# Scale test
scale_test(clm_full)


#### Cumulative Link mixed model ####

##### Arousal ##### 

library(dplyr)
library(tidyr)
library(ordinal)
library(lme4)
library(ggplot2)
library(car)
library(MuMIn)

df_a$ar_resp <- factor(df_a$ar_resp, ordered = TRUE)
df_a$val_resp <- factor(df_a$val_resp, ordered = TRUE)

# remove NAs = 572 rows
df_a <- na.omit(df_a)


# Create group-level variables (ind - group mean)

df_a_2 <- df_a %>%
  group_by(participant, type) %>%
  mutate(ind_type_av = mean(ar_resp))


df_a$ar_resp <- as.numeric(df_a$ar_resp)
df_a_2 <- df_a %>%
  group_by(participant, type) %>%
  dplyr::summarise(across(ar_resp,
                          list(mean = ~mean(.x, na.rm=T), 
                               sd = ~sd(.x, na.rm=T)),
                          .names = "{.fn}")) %>%
  arrange(factor(type, levels=c('low', 'medium', 'high')), 
          .by_group=T)


# arrange type and interval
df_a %<>% 
  group_by(participant) %>%
  arrange(factor(type, levels = c('low', 'medium', 'high')),
          factor(interval, levels = c('short', 'medium', 'long')), 
          .by_group=T)


# Treat each trial as a unique entry (NB! each alarm presented twice)

# null model with random effects of trial and participant
clmm_null <- clmm(as.factor(ar_resp) ~ 1 + (1|participant), data = df_a)
summary(clmm_null)

# full model with fixed effects of type and interval
clmm_full <- clmm(as.factor(ar_resp) ~ type * interval + (1|participant), 
                  data = df_a)
summary(clmm_full)



# Take the average of 2 presentations of each stimulus
df_a %<>% 
  mutate(across(c('ar_resp', 'val_resp'), 
                ~as.numeric(.), na.rm=T)
  )



arousal_av <- df_a %>%
  group_by(participant, type, interval) %>%
  dplyr::summarise(across(ar_resp,
                          list(mean = ~mean(.x, na.rm=T), 
                               sd = ~sd(.x, na.rm=T)),
                          .names = "{.fn}")) %>%
  arrange(factor(type, levels=c('low', 'medium', 'high')), 
          factor(interval, levels=c('long', 'medium', 'short'))) 

arousal_av <- na.omit(arousal_av)


arousal_av$mean <- factor(arousal_av$mean, ordered = TRUE)


# df = arousal average
# null model with random effects of  and participant
clmm_null_ar_av <- clmm(as.factor(mean) ~ 1 + (1|participant), data = arousal_av)
summary(clmm_null_ar_av)

# full model with fixed effects of type and interval
clmm_full_ar_av <- clmm(as.factor(mean) ~ type * interval + (1|participant), 
                  data = arousal_av)
summary(clmm_null_ar_av)

anova(clmm_null_ar_av, clmm_null_ar_av)

# proportional odds assumption 

df_a$ar_resp <- factor(df_a$ar_resp, ordered = TRUE)

add_fmla <- formula(mean ~ type + interval)
sat_fmla <- formula(mean ~ type * interval)


vglm_ordinal <- vglm(add_fmla, propodds, data = arousal_av)
vglm_multinomial <- vglm(sat_fmla, cumulative, data = arousal_av)

# likelihood ratio test 
lrtest(vglm_multinomial, vglm_ordinal)

anova(clmm_null_ar_av, clmm_full_ar_av)


fmla1 <- formula(mean ~ type + interval)
clm1 <- clm(fmla1, data = arousal_av)
summary(clm1)


fmla2 <- formula(ar_resp ~ type * interval)
clm2 <- clm(fmla2, data = arousal_av)
anova(clm1, clm2)


tbl_regression(clm2)
tidy(clm2)

library(broom.mixed)
tidy <- clm2 %>% 
  broom.mixed::tidy(conf.int = TRUE, exponentiate = TRUE)


broom.mixed::tidy(clm2)

nagelkerke <- rcompanion::nagelkerke(clmm_full_ar_av)
nagelkerke
##### Valence ##### 

valence_av <- df_a %>%
  group_by(participant, type, interval) %>%
  dplyr::summarise(across(val_resp,
                          list(mean = ~mean(.x, na.rm=T), 
                               sd = ~sd(.x, na.rm=T)),
                          .names = "{.fn}")) %>%
  arrange(factor(type, levels=c('low', 'medium', 'high')), 
          factor(interval, levels=c('long', 'medium', 'short'))) 



# null model with random effects of  and participant
clmm_null_val_av <- clmm(as.factor(val_resp) ~ 1 + (1|participant), data = df_a)
summary(clmm_null_val_av)

# full model with fixed effects of type and interval
clmm_full_val_av <- clmm(as.factor(val_resp) ~ type + interval + (1|participant), 
                     data = df_a)
summary(clmm_full_val_av)

anova(clmm_null_val_av, clmm_full_val_av)



# Ordinal regression POLR  ------------------------------------------------------


