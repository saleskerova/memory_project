library(dplyr)
library(MASS)
library(ggplot2)
library(forcats)
library(effects)
library(data.table)
library(ggplot2)
library(extrafont)
library(ggdist)
library(Rmisc)


# Data prep ---------------------------------------------------------------

# read files
v3_files <- list.files(pattern="*.csv")

# create a list containing all files
df_v3 <- lapply(v3_files, 
                read.table,
                header = TRUE,
                sep = ';',
                stringsAsFactors = FALSE)

# create a dataset
df_v3 <- do.call(rbind, df_v3)

# rename the first column
names(df_v3)[1] <- "mask"

# create a column for memory type
df_v3 <- df_v3 %>% 
  mutate(type = case_when(
    endsWith(sector, "LM") ~ "low",
    endsWith(sector, "MM") ~ "medium",
    endsWith(sector, "HM") ~ "high"
  ))

# arrange "type" in the order from low to high
df_v3 <- df_v3 %>%
  group_by(participant) %>%
  arrange(factor(type, levels = c('low', 'medium', 'high')), 
          .by_group = TRUE)

# write csv
#write.csv(df_v3, 'df_v3.csv', row.names = FALSE)

# V3: Memory performance --------------------------------------------------

df_v3 <- read.csv('df_v3.csv')


# Overall mean accuracy 
mean(df_v3$resp_m.corr)
sd(df_v3$resp_m.corr)

df_v3 %>%
  group_by(participant) %>%
  arrange(factor(type, levels = c('low', 'medium', 'high')), 
          .by_group = TRUE)

# Calculate mean by type
v3_acc_type <- df_v3 %>%
  group_by(type) %>%
  dplyr::summarise(
    acc = round( mean(resp_m.corr)*100, 1), 
    sd = round( sd(resp_m.corr)*100, 1)) %>%
  arrange(factor(type, levels = c('low', 'medium', 'high')), 
          .by_group = TRUE)


# Calculate mean by type and participant
v3_acc_type_sid <- df_v3 %>%
  group_by(participant, type) %>%
  summarise(acc = round( (sum(resp_m.corr) / n() )*100, 2)) %>%
  arrange(factor(type, levels = c('low_m', 'medium_m', 'high_m')), 
          .by_group = TRUE)


v3_acc_type_sid <- df_v3 %>%
  group_by(participant, type) %>%
  summarise(acc = round( mean(resp_m.corr)*100, 1)) %>%
  arrange(factor(type, levels = c('low', 'medium', 'high')), 
          .by_group = TRUE)

# Calculate mean and sd by participant
v3_acc_sid <- df_v3 %>%
  group_by(participant) %>%
  dplyr::summarise(
    acc = round( mean(resp_m.corr)*100, 1),
    sd = round( sd(resp_m.corr)*100, 1))

# Outliers check: chance level is 12.5%
plot(acc ~ participant, data = v3_acc_sid, main="Accuracy by participant",
     xlab= "Paricipants", ylab="Performance rate", pch=19)


# Rename levels in the type column
violin_df <- v3_acc_type_sid %>%
  mutate(type_m=case_when(
    grepl('low', type) ~ 'low',
    grepl('medium', type) ~ 'medium',
    grepl('high', type) ~ 'high'
  ))

# Violin plot for performance by type 
ggplot(violin_df, aes(x= fct_inorder(type), y=acc)) +
  geom_violin()+
  geom_violin(scale = "count")+
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  geom_jitter(height = 0, width = 0.1)+
  labs(title="Performance rates by condition",
       x ="Difficulty condition", y = "Performance (%)")

# Raincloud plot
plot_m_total <- ggplot(v3_acc_type_sid, aes(x = fct_inorder(type), y = acc)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.8,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off") +
  labs(title="Performance rates by condition in Memory Pilot 3",
       x ="Difficulty condition", y = "Performance (%)") +
  theme_light() +
  theme(
    text = element_text(size=12,  family='serif'),
    axis.title = element_text(size = 13, face="bold"), 
    plot.title = element_text(size=14, face="bold", family = 'serif'))


v3_conf_type_sid <- df_v3 %>%
  group_by(participant, type) %>%
  summarise(conf = round( mean(resp_conf.keys), 1)) %>%
  arrange(factor(type, levels = c('low', 'medium', 'high')))

# mean confidence rating per type of memory, arrange in asc order by type
v3_conf_type <- df_v3 %>%
  group_by(type) %>%
  summarise(
    mean = round( mean(resp_conf.keys), 1),
    sd = round( sd(resp_conf.keys), 1)) %>%
  arrange(factor(type, levels = c('low', 'medium', 'high')))

df_v3$resp_conf <- as.numeric(df_v3$resp_conf)
df_v3$type <- as.factor(df_v3$type)

v3_conf_type_sid <- df_v3 %>%
  group_by(participant, type) %>%
  summarise(conf = round( mean(resp_conf.keys), 1)) %>%
  dplyr::arrange(factor(type, levels = c('low', 'medium', 'high')), 
                 .by_group=T)


plot_conf <- ggplot(v3_conf_type_sid, aes(x = fct_inorder(type), y = conf)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.8,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off") +
  labs(title="Confidence ratings by difficulty condition in Memory Pilot 3",
       x ="Difficulty condition", y = "Confidence (mean)") +
  theme_light() +
  theme(
    text = element_text(size=12,  family='serif'),
    axis.title = element_text(size = 13, face="bold"), 
    plot.title = element_text(size=14, face="bold", family = 'serif'))

# Multiplot: memory and confidence distr of results
Rmisc::multiplot(plot_m_total, plot_conf)


#### OBJECT RECOGNITION ANALYSIS ####

# Calculate object recognition performance 

df_v3 <- df_v3 %>% 
  mutate(obj = case_when(cor_ans - resp_m.keys == 1 ~ 1, 
                         cor_ans - resp_m.keys == -1 ~ 1, 
                         cor_ans == resp_m.keys ~ 1,
                        TRUE ~ 0))

mean(df_v3$obj)

write.csv(df_v3, 'df_v3_obj.csv', col.names = TRUE)

df_v3$type <- as.factor(df_v3$type)

# Performance for object recognition by participant 
df_v3$type <- as.factor(df_v3$type)

obj_acc_sid <- df_v3 %>%
  group_by(participant) %>%
  summarise(acc_obj = round( (sum(obj) / n() )*100, 2)) %>%
  dplyr::arrange(factor(type, levels = c('low', 'medium', 'high')), 
          .by_group = TRUE)

# Performance for object recognition by type
obj_acc_type <- df_v3 %>%
  group_by(type) %>%
  dplyr::summarise(
    obj_mean = round( mean(obj)*100, 1), 
    obj_sd = round( sd(obj)*100, 1))  %>%
  dplyr::arrange(factor(type, levels = c('low', 'medium', 'high')), 
          .by_group = TRUE)

mean(df_v3$obj)
sd(df_v3$obj)

# Performance for object recognition by participant and type
obj_acc_type_sid <- df_v3 %>%
  group_by(participant, type) %>%
  summarise(acc_obj = round( (sum(obj) / n() )*100, 1)) %>%
  arrange(factor(type, levels = c('low', 'medium', 'high')), 
          .by_group = TRUE)

# Plot difference between object and overall performance
obj_dff <- df_v3 %>%
  dplyr::select(type, resp_m.corr, obj)

library(reshape2)
dat.m <- melt(obj_dff, id.vars=c('participant', 'type'), measure.vars=c('resp_m.corr','obj'))
names(dat.m)[2] <- "type_q"

p <- ggplot(dat.m) +
  geom_boxplot(aes(x=type_q, y=mean(value), color=variable))
p

names(dat.m)[2] <- "Difficulty_condition"
names(dat.m)[3] <- "Question"


dat.mean <- dat.m %>%
  group_by(Difficulty_condition, Question) %>%
  dplyr::summarise(across(value,
                          list(mean = ~round( (sum(.x) / n() )*100, 1), 
                               sd = ~round( sd(.x)*100, 1)),
                          .names = "{.fn}")) %>%
  arrange(factor(Difficulty_condition, levels=c('low', 'medium', 'high')), 
          factor(Question, levels=c('resp_m.corr', 'obj')), 
          .by_group = T)

dat.mean %<>%
  arrange(factor(Difficulty_condition, levels=c('low', 'medium', 'high')))


mean_obj_total <- dat.mean %>%
  ggplot(aes(x=Difficulty_condition, y=mean)) +
  geom_errorbar(width=0.2, size=1.4,
                aes(ymin = mean-sd, ymax = mean+sd, color = Question),
                position = position_dodge(0.6)) +
  geom_point(aes(color = Question), size=3, position = position_dodge(0.6)) +
  scale_color_manual(values = c("#ebd934", "#eb8c34"), 
                     name = 'Type of question') +
  ggtitle("Difference in accuracy between total memory performance and object recognition") +
  labs(y="Performance, %", x = "Difficulty level") +
  theme_light() +
  theme(
    text = element_text(size=12,  family='serif'),
    axis.title = element_text(size = 13, face="bold"),
    plot.title = element_text(size=14, face="bold", family = 'serif'))


mean_obj_total


df_v3 %<>%
  mutate(cl_fr = resp_m.corr - obj)

df_v3 %<>%
  mutate(cl_fr = case_when(resp_m.corr == 1 ~ 1, 
                         resp_m.corr - obj == -1 ~ 0, 
                         TRUE ~ 0))
v3_cl_fr <- df_v3 %>%
  group_by(type) %>%
  dplyr::summarise(
    acc = round( mean(cl_fr)*100, 1),
    sd = round( sd(cl_fr)*100, 1)) %>%
  dplyr::arrange(factor(type, levels = c('low', 'medium', 'high')), 
                 .by_group = TRUE)
  



v3_cl_fr.lm <- glm(cl_fr ~ type, family = binomial(link='logit'),
                       data = df_v3)
summary(v3_cl_fr.lm)

library(forcats)

cl_fr_plt <- v3_cl_fr %>%
  ggplot(aes(x=fct_inorder(type), y=acc)) +
  geom_errorbar(width=0.2, size=1.4,
                aes(ymin = acc-sd, ymax = acc+sd),
                position = position_dodge(0.6)) +
  geom_point(aes(), size=3, position = position_dodge(0.6)) +
  ggtitle("Mean peformance for correct mask recognition") +
  labs(y="Performance, %", x = "Difficulty Condition") +
  theme_light() +
  theme(
    text = element_text(size=12,  family='serif'),
    axis.title = element_text(size = 13, face="bold"),
    plot.title = element_text(size=14, face="bold", family = 'serif'))

cl_fr_plt

# Plots -------------------------------------------------------------------

plot_m_total <- ggplot(v2_total_type_sid, aes(x = fct_inorder(type), y = acc)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.8,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off") +
  labs(title="Performance rates by condition: total accuracy",
       x ="Difficulty condition", y = "Performance (%)") +
  theme_light() +
  theme(
    text = element_text(size=12,  family='serif'),
    axis.title = element_text(size = 13, face="bold"), 
    plot.title = element_text(size=14, face="bold", family = 'serif'))





#obj_df <- melt(obj_dff ,  id.vars = 'type_m', value.name = 'obj')

# Overall performance - object recognition by type and participant


# Violin plot for performance by type 
ggplot(obj_acc_type_sid, aes(x= fct_inorder(type), y=acc_obj)) +
  geom_violin()+
  geom_violin(scale = "count")+
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  geom_jitter(height = 0, width = 0.1)+
  labs(title="Performance rates by condition",
       x ="Difficulty condition", y = "Performance (%)")


# V3: Memory analysis -----------------------------------------------------

# GLM: logistic regression

df_v3$type_m <- relevel(df_v3$type_m, ref = "low_m")

df_v3 <- df_v3 %>%
  arrange(factor(type_m, levels=c('low_m', 'medium_m', 'high_m')))

df_v3$type_m <- as.factor(df_v3$type_m)

df_v3 %>%
fct_relevel(type_m, 'low_m', 'medium_m', 'high_m')



v3_m.lm <- glm(resp_m.corr ~ type_m, family = binomial(link='logit'),
               data = df_v3)
summary(v3_m.lm)

anova(v3_m.lm, test="Chisq")
library(pscl)
pR2(v3_m.lm)



# fitting 
train <- df_v3[1:292,]
test <- df_v3[293:585,]


model <- glm(resp_m.corr ~ type_m, family=binomial(link='logit'), data=train)
summary(model)
anova(model, test="Chisq")
fitted.results <- predict(model,newdata=subset(test,select=c(10)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$resp_m.corr)
print(paste('Accuracy',1-misClasificError))

library(ROCR)
p <- predict(model, newdata=subset(test,select=c(10)), type="response")
pr <- prediction(p, test$resp_m.corr)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


library(caret)
glm_model <- caret::train(resp_m.corr~type_m,
                          data = df_v3, 
                          method='glm', 
                          family='binomial')
summary(glm_model)

OddsPlotty::odds_plot(glm_model$finalModel, 
                      title='Odds plot', 
                      subtitle='Odds of remembering the association right based on the type'
                        )


plot(Effect(focal.predictors = c("neuroticism","extraversion","sex"), mod = mod.cowles,
            xlevels=list(extraversion=seq(10, 20, 2), neuroticism=10:20)))





plot(allEffects(v3_m.lm), main = "Predicted probabilities for each condition")



# Object recognition regression model 
obj_m.lm <- glm(obj ~ type_m, family = binomial(link='logit'),
                data = obj_dff)
summary(obj_m.lm)


---------------------------
library(OddsPlotty)

plot_odds <- odds_plot(v3_m.lm, 
                       title='Odds plot', 
                       subtitle = 'Odds of remembering the association based on difficulty')
library(finalfit)

or_plot(df_v3$resp_m.corr, df_v3$type_m)
dependent <- df_v3$resp_m.corr
explanatory <- df_v3$type_m


df_v3 %>%
  or_plot(dependent, explanatory, table_text_size=4, title_text_size=14,
          plot_opts=list(xlab("OR, 95% CI"), theme(axis.title = element_text(size=12))))

exp(coefficients(v3_m.lm))

ctable_m <- coef(summary(v3_m.lm))

confint.default(v3_m.lm)
exp(confint.default(v3_m.lm))


ggplot(df_v3, aes(x=type_m, y=resp_m.corr)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))


df_v3 <- df_v3 %>%
  mutate(type_m=case_when(
  grepl('low', type_m) ~ 'low',
  grepl('medium', type_m) ~ 'medium',
  grepl('high', type_m) ~ 'high'
))

df_v3 <- df_v3 %>% 
  mutate(type = factor(type_m, levels = c('low', 'medium', 'high'))
  ) 

plot(allEffects(v3_m.lm),
     main="Performance in Version 3",
     ylab="Accuracy", 
     xlab='Difficulty', 
     col = rgb(0, 0, type_m))

# Plot of the distribution of performance rates across types of memory
ggplot(v3_acc_type_sid, aes(x = type_m, y = acc)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Mixed effects model -----------------------------------------------------
library(lme4)
# difference between object recognition and total
mem.full <- glmer(value ~  type+variable + (1|participant),
                      nAGQ=0, data=dat.m, family = binomial(link='logit'))
car::Anova(mem.full) 
summary(mem.full)

mem.fix.null <- glmer(value ~ (1|participant), dat.m, family = binomial(link='logit'))
mem.fix.null2 <- glmer(value ~ (1+type|participant), dat.m, family = binomial(link='logit'))
#mem.fix.null3 <- glmer(dur_fix ~ (1+Type|Subject)+(1+Type|Target_pic), dat,family = Gamma(link="identity"))
anova(mem.fix.null,mem.fix.null2)
#anova(mem.fix.null3,mem.fix.null2)

mem.fix.full <- glmer(value ~  type+variable + (1+type|participant),nAGQ=0, 
                      dat.m, family = binomial(link='logit'))
car::Anova(mem.fix.full) 
summary(mem.fix.full)

res.m <- aov(value ~ type + variable, data = dat.m)
summary(res.m)

TukeyHSD(res.m, which = "variable", conf.level = 0.95, p.adjust.methods = 'bonferroni')

t.test(obj ~ resp_m.corr, data = obj_dff)


# V3: Confidence rating means -------------------------------------------------------

#for each sound, a scale of 5. 
#Mean confidence = sum of confidence responses per type and divide by number of trials.


# mean confidence rating per each participant and type of memory
v3_conf_type_sid <- df_v3 %>%
  group_by(participant, type_m) %>%
  summarise(conf = round( (sum(resp_conf.keys) / n() ), 2))

# mean confidence rating per type of memory, arrange in asc order by type
v3_conf_type <- df_v3 %>%
  group_by(type_m) %>%
  summarise(conf = round( (sum(resp_conf.keys) / n() ), 2)) %>%
  arrange(factor(type_m, levels = c('low_m', 'medium_m', 'high_m')))


# V3: Analysis of confidence ------------------------------------------------


# re-write confidence rating as factor 
df_v3$resp_conf.keys <- as.factor(df_v3$resp_conf.keys)

# distribution of confidence ratings across types
ggplot(v3_conf_type_sid, aes(x = type_m, y = conf)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# ordinal regression
v3_conf<- polr(resp_conf.keys ~ type_m, data = df_v3, Hess = T)
summary(v3_conf)

# store the table of the coef summary
ctable <- coef(summary(v3_conf))

# calculate and store p-values
p <- pnorm(abs(ctable[, "t value"]), 
           lower.tail = FALSE) * 2

# combined table with p- and t-values
ctable <- cbind(ctable, "p value" = p)
print(ctable)

# confidence intervals
ci <- confint(v3_conf)
confint.default(v3_conf)


# convert the coefs to odds ratios (OR)
# exponentiate the estimates and CIs

exp(coef(v3_conf))

# OR and CIs. These are proportional odds ratios
exp(cbind(OR = coef(v3_conf), ci))





