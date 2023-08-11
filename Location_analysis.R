library(tidyverse)
library(lme4)
library(ltm)
library(mirt)
library(magrittr)
library(tibble)
library(ggplot2)
library(eRm)
library(xlsx)
library(data.table)

# read df's

df_v1 <- fread("df_v1.csv",
                  select = c('participant', 'x_coord', 'y_coord', 
                             'sector', 'resp_m.corr', 'resp_conf', 'type_m'))

df_v2 <- fread("df_v2.csv",
               select = c('participant', 'x_coord', 'y_coord', 
                          'sector', 'resp_m.corr', 'resp_conf.keys', 'type_m'))

names(df_v2)[6] <- "resp_conf"

# only object accuracy in V3
df_v3 <- fread("df_v3_obj.csv",
               select = c('participant', 'x_coord', 'y_coord', 
                          'sector', 'obj', 'resp_conf.keys', 'type_m'))

names(df_v3)[5] <- "resp_m.corr"
names(df_v3)[6] <- "resp_conf"


# create final df with all samples
loc_df <- rbind(df_v1, df_v2, df_v3)

# create loc column
loc_df %<>%
  unite("loc", x_coord:y_coord, sep= "; ", 
        remove = TRUE)

#loc_df <- read.csv(file = 'locations_total_df.csv', sep=';')


# Average performance by location
loc_mean_total <- loc_df %>%
  group_by(loc) %>%
  summarise(acc = round( (sum(resp_m.corr) / n() )*100, 2))

write.xlsx(loc_mean_total, 'loc_acc.xlsx', col.names = TRUE)


# rearrange df
loc_df %<>%
  group_by(participant) %>%
  arrange(loc)

# Select columns participant, loc, corr/incor, type
loc_df %<>%
  dplyr::select(participant, loc, resp_m.corr)


# Check types of variables 
loc_df$loc <- as.factor(loc_df$loc)

# Convert from long to wide format
wide_df <- loc_df %>%
  tidyr::spread(loc, resp_m.corr) %>%
  ungroup() %>%
  dplyr::select(-participant)

# Save as xlsx
write.xlsx(wide_df, 'wide_df.xlsx', col.names = TRUE)

# Plot percent of participants who answered correctly 

english_words %>% 
  select(-sex, -age) %>% 
  apply(2, mean) %>% 
  enframe() %>% 
  mutate(animal = fct_reorder(name, value)) %>% 
  ggplot(aes(x = animal, y = value)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = "Percent of kids that produce the word"
  )


v1_perc <- df_v1 %>% 
  select(resp_m.) %>% 
  apply(2, mean) %>% 
  enframe() %>% 
  mutate(animal = fct_reorder(name, value)) %>% 
  ggplot(aes(x = animal, y = value)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = "Percent of kids that produce the word"
  )
b


# IRT Rasch model ---------------------------------------------------------

irt_rasch <- RM(wide_df)
irt_rasch
summary(irt_rasch)
betas <- -coef(irt_rasch) # item difficulty parameters
round(sort(betas), 2)

# Plots

plotjointICC(irt_rasch, item.subset =  1:10, cex = .6)
mean(betas[1:22]) 
mean(betas[23:45]) 

# person-item map 
plotPImap(irt_rasch, cex.gen = .55)
plotPImap(irt_rasch, cex.gen = .55, sorted = TRUE)



# IRT 2-PL model --------------------------------------------------------------

irt_2pl <- ltm(wide_df ~ z1, IRT.param = TRUE) # 2pl
irt_2pl
summary(irt_2pl)

# Coefficients
coef_2pl <- coef(irt_2pl)
write.xlsx(coef_2pl, 'coef_2pl.xlsx', col.names = TRUE)

# Plots of IRTs

plot(irt_2pl, type='ICC') # all items at once
plot(irt_2pl, type='ICC', items = 16, cex = 0.6) # plots only item 3
plot(irt_2pl, type='ICC', items = c(1:5)) # plots items 1-3
plot(irt_2pl, type='IIC', items=0) # test information function

# Factor scores
options(max.print=100000) # max number of lines in output

factor.scores(irt_2pl) # this gives every single combination of response patterns 
# z1 - expected ability score 

person.fit(irt_2pl) # each person's data pattern and their corresponding ability estimate

item.fit(irt_2pl) # the probablity that items don't fit the model. We want it to be non sg. 
#with large samples items are always sg. it's a Chi-sq. 


# MIRT package 

irt_2pl_mirt <- mirt(
    data = wide_df,
    model = 1,
    itemtype = "2PL",
    verbose = FALSE
  )

plot(irt_2pl_mirt, type = "ICC")
plot(irt_2pl_mirt, type = "trace")
coef(irt_2pl_mirt)

coef(irt_2pl_mirt, simplify = TRUE)$items %>%
  as_tibble() %>% 
  mutate(item = row.names(coef(irt_2pl_mirt, simplify = TRUE)$items)) %>% 
  ggplot(aes(x = d, y = a1)) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(aes(label = item), size =1.5) +
  labs(
    x = "Easiness",
    y = "Discrimination"
  )

abilities <- wide_df %>% 
  mutate(theta_2pl = fscores(irt_2pl)[ , 1])

loc_df %>% 
  ggplot(aes(x = resp_m.corr, y = theta_2pl)) +
  geom_point(alpha = 0.1, size = 3)


# Comparison of Rasch and 2 PL
 ## lower values indicate better fit. Here Rasch is preferred 
rasch_m <- rasch(wide_df)
anova(rasch_m, irt_2pl)

cor(coef(rasch_m)[, 1], coef(irt_2pl)[, 1])

# Logistic regression -----------------------------------------------------


# Locations analysis: is there a relationship between certain locations and performance?
loc_df$loc <- as.factor(loc_df$loc)

loc.lm <- glm(resp_m.corr ~ loc, family = binomial(link='logit'),
                 data = loc_df)
summary(loc.lm)


# Mixed effects

m <- glmer(resp_m.corr ~ loc + (1 | participant), data=loc_df, 
      family = binomial, nAGQ=1)

print(m)

# 2PL 

irt_model <- ltm()

data(LSAT)


##### From another script


library(tidyverse)
library(lme4)
library(ltm)
library(mirt)
library(magrittr)
library(tibble)
library(ggplot2)

#df
loc_df <- read.csv(file = 'locations_total_df.csv', sep=';')

# rearrange df
loc_df %<>%
  group_by(participant) %>%
  arrange(loc)

# Select columns participant, loc, corr/incor, type
loc_df %<>%
  dplyr::select(participant, loc, resp_m.corr)


# Check types of variables 
loc_df$loc <- as.factor(loc_df$loc)



# Convert from long to wide format
wide_df <- loc_df %>%
  tidyr::spread(loc, resp_m.corr) %>%
  ungroup() %>%
  dplyr::select(-participant)


colnames_list <- dQuote(colnames(wide_df))
df <- data.frame(matrix(unlist(colnames_list), 
                        nrow=length(colnames_list), byrow=TRUE))


# IRT models --------------------------------------------------------------
irt <- ltm(wide_df ~ z1, IRT.param = TRUE) # 2pl
summary(irt)

# Coefficients
coef(irt)

# Plots of IRTs

plot(irt, type='ICC') # all items at once
plot(irt, type='ICC', items = 6) # plots only item 3
plot(irt, type='ICC', items = c(1:3)) # plots items 1-3
plot(irt, type='IIC', items=0) # test information function


# Factor scores
options(max.print=100000) # max number of lines in output

factor.scores(irt) # this gives every single combination of response patterns 
# z1 - expected ability score 

person.fit(irt) # each person's data pattern and their corresponding ability estimate

item.fit(irt) # the probablity that items don't fit the model. We want it to be non sg. 
#with large samples items are always sg. it's a Chi-sq. 


# MIRT package 

irt_2pl <- mirt(
  data = wide_df,
  model = 1,
  itemtype = '2PL',
  verbose = FALSE
)

plot(irt_2pl, type = "ICC")
plot(irt_2pl, type = "trace")
coef(irt_2pl)

coef(irt_2pl, simplify = TRUE)$items %>%
  as_tibble() %>% 
  mutate(item = row.names(coef(irt_2pl, simplify = TRUE)$items)) %>% 
  ggplot(aes(x = d, y = a1)) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(aes(label = item), size =1.5) +
  labs(
    x = "Easiness",
    y = "Discrimination"
  )

abilities <- wide_df %>% 
  mutate(theta_2pl = fscores(irt_2pl)[ , 1])

loc_df %>% 
  ggplot(aes(x = resp_m.corr, y = theta_2pl)) +
  geom_point(alpha = 0.1, size = 3)


# Logistic regression -----------------------------------------------------


# Locations analysis: is there a relationship between certain locations and performance?
loc_df$loc <- as.factor(loc_df$loc)

loc.lm <- glm(resp_m.corr ~ loc, family = binomial(link='logit'),
              data = loc_df)
summary(loc.lm)


# Mixed effects

m <- glmer(resp_m.corr ~ loc + (1 | participant), data=loc_df, 
           family = binomial, nAGQ=1)

print(m)

# 2PL 

irt_model <- ltm()

data(LSAT)



