library(dplyr)

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
  mutate(type_m = case_when(
    endsWith(sector, "LM") ~ "low_m",
    endsWith(sector, "MM") ~ "medium_m",
    endsWith(sector, "HM") ~ "high_m"
  ))

# arrange "type" in the order from low to high
df_v3 <- df_v3 %>%
  group_by(participant) %>%
  arrange(factor(type_m, levels = c('low_m', 'medium_m', 'high_m')), 
          .by_group = TRUE)



# write csv
#write.csv(df_v3, 'df_v3.csv', row.names = FALSE)