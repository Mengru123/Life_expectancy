library(dplyr)
library(plyr)
library(reshape2)
options(digits = 12)
source("functions.R")

# Calculate LE for rsss in PopHR data, 1998-2014 -------------------------

# input datasets
df.rls = create_LE(df1.rounte = "Data/3. RLS/pophr_public_le_table_denom.csv", 
                 df2.rounte = "Data/3. RLS/pophr_public_le_table_num.csv", 
                 max_age = 90, yr1 = 2006, yr2 = 2011, yr3 = 2014, n1 = 9, n2 = 10, n3 =10, 
                 max_age_cat = 10, n_cat1 = 10, n_cat2max = 10)


df.rss = create_LE(df1.rounte = "Data/2. RSS/pophr_public_le_table_denom.csv", 
                   df2.rounte = "Data/2. RSS/pophr_public_le_table_num.csv", 
                   max_age = 90, yr1 = 2006, yr2 = 2011, yr3 = 2014, n1 = 9, n2 = 10, n3 =10, 
                   max_age_cat = 19, n_cat1 = 5, n_cat2max = 5)


df.clsc = create_LE(df1.rounte = "Data/1. CLSC/pophr_public_le_table_denom.csv", 
                   df2.rounte = "Data/1. CLSC/pophr_public_le_table_num.csv", 
                   max_age = 90, yr1 = 2006, yr2 = 2011, yr3 = 2014, n1 = 9, n2 = 10, n3 =10, 
                   max_age_cat = 9, n_cat1 = 20, n_cat2max = 10)

write.csv(df.rss, file = "Output/LE_RSS.csv")
write.csv(df.clsc, file = "Output/LE_CLSA.csv")
write.csv(df.rls, file = "Output/LE_RLS.csv")

