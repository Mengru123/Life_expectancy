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

# write.csv(df.rss, file = "Output/LE_RSS.csv")
# write.csv(df.clsc, file = "Output/LE_CLSA.csv")
# write.csv(df.rls, file = "Output/LE_RLS.csv")

df.rls.3yr = create_LE(df1.rounte = "Data/3. RLS/pophr_public_le_table_denom.csv", 
                            df2.rounte = "Data/3. RLS/pophr_public_le_table_num.csv", 
                            max_age = 90, yr1 = 2006, yr2 = 2011, yr3 = 2014, n1 = 3, n2 = 3, n3 =3, 
                            max_age_cat = 10, n_cat1 = 10, n_cat2max = 10)


df.rss.3yr = create_LE(df1.rounte = "Data/2. RSS/pophr_public_le_table_denom.csv", 
                           df2.rounte = "Data/2. RSS/pophr_public_le_table_num.csv", 
                           max_age = 90, yr1 = 2006, yr2 = 2011, yr3 = 2014, n1 = 3, n2 = 3, n3 =3,
                           max_age_cat = 19, n_cat1 = 5, n_cat2max = 5)


df.clsc.3yr = create_LE(df1.rounte = "Data/1. CLSC/pophr_public_le_table_denom.csv", 
                             df2.rounte = "Data/1. CLSC/pophr_public_le_table_num.csv", 
                             max_age = 90, yr1 = 2006, yr2 = 2011, yr3 = 2014, n1 = 3, n2 = 3, n3 =3, 
                             max_age_cat = 9, n_cat1 = 20, n_cat2max = 10)

df.rls.comp = create_LE_1yr(df1.rounte = "Data/3. RLS/pophr_public_le_table_denom.csv", 
                   df2.rounte = "Data/3. RLS/pophr_public_le_table_num.csv", 
                   max_age = 90, yr1 = 2006, yr2 = 2011, yr3 = 2014, n1 = 3, n2 = 3, n3 =3, 
                   max_age_cat = 91, n_cat1 = 1, n_cat2max = 1)


df.rss.comp = create_LE_1yr(df1.rounte = "Data/2. RSS/pophr_public_le_table_denom.csv", 
                   df2.rounte = "Data/2. RSS/pophr_public_le_table_num.csv", 
                   max_age = 90, yr1 = 2006, yr2 = 2011, yr3 = 2014, n1 = 3, n2 = 3, n3 =3,
                   max_age_cat = 91, n_cat1 = 1, n_cat2max = 1)


df.clsc.comp = create_LE_1yr(df1.rounte = "Data/1. CLSC/pophr_public_le_table_denom.csv", 
                    df2.rounte = "Data/1. CLSC/pophr_public_le_table_num.csv", 
                    max_age = 90, yr1 = 2006, yr2 = 2011, yr3 = 2014, n1 = 3, n2 = 3, n3 =3, 
                    max_age_cat = 91, n_cat1 = 1, n_cat2max = 1)


# t= rbind(df.clsc, df.clsc.3yr, df.clsc.comp)
# write.csv(t, "C:/Users/Mengru/Desktop/clsc_3mtd.csv")
# t= rbind(df.rls, df.rls.3yr, df.rls.comp)
# write.csv(t, "C:/Users/Mengru/Desktop/rls_3mtd.csv")
# t= rbind(df.rss, df.rss.3yr, df.rss.comp)
# write.csv(t, "C:/Users/Mengru/Desktop/rss_3mtd.csv")