
######################
df.denom = read.csv("Data/3. RLS/pophr_public_le_table_denom.csv", header = FALSE)
names(df.denom) = c("rss", "age", "count", "year")
df.denom$year = substr(df.denom$year, 1, 4)

df.num = read.csv("Data/3. RLS/pophr_public_le_table_num.csv", header = FALSE) 
names(df.num) = c("age", "year", "rss", "count") # year --> death year

# check data --------------------------------------------------------------
str(df.denom) ; summary(df.denom); length(unique(df.denom$rss)) # no missing rss(5)
unique(df.denom$age) ; unique(df.denom$year) # no missing age category(0-111); yr:no missing year(1998 - 2014)
# 5*112*17=9520, df.denom has 8809 obs, --> some age/yr/rss combination didn't exist

summary(df.num) # 5/57 rss have zero count for certain age/year combination
df.num = df.num[df.num$count != 0, ]
length(unique(df.num$rss));unique(df.num$age);length(unique(df.num$year)) # 5 rss; 112 age category; 17 category

# generate all age/rss/yr combination
df.denom= as.data.frame(xtabs(df.denom$count  ~., df.denom))
df.num = as.data.frame(xtabs(df.num$count ~., df.num)) 
df.ori.denom = df.denom
df.ori.num = df.num

# change maximum age to 90 ----------------------------------------------------
df.denom = set_max_age(df.denom, 90)
df.num = set_max_age(df.num, 90)

# Group 10-consecutive years for 2011,2014, 9-consecutive years for 2006 ------------------------------------------------------------
df.denom = group_nconsecutive_yr(df.denom, 2006, 2011,2014, 9,10,10)
df.num = group_nconsecutive_yr(df.num,2006, 2011,2014, 9,10,10)

# Group age categories (0-4, 5-10, every 5-year age category, 90+)----------------------------------------------------
df.denom$age = as.numeric(as.character(df.denom$age)) # if not as character then age0 will disappear
df.denom$age_cat = cut(df.denom$age, c(-1,seq(10, 100, 10)), right= FALSE, labels = c(1:10))
df.denom = df.denom[, c("rss", "value", "age_cat", "Freq")]
df.denom = as.data.frame(xtabs(df.denom$Freq ~., df.denom))

df.num$age = as.numeric(as.character(df.num$age))
df.num$age_cat = cut(df.num$age, c(-1,seq(10, 100, 10)), right = FALSE, labels = c(1:10))
df.num = df.num[, c("rss", "value", "age_cat", "Freq")]
df.num = as.data.frame(xtabs(df.num$Freq ~., df.num))

# final table
df = join(df.num, df.denom, by = c("rss", "age_cat", "value"))
names(df) = c("rss", "year", "age_cat","deaths", "pop" )
df$rate = df$deaths/df$pop
df = df[, c("rss", "year",  "age_cat", "rate", "pop")]
df.sp = split(df, df$rss)
rm(df.denom); rm(df.num); rm(df)

df.ori = join(df.ori.num, df.ori.denom, by = c("rss", "age", "year"))
names(df.ori) = c("age", "year", "rss","deaths", "pop" )
df.ori.sp = split(df.ori, df.ori$rss)
rm(df.ori);rm(df.ori.denom)

# create life table component ------------------------------------------------------
# calculate the death probabilites
t= lapply(df.ori.sp, function(x) calculate_constantC(x))
rm(df.ori.num);rm(df.ori.sp)
df.sp = add_constantC(df.sp, t);rm(t)
df.sp = lapply(df.sp, function(x) add_death_prob(x, 10, 10, 10))

# calculate life table components based on fictitious cohort of 100,000 newborns
df.sp = lapply(df.sp, function(x) split(x, x$year))

df.sp = lapply(df.sp, function(x) {
    lapply(x, function(x){
        life_table_comp(x, 10, 10,10)
    })
})

LE = lapply(df.sp, function(x) {
    lapply(x, function(x){
        T= sum(x$num_live)
        le= T/100000
        return(le)
    })
})

# LE table for all rss/year combinations
df = as.data.frame(matrix(unlist(LE), nrow=length(unlist(LE[1])), dimnames = list(NULL, names(LE))), row.names = names(LE[[1]]))
rm(df.sp); rm(LE)
