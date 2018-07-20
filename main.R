library(dplyr)
library(plyr)
library(reshape2)
options(digits = 12)

# Calculate LE for CLSCs in PopHR data, 1998-2014 -------------------------

# input datasets
df.denom = read.csv("Data/pophr_public_le_table_denom.csv", header = FALSE, stringsAsFactors = FALSE)
names(df.denom) = c("clsc", "age", "count", "year")
df.num = read.csv("Data/pophr_public_le_table_num.csv", header = FALSE) 
names(df.num) = c("age", "year", "clsc", "count") # year --> death year

# check data --------------------------------------------------------------
str(df.denom) ; summary(df.denom)
length(unique(df.denom$clsc)) # no missing clsc(57)
unique(df.denom$age) ; unique(df.denom$year) # no missing age category(0-111); yr:no missing year(1998 - 2014)
# 57*112*17=108528, df.denom has 97307 obs, --> some age/yr/clsc combination didn't exist

summary(df.num) # 33/57 clsc have zero count for certain age/year combination
df.num = df.num[df.num$count != 0, ]
length(unique(df.num$clsc));unique(df.num$age);length(unique(df.num$year)) # 57 clsc; 112 age category; 17 category

# generate all age/clsc/yr combination
df.denom= as.data.frame(xtabs(df.denom$count  ~., df.denom))
df.num = as.data.frame(xtabs(df.num$count ~., df.num)) # 64678/108528 have Freq zero

# Group age categories (0-19, 19-20... 89-90)----------------------------------------------------
# can't treat 0-year-old as one category as no death for all clsc/year combination
df.denom$age = as.numeric(as.character(df.denom$age)) # if not as character then age0 will disappear
df.ori.denom = df.denom
df.denom$age_eld = ifelse(df.denom$age >90, 90, df.denom$age)
df.denom$age_cat = cut(df.denom$age_eld, c(-1, seq(20, 100, 10)), right= FALSE, labels = c(1:9))
df.denom = df.denom[, c("clsc", "year", "age_cat", "Freq")]
df.denom = as.data.frame(xtabs(df.denom$Freq ~., df.denom))

df.num$age = as.numeric(as.character(df.num$age))
df.ori.num = df.num
df.num$age_eld = ifelse(df.num$age >90, 90, df.num$age)
df.num$age_cat = cut(df.num$age_eld, c(-1, seq(20, 100, 10)), right = FALSE, labels = c(1:9))
df.num = df.num[, c("clsc", "year", "age_cat", "Freq")]
df.num = as.data.frame(xtabs(df.num$Freq ~., df.num))

# Group 10-consecutive years for 2011,2014, 9-consecutive years for 2006 ------------------------------------------------------------
group_year = function(df) {
    names = dput(colnames(df))
    df$year = as.integer(as.character(df$year))
    df$y1 = ifelse(df$year %in% (1998:2006), 2006, NA)
    df$y2 = ifelse(df$year %in% (2002:2011), 2011, NA)
    df$y3 = ifelse(df$year %in% (2005:2014), 2014, NA)
    df = melt(df, id = names)
    df = df[!is.na(df$value),]
    df = df[!names(df) %in% c("variable", "year")]
    df$value = as.factor(as.character(df$value))
    df = as.data.frame(xtabs(df$Freq ~., df))
    return(df)
}
df.denom$year = substr(df.denom$year, 1, 4)
df.denom = group_year(df.denom)
df.num = group_year(df.num)

df.ori.denom$year = substr(df.ori.denom$year, 1, 4)
df.ori.denom = group_year(df.ori.denom)
df.ori.num = group_year(df.ori.num)

# final table
df = join(df.num, df.denom, by = c("clsc", "age_cat", "value"))
names(df) = c("clsc", "age_cat", "year", "deaths", "pop" )
df$rate = df$deaths/df$pop
df = df[, c("clsc",  "age_cat","year", "rate", "pop")]
df.sp = split(df, df$clsc)
rm(df.denom); rm(df.num); rm(df)

df.ori = join(df.ori.num, df.ori.denom, by = c("clsc", "age", "value"))
names(df.ori) = c("age", "clsc", "year", "deaths", "pop" )
df.ori.sp = split(df.ori, df.ori$clsc)

# create life table component ------------------------------------------------------
# calculate the death probabilites
constantC = function(df){
    df$age = as.numeric(as.character(df$age))
    df1 = df %>%
        filter(age < 86 & age >4) 
    m5.85 = sum(df1$deaths)/sum(df1$pop)
    df2 = df %>%
        filter(age < 41 & age >4) 
    m5.40 = sum(df2$deaths)/sum(df2$pop)
    C= (1/45)*logb(m5.85/m5.40)
    return(C)
}
t= lapply(df.ori.sp, function(x) constantC(x))
rm(df.ori);rm(df.ori.denom);rm(df.ori.num);rm(df.ori.sp);

add_constantC = function(df, clist) {
    for (i in 1: length(clist)) {
        C= clist[[i]]
        df[[i]]$consC = C
    }
    return(df)
}
df.sp = add_constantC(df.sp, t);rm(t)

add_death_prob = function(df) {
    n = 10
    n1 = 20
    df$age_cat = as.numeric(as.character(df$age_cat))
    df$d_prob = ifelse(df$age_cat == 1,df$rate/(1/n1 + df$rate*(0.5+(n1/12)*(df$rate-logb(df$consC)))), 
           ifelse((df$age_cat>= 2 & df$age_cat <=8), df$rate/(1/n + df$rate*(0.5+(n/12)*(df$rate-logb(df$consC)))),1))
    return(df)
}
df.sp = lapply(df.sp, function(x) add_death_prob(x))

# calculate life table components based on fictitious cohort of 100,000 newborns
df.sp = lapply(df.sp, function(x) split(x, x$year))

life_table_comp = function(df){
    lo = 100000
    for (i in 1:length(df$age_cat)) {
        df$num_d[i] = lo*df$d_prob[i]
        lo = lo-df$num_d[i]
        df$num_surv[i] = lo  }
    df$surv_prob = 1-df$d_prob
    df$num_live = ifelse(df$age_cat ==1, 20*(df$num_surv + df$num_d*0.5), 
                         ifelse((df$age_cat >=2 & df$age_cat <=8), 10*(df$num_surv + df$num_d*0.5),df$num_surv*(1/df$rate)))
    return(df)
}   

df.sp = lapply(df.sp, function(x) {
    lapply(x, function(x){
        life_table_comp(x)
    })
})

LE = lapply(df.sp, function(x) {
    lapply(x, function(x){
        T= sum(x$num_live)
        le= T/100000
        return(le)
    })
})

# LE table for all CLSC/year combinations
df = as.data.frame(matrix(unlist(LE), nrow=length(unlist(LE[1])), dimnames = list(NULL, names(LE))), row.names = names(LE[[1]]))
rm(df.sp); rm(LE)
