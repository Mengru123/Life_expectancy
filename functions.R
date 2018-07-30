# set the maximum age to a user-defined value
set_max_age = function(df, max.age) {
    df$age = as.numeric(as.character(df$age)) # if not as character then age0 will disappear
    df$age = ifelse(df$age > max.age, max.age, df$age)
    df = as.data.frame(xtabs(df$Freq ~., df))
    return(df)
}

# group cases by year intervals specified by user, only 3 year of value are avaiable in this function, but can be added
group_nconsecutive_yr = function(df, yr1,yr2,yr3, n1, n2, n3 ) {
    names = colnames(df)
    df$year = as.integer(as.character(df$year))
    df$y1 = ifelse(df$year %in% ((yr1-n1 + 1): (yr1)), yr1, NA)
    df$y2 = ifelse(df$year %in% ((yr2-n2 + 1): (yr2)), yr2, NA)
    df$y3 = ifelse(df$year %in% ((yr3-n3 + 1): (yr3)), yr3, NA)
    df = melt(df, id = names)
    df = df[!is.na(df$value),]
    df = df[!names(df) %in% c("variable", "year")]
    df$value = as.factor(as.character(df$value))
    df = as.data.frame(xtabs(df$Freq ~., df))
    return(df)
}

calculate_constantC = function(df){
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

# add constanctC to the list 
add_constantC = function(df, clist) {
    for (i in 1: length(clist)) {
        C= clist[[i]]
        df[[i]]$consC = C
    }
    return(df)
}

# add death probability to the list
# age0 as a separate category was not considered
add_death_prob = function(df, max_age_cat, n_cat1, n_cat2tomax) {
    n1 = n_cat1
    n = n_cat2tomax
    df$age_cat = as.numeric(as.character(df$age_cat))
    df$d_prob = ifelse(df$age_cat == 1,df$rate/(1/n1 + df$rate*(0.5+(n1/12)*(df$rate-logb(df$consC)))), 
                       ifelse((df$age_cat>= 2 & df$age_cat < max_age_cat), df$rate/(1/n + df$rate*(0.5+(n/12)*(df$rate-logb(df$consC)))),1))
    return(df)
}

# calculate life table component using a fictionary cohort of 100,000 people
life_table_comp = function(df,max_age_cat,  n_cat1, n_cat2tomax){
    lo = 100000
    n1 = n_cat1
    n = n_cat2tomax
    for (i in 1:length(df$age_cat)) {
        df$num_d[i] = lo*df$d_prob[i]
        lo = lo-df$num_d[i]
        df$num_surv[i] = lo  }
    df$surv_prob = 1-df$d_prob
    df$num_live = ifelse(df$age_cat ==1, n1*(df$num_surv + df$num_d*0.5), 
                         ifelse((df$age_cat >=2 & df$age_cat < max_age_cat), n*(df$num_surv + df$num_d*0.5),df$num_surv*(1/df$rate)))
    return(df)
}   

create_LE = function(df1.rounte, df2.rounte,  max_age, yr1, yr2, yr3, n1, n2, n3, max_age_cat, n_cat1, n_cat2max) {
    df.denom = read.csv(as.character(df1.rounte), header = FALSE)
    names(df.denom) = c("hr", "age", "count", "year")
    df.denom$year = substr(df.denom$year, 1, 4)
    df.num = read.csv(as.character(df2.rounte), header = FALSE) 
    names(df.num) = c("age", "year", "hr", "count") # year --> death year
    df.num = df.num[df.num$count != 0, ]
    df.denom= as.data.frame(xtabs(df.denom$count  ~., df.denom))
    df.num = as.data.frame(xtabs(df.num$count ~., df.num)) 
    df.ori.denom = df.denom
    df.ori.num = df.num
    #set maximum age 
    df.denom = set_max_age(df.denom, max_age)
    df.num = set_max_age(df.num, max_age)
    # group data by year groups 
    df.denom = group_nconsecutive_yr(df.denom, yr1, yr2, yr3, n1,n2,n3)
    df.num = group_nconsecutive_yr(df.num,yr1, yr2, yr3, n1,n2,n3)
    
    # Group age categories (0-4, 5-10, every 5-year age category, 90+)----------------------------------------------------
    df.denom$age = as.numeric(as.character(df.denom$age)) # if not as character then age0 will disappear
    if (length(unique(df.denom$hr)) == 26) {
        df.denom$age_cat = cut(df.denom$age, c(-1,seq(10, 100, 10)), right= FALSE, labels = c(1:10))
    } else if(length(unique(df.denom$hr)) == 5) {
        df.denom$age_cat = cut(df.denom$age, c(-1,seq(5, 95, 5)), right= FALSE, labels = c(1:19))
    } else {
        df.denom$age_cat = cut(df.denom$age, c(-1, seq(20, 100, 10)), right= FALSE, labels = c(1:9))
    }
    df.denom = df.denom[, c("hr", "value", "age_cat", "Freq")]
    df.denom = as.data.frame(xtabs(df.denom$Freq ~., df.denom))
    
    df.num$age = as.numeric(as.character(df.num$age))
    if (length(unique(df.num$hr)) == 26) {
        df.num$age_cat = cut(df.num$age, c(-1,seq(10, 100, 10)), right= FALSE, labels = c(1:10))
    } else if(length(unique(df.num$hr)) == 5) {
        df.num$age_cat = cut(df.num$age, c(-1,seq(5, 95, 5)), right= FALSE, labels = c(1:19))
    } else {
        df.num$age_cat = cut(df.num$age, c(-1, seq(20, 100, 10)), right= FALSE, labels = c(1:9))
    }
    df.num = df.num[, c("hr", "value", "age_cat", "Freq")]
    df.num = as.data.frame(xtabs(df.num$Freq ~., df.num))
    
    
    df = join(df.num, df.denom, by = c("hr", "age_cat", "value"))
    names(df) = c("hr", "year", "age_cat","deaths", "pop" )
    df$rate = df$deaths/df$pop
    df = df[, c("hr", "year",  "age_cat", "rate", "pop")]
    df.sp = split(df, df$hr)
    rm(df.denom); rm(df.num); rm(df)
    
    df.ori = join(df.ori.num, df.ori.denom, by = c("hr", "age", "year"))
    names(df.ori) = c("age", "year", "hr","deaths", "pop" )
    df.ori.sp = split(df.ori, df.ori$hr)
    rm(df.ori);rm(df.ori.denom)
    
    # create life table component ------------------------------------------------------
    # calculate the death probabilites
    t= lapply(df.ori.sp, function(x) calculate_constantC(x))
    rm(df.ori.num);rm(df.ori.sp)
    df.sp = add_constantC(df.sp, t);rm(t)
    df.sp = lapply(df.sp, function(x) add_death_prob(x, max_age_cat, n_cat1, n_cat2max))
    
    # calculate life table components based on fictitious cohort of 100,000 newborns
    df.sp = lapply(df.sp, function(x) split(x, x$year))
    
    df.sp = lapply(df.sp, function(x) {
        lapply(x, function(x){
            life_table_comp(x, max_age_cat, n_cat1, n_cat2max)
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
    return(df)
    
}


