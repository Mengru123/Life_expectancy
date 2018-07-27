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