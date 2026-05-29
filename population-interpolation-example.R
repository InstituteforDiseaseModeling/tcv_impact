# Example population interpolation script using annual growth rates

# This script projects population growth forward from a single, baseline observation
# of population by age group and a single annual population growth rate

# Assumes that your data (df_pop) is a data frame with columns for
# - month (numeric, 1-12)
# - year (numeric)
# - age_group (character/factor, using levels described in data template)
# - population (numeric, population size)

# Users must define the maximum time for interpolation (years since baseline population measurement
# for which to infer population size). E.g., '1' would estimate population size for one
# year. Time step is monthly (by 1/12), assuming r is a yearly growth rate
maxtime<-1 #Set to your max time variable

# define the annual growth rate r for population 
r = 0.0274

# find the baseline population data
df_pop$time<-as.numeric(df_pop$year+(df_pop$month/12))
sorted_pop <- df_pop %>%
  arrange(time)
df_pop_t0<-ddply(sorted_pop, .(age_group), function(x) head(x,1))
df_pop_t0$prop_age<-df_pop_t0$population/sum(df_pop_t0$population)
df_pop_t0$age_group_factor<-factor(df_pop_t0$age_group, levels=c('0-2y', '2-5y', '5-15y', '15y+'))
sorted_pop_t0<- df_pop_t0 %>%
  arrange(age_group_factor)

# create time vector/data frame over which to project population size
tvec<-seq(from=0, to=maxtime-1/12, by=1/12)
df_pop_interpolated<-data.frame(time=rep(tvec, 4), #Repeat t 4x because we have 4 age groups
                                age_group=rep(df_pop_t0$age_group_factor, each=length(tvec)),
                                population=rep(NA, 4*length(tvec)))

#in the function, substitute basepop with your population at t0, r with the annual growth rate assuming you have age
#specific baseline populations (as calculated above)
population<-numeric(4*length(tvec))
annual_growth_rate<-function(basepop=sorted_pop_t0$population, r=0.0274, t=tvec, p_age=sorted_pop_t0$prop_age, age_groups=df_pop_t0$age_group_factor){
  for(i in 1:length(age_groups)){
    for(j in 1:length(tvec)){
      population[(i-1)*length(tvec)+j]<-basepop[i]*(1+r)^tvec[j]
    }
  }
  return(population)
}

# create population
df_pop_interpolated$population<-annual_growth_rate(r=r)