#### Prepare workspace and import data sets
library(tidyverse)
library(lubridate)

setwd("/Users/rodrigoalves/Desktop/PHD/COLABS/2025_pref")
#rm(list=ls())

# Feeding surveys
data <- read.csv('NZ-FeedingObservations.csv')
colnames(data)[6] <- 'Pred'

# Abundance data 
numbers <- read.csv('NZ-SpeciesDensities.csv')
colnames(numbers)[8] <- 'Prey' 

# Temperature data
temp <- read.csv('NZ-Temps-Monthly.csv')

## Calculate feeding rates

# Import regression coefficients (lab)
regs <- read.csv('NZ-HandlingTimes-MRegnCoeff-MeasuredANDMatched.csv',header=F,sep=',')

# Adjust df
regs <- regs[-c(1:3),]
colnames(regs) <- regs[1,]
regs <- regs[-1,]
regs$Pred <- as.factor(regs$Pred)

# Filter lab observations
coef <- regs %>% filter(ConLevel=='0.1' & Type =='Weighted')

# Join dfs
tab <- inner_join(data,coef,by=c('Pred','Prey'))

#tab <- anti_join(data,coef,by=c('Pred','Prey'))

#unmatch <- data.frame(unique(cbind(Prey=tab$Prey,Pred=tab$Pred)))

#unmatch <- filter(unmatch,Pred=='Haustrum haustorium'|Pred=='Haustrum scobina')

tab$Date <- month(mdy(tab$Date))

colnames(tab)[2] <- 'Month'

colnames(temp)[2] <- 'Month'

temp$Month <- month(ym(temp$Month))

df <- inner_join(tab,temp,by=c('Year','Month','Site'))

#unique <- unique(cbind(anti$Site,anti$Year,anti$Date))

#calculate handling times
df[,18:26] <- df[,18:26] %>% mutate_if(is.character,as.numeric)

df <- df %>% 
  mutate(htime = exp(logIntC 
  + logPredSizeC * log(PredSize) 
  + logPreySizeC * log(PreySize) 
  + logTempC * log(MeanTemp)
))

#convert to days
df$htime <- df$htime/24

#add non-feeding individuals
data$Date <- month(mdy(data$Date))

not_f <- data %>% filter(Prey=='Not Feeding') %>% select(Year,Date,Pred,Prey)

#df <- bind_rows(df,not_f)

#Calculate feeding rates (observational method)
f <- df %>% 
  group_by(Pred,Prey,Site) %>%
  summarise(m_h=mean(htime), #average handling times
            s=n()) %>% #number of surveyed predators
  mutate(p=s/sum(s),
         f=p/m_h) #feeding rates

## Add prey densities
env <- numbers %>%
  group_by(Prey,Site) %>%
  summarise(density = mean(Density))

df <- left_join(f,env)

df <- filter(df,Prey!='Not Feeding') #remove non-feeding observations

df$Pred <- as.factor(df$Pred)

#Calculate preferences
df <- df %>%
  na.omit() %>% 
  group_by(Pred,Site) %>% 
  mutate(phi=(f/density)/sum(f/density))

##rank distribution

df <- df %>% 
  group_by(Pred,Site) %>%
  arrange(Pred,Site, desc(phi)) %>% 
  mutate(rank = row_number())

#plot
df %>% 
  ggplot(aes(x=rank,y=phi))+
  geom_line(group=1)+
  geom_point()+
  theme_classic()+
  scale_y_log10()+
  #geom_text(aes(label=Prey),fontface='italic',size=3,angle=10)+
  facet_grid(~Pred~Site)
