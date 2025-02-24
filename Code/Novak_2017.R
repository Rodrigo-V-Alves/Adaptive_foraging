#### Prepare workspace and import data sets
library(tidyverse)
library(lubridate)

setwd("/Users/rodrigoalves/Desktop/PHD/COLABS/2025_pref")
#rm(list=ls())

# Feeding surveys

#unmanipulated
data <- read.csv('NatPatch-Modfit-FeedObs_raw.csv')

data$type <- rep(paste('Unmanipulated'),nrow(data))

#manipulated
data2 <- read.csv('NatPatchManip-Modfit-FeedObs_raw.csv')

colnames(data2)[3] <- 'Spp'

data2$type <- rep(paste('Manipulated'),nrow(data2))

#compact dfs
data <- data %>%
  group_by(Pred,Spp,type) %>%
  summarise(s=n()) 

data2 <- data2 %>%
  group_by(Pred,Spp,type) %>%
  summarise(s=n())

#add non-feeding predators
no <- filter(data,Spp =="Not_Feeding") %>% 
  spread(key=Spp,value=s,fill=0)

data <- left_join(data,no)

no2 <- filter(data2,Spp =="Not_Feeding") %>% 
  spread(key=Spp,value=s,fill=0)

data2 <- left_join(data2,no2)

#combine all
frate <- rbind(data,data2)
                   
## Handling times
#unmanipulated
th <- read.csv('NatPatch-Modfit-HandlingTimes.csv')

th$type <- rep(paste('Unmanipulated'),nrow(th))

#average handling times across pacthes
th$m_h <- apply(th[,-c(1,12)],1,mean)

#manipulated
th2 <- read.csv('NatPatchManip-Modfit-HandlingTimes.csv')

colnames(th2)[1] <- 'Spp'

th2$type <- rep(paste('Manipulated'),nrow(th2))

#average handling times across patches
th2$m_h <- apply(th2[,-c(1,20)],1,mean)

#combine
hand <- rbind(th[,-c(2:11)],th2[,-c(2:19)])

#join dfs
feed <- inner_join(frate,hand)

#Calculate feeding rates (observational method)
feed <- feed %>%
  mutate(p = s/(s+Not_Feeding),
         f = p/m_h)

## Add prey densities
#unmanipulated
numbers <- read.csv('NatPatch-Modfit-PreyMeans.csv')

numbers$type <- rep(paste('Unmanipulated'),nrow(numbers))

#manipulated
numbers2 <- read.csv('NatPatchManip-AllPreyMeans.csv')

numbers2$type <- rep(paste('Manipulated'),nrow(numbers2))

colnames(numbers2)[1] <- 'Spp'

#average across patches
numbers$m_n <- apply(numbers[,-c(1,12)],1,mean)

numbers2$m_n <- apply(numbers2[,-c(1,20)],1,mean)

#combine
n1 <- inner_join(frate,numbers)

n1 <- n1[,-c(6:15)]

n2 <- inner_join(frate,numbers2)

n2 <- n2[,-c(6:23)]

env <- rbind(n1,n2)

#final df
table <- full_join(feed,env)

#Calculate preferences
table <- table %>%
  #na.omit() %>% 
  filter(m_n > 0) %>% 
  group_by(Pred,type,Not_Feeding) %>% 
  mutate(phi=(f/m_n)/sum(f/m_n)) 

##rank distribution

table <- table %>%
  na.omit() %>% 
  group_by(Pred, type) %>%
  arrange(Pred, type, desc(phi)) %>% 
  mutate(rank = row_number())

#plot
table %>% 
  ggplot(aes(x=rank,y=phi))+
  geom_line(group=1)+
  geom_point()+
  theme_bw()+
  scale_y_log10()+
  #geom_text(aes(label=Prey),fontface='italic',size=3,angle=10)+
  facet_grid(~type~Pred)
