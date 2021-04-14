#Libraries
library(readxl)
library(ggplot2)
library(ggthemes)
library(ggpubr)

#Read the Data
Deaths <- read_excel("~/Deaths_2010.xlsx")

#Because I asked for 2009-2011 deaths, I average them
Deaths$deaths<-Deaths$Deaths/3 

#Read in the Demonstration Product
Summary_File <- read_excel("~/nhgis_ppdd_20200527_county.xlsx")

#Calculate FIPS
#State FIPS
Summary_File$STATEFP<-as.numeric(substr(Summary_File$gisjoin,2,3))
Summary_File$COUNTYFP<-as.numeric(substr(Summary_File$gisjoin,5,7))

Summary_File$FIPS<-Summary_File$STATEFP*1000+Summary_File$COUNTYFP

Rural <- read_excel("~/ruralurbancodes2013.xls")
Rural$FIPS<-as.numeric(Rural$FIPS)
Rural<-subset(Rural,FIPS<71999)

Rural$metro<-ifelse(Rural$RUCC_2013<4,"Metro","Non-Metro")
Rural$metro2<-ifelse(Rural$RUCC_2013<4,1,0)
Rural$FIPS<-as.numeric(Rural$FIPS)

library(dplyr)
county_full <- left_join(Summary_File,Deaths, by = "FIPS")
county_full<-left_join(county_full,Rural,by="FIPS")

## Calculation of denominators and Mortality Rates
#Calculating denominators
county_full$pop_under1_sf<-county_full$H78003_sf+county_full$H78024_sf
county_full$pop_under1_dp<-county_full$H78003_dp+county_full$H78024_dp

#Tabulations
county_full$change_pop<-county_full$pop_under1_sf-county_full$pop_under1_dp
county_full$change_pop_pct<-(county_full$pop_under1_sf-county_full$pop_under1_dp)/county_full$pop_under1_sf

county_full$popcat<-ifelse(county_full$change_pop>0,"SF HIGHER",
                           ifelse(county_full$change_pop<0,"SF LOWER","SAME"))

#Here are the tables
table(county_full$popcat)
table(county_full$popcat,county_full$metro)

#Calculating Mort Rates
county_full$sf_mortrate<-county_full$deaths/county_full$pop_under1_sf
county_full$dp_mortrate<-county_full$deaths/county_full$pop_under1_dp

#Rate Ratio
county_full$rr<-(county_full$sf_mortrate)/county_full$dp_mortrate*100

county_full$rrbreaks<-ifelse(county_full$rr<97.50,"< 98.00",
                             ifelse(county_full$rr<=102.50,"98.00,102.50",
                                    ">102.50"))

county_full$rrbreaks2<-ifelse(county_full$rr<97.50,"Out",
                              ifelse(county_full$rr<=102.50,"Within",
                                     "Out"))
table(county_full$rrbreaks2)
table(county_full$rrbreaks2,county_full$metro)

## Descriptive Statistics
summary(county_full$change_pop)
summary(county_full$change_pop_pct)
summary(county_full$sf_mortrate)
is.na(county_full$dp_mortrate)<-sapply(county_full$dp_mortrate, is.infinite)
summary(county_full$dp_mortrate)
summary(county_full$rr)


## Visualization
# Panel A 

xmin<-min(0)
xmax<-max(0.045)
ymin<-min(0)
ymax<-max(0.045)

county_full3<-subset(county_full,is.na(county_full$rr)==FALSE)

is.na(county_full3$dp_mortrate)<-sapply(county_full3$dp_mortrate, is.infinite)

a<-ggplot(county_full3, aes(x=(sf_mortrate), y=(dp_mortrate),color=metro))+ geom_point(size=3)+
  geom_abline(intercept = 0, size=0.25, slope = 1,color="blue")+theme_classic()+
  labs(title="2010 Infant Mortality Rates, with different denominators",
       subtitle="Blue line is the line of equality",size=4)+
  ylab("IMR using Differential Privacy Denominator")+
  xlab("IMR using 2010 U.S. Census Denominator")+theme_classic()+
  ylim(ymin,ymax)+
  xlim(xmin,xmax)+theme(legend.title =element_blank(),legend.position = c(0.8,0.5),
                        legend.text=element_text(size=10))

a

#Tabulations
county_full$categories<-ifelse(county_full$sf_mortrate==county_full$dp_mortrate,"Same",
                               ifelse(county_full$sf_mortrate>county_full$dp_mortrate,"SFHigh", "SF LOWER"))

table(county_full$categories)


#Panel B
county_full2<-subset(county_full,is.na(county_full$rr)==FALSE)

b<-ggplot(county_full2, aes(x=log(pop_under1_sf), y=rr,color=metro)) + 
  geom_point(size=3)+
  labs(title="Infant Mortality Rate Ratio by population size",
       subtitle="± 2.5% ranges in black dashed lines",size=5)+
  ylab("Mortality Rate Ratio")+xlab("Logarithm of the population under 1 year")+
  theme_classic()+
  ylim(25,175)+
  xlim(4.5,13)+theme(legend.title =element_blank(),legend.position = "bottom",
                     legend.text=element_text(size=10))+
  geom_hline(yintercept=97.5, linetype="dashed", color = "black", size=0.5)+
  geom_hline(yintercept=102.5, linetype="dashed", color = "black", size=0.5)

b

#Create final Figure (Join Panels)
joined<-ggarrange(a, b, 
                  labels = c("A", "B"),
                  ncol = 2, nrow = 1,common.legend = TRUE,legend ="bottom")

joined