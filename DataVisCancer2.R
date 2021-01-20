setwd("~/Dropbox/Data Science Semester 2/Data Visualisation/Tionscanamh")
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
A_SEXANDYEAR = as.data.frame(read.csv("All_sex_and_year.csv"))
incidence_rate_94vs15 = as.data.frame(read.xlsx)

typeof(A_SEXANDYEAR)
View(A_SEXANDYEAR)
#comments section Case numbers / % invasive / Crude rate are specific to the age selection.
#Age-standardised rate and Standard error are based on age range 0 to 85+ (0 to 19 for childhood cancer).
#Risk is based on age range 0 to 74 (0 to 19 for childhood cancer).
ASY = A_SEXANDYEAR[-c(67:69),]#remove comment notes
ASY= as.data.frame(ASY)
)
typeof(ASY)
ASY_SEXVEC = as.vector(ASY$Sex)

#Not working at the moment, same problemwith levels as other assignment, need to set them yourself
Silver Bullet
ASY$SEX = factor(ASY$Sex)
ASY$Sex = as.factor(ASY$Sex)
factor(ASY$Sex)
df = as.data.frame(ASY[c(1:3,5)])

ASY$Sex <- factor(AS,levels=c(3,4,5),

#shes alive
p = ggplot(df, aes(Year, Crude.rate))
p + geom_point()
p + geom_smooth(aes(colour = factor(df$Sex)))

a = ggplot(df,aes(factor(Sex)))
a + geom_bar()
#used to work like this
case_totals = df %>% 
  group_by(Sex) %>%
  summarize(total = sum(Case.numbers))

m_colo = MC_colorectal %>% 
  group_by(Region,Year) %>% 
  summarize(total = mean(Crude.rate))

ggplot(data = crude_totals, mapping = aes(x = Sex, y = total)) + geom_bar(aes(fill = factor(Sex)), stat="identity") # very important, if you leave this blank the bar heights are proportional not actual numbers. if you set to identity you also need to instruct y mapping.
#stopped working
ggplot(data = df, mapping = aes(x = Sex, y = total)) + geom_bar(aes(fill = factor(Sex)), stat="identity")

#select only male & female to exclude both

#sandbox, this works
crude_totals = df %>% 
  group_by(Sex) %>% 
  summarize(total = mean(Crude.rate))

ggplot(data = crude_totals, mapping = aes(x = Sex, y = total)) + geom_bar(aes(fill = factor(Sex)), stat="identity") # very important, if you leave this blank the bar heights are proportional not actual numbers. if you set to identity you also need to instruct y mapping.


#Code for getting the lifetime risk
Male_risk = c((as.numeric(sub("%", "e-2", ASY[1,8]))), (as.numeric(sub("%", "e-2",ASY[22,8]))))
Fem_risk =  c((as.numeric(sub("%", "e-2", ASY[23,8]))), (as.numeric(sub("%", "e-2",ASY[44,8]))))

#Crude rate % increase risk
male_crude_rate_inc = print(df[22,4]/df[1,4])

#Increased LT risk in time period
Men = (((Male_risk[2] - Male_risk[1]) /Male_risk[1])*100)
print(round(Men, digits = 2))

http://stat545.com/block023_dplyr-do.html

ACOUNTY = as.data.frame(read.csv("All_Region_BothSexes.csv"))
ACOUNTY = ACOUNTY[-c(771:773),-c(3:4,6:9)] # SHED THE SHITE
#Gather/group_by might be easier
counties = ACOUNTY %>% 
  group_by(Region) %>% 
  summarise(mean = mean(Crude.rate, na.rm = T))
counties
Look to see if county data is there, make a masterdata frame or spreadsheet

# Barchart w/crude rates of 5 most common cancers for both sexes

#Males

# Less work if you download the regional csv's and group by year to get the crude rate - agrees with overall crude rate in other csv
m_colo = MC_colorectal %>%
  + group_by(Year) %>%
  + summarize(total = mean(Crude.rate))




#code to investigate if any of the most common cancers had exceptionally high increases



big_hitter = master %>% 
  filter(State =='Ireland') %>% 
  filter(Year ==2015 |Year == 1994) %>% 
  group_by(Year) %>% 
  summarise(Total = mean(Crude.rate))

master_w_yearfac = master
master_w_yearfac$Year = factor(master_w_yearfac$Year)

#This looks shite despite all the work!
big_hitter = master %>% 
  filter(State =='Ireland') %>% 
  filter(Year ==2015 |Year == 1994) %>% 
  group_by(Year,.dots=c("M1_pros_crude", "M2_colo_crude","M3_lung_cancer_crude","M4_melan_crude","M5_nhl_crude","F1_breast_crude","F2_lung_crude","F3_colo_crude", "F4_melan_crude","F5_uteri_crude" )) %>% 
  summarise()

big_hitter
big_hitter$Year = NULL

big_hitter$group <- row.names(big_hitter)
dat.m <- melt(big_hitter, id.vars = "group")
ggplot(dat.m, aes(group, value)) + geom_boxplot(outlier.colour = 'red')

rel_increases = as.numeric(big_hitter[2,])/as.numeric(big_hitter[1,])
boxplot(rel_increases, outcol = 'red')

bp <- ggplot(big_hitter, aes(x=Year, y=)) + 
  geom_boxplot()



bp = as.data.frame(big_hitter)
bp$risk_pros = NULL
bp$State = NULL
bpb = data.frame
bpb$Year1994 = as.numeric(c(bp[1,c(2:10)]))
bpb$Year2015 = as.numeric(c(bp[2,c(2:10)]))



results %>% group_by(soil) %>%
  summarise(mean_yield = mean(yield), sd_yield = sd(yield))

is.factor(results$soil) 
levels(results$soil)

boxplot(big_hitter$~results$soil, outcol = 'red')


big_hitter
melan3$State = NULL
melan3$Year = factor(melan3$Year)
melan3$group <- row.names(melan3)
dat.m <- melt(melan3, id.vars = "Year")
ggplot(dat.m, aes(Year, value)) + geom_boxplot(outlier.colour = 'red')

DT.m1 = melt(melan3, id.vars = c("Year"),
             measure.vars = c("M4_melan_crude", "F4_melan_crude"))

levels(DT.m1$variable)
boxplot(DT.m1)
rel_increases = as.numeric(big_hitter[2,])/as.numeric(big_hitter[1,])
boxplot(rel_increases, outcol = 'red')

bp <- ggplot(DT.m1, aes(x=id~variable, y=value)) + 
  geom_boxplot()
#Looks like it works!
dfm = DT.m1
boxplot(dfm$value~dfm$Year*dfm$variable)
#MEANS FOR 1994 & 2015
group_by(melan3,Year) %>% 
  summarise(mean = mean(M4_melan_crude))

group_by(melan3,Year) %>% 
  summarise(mean = mean(F4_melan_crude))


viscosity = read.delim("viscosity.txt")
viscosity$pH = as.factor(viscosity$pH)
viscosity$Catalyst = as.factor(viscosity$Catalyst)
table(viscosity$Catalyst, viscosity$pH)
boxplot(viscosity$Viscosity~viscosity$Catalyst, ylab = 'Viscosity')
boxplot(Viscosity~Catalyst*pH, data = viscosity, col =(c("gold",'darkgreen')),main='Viscosity', xlab = "CAt and pH")

viscosity %>% group_by(Catalyst) %>% 
  summarise(mean_viscosity = mean(Viscosity), var_viscosity = var(Viscosity))
viscosity %>% group_by(pH) %>% 
  summarise (mean_viscosity = mean(Viscosity), var_viscosity = var(Viscosity))


boxplot(big_hitter)  
  arrange(Inc = big_hitter[2,]/big_hitter[1,])
big_hitter$risk_pros = NULL

rel_incs = list()
for (i in c(3:14)) {
  rel_incs[i] = as.numeric(big_hitter[2,i])/(as.numeric(big_hitter[1,i]))
}  


big_hitter %>% 
  group_by(rel_inc = 

round(100*((dfci$Rate.2015/dfci$Rate.1994)-1),2)
library(dplyr)
males = master %>%
  + group_by(master$State) %>% 
  summarize(males)

counties = master %>%
  filter(State =='Ireland') %>% 
  filter(Year == 2015)
    
counties    
  mean(counties$M1_pros_crude)
  
  
  filter(State == "Ireland")
  
  
  group_by(State,Year) %>% 
  summarise(total = mean(master$M1_pros_crude))
  figure2 = mdf %>% 
    filter(State =='Ireland') %>% 
    filter(Year == 2015  | Year == 1994) 
  
  
install.packages("plotrix")
library(plotrix)


xy.pop<-c(figure2[2,3:7])
xx.pop<-c(figure2[2,8:12])

xy.pop<-sort(as.numeric(c(figure2[2,3:7]), decreasing = TRUE))
xx.pop<-sort(as.numeric(c(figure2[2,8:12]),decreasing = TRUE))
agelabels<-c("Paaaaa","Caaa","Maaaa","Baaaa","baaa")
mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)
par(mar=pyramid.plot(xy.pop,xx.pop,unit="Cases p/100,000",top.labels = c("Male","","Female"),
                     main="Top 5 Cancers 2015",laxlab=c(25,50,75,100,125,150),raxlab=c(0,20,40,60,80,100,120,140),lxcol=mcol,rxcol='red',
                     gap=0.5,show.values=TRUE))



#county incidence tables
countyincidence_94to15 = as.data.frame(read.csv("INCREASEDATA.csv"))
dfci = countyincidence_94to15 
dfci$Percent_Inc = round(100*((dfci$Rate.2015/dfci$Rate.1994)-1),2)

dfci %>% group_by(County) %>%
  arrange(desc(Rate.1994))

#highest inc 2015
dfci %>% group_by(County) %>%
  arrange(desc(dfci$Rate.2015))

dfci %>% group_by(County) %>%
  arrange(desc(Rate.1994))
#by percent increase
dfci %>% group_by(County) %>%
  arrange((dfci$Percent_Inc))



dfci %>% group_by(County) %>%
  + arrange(asc(dfci$pc_inc))

dfci %>% group_by(County) %>%
  + arrange(desc(dfci$pc_inc))


dfci %>% group_by(County) %>% 
  summarise(pc_increase =(X2015/X1994) %>% 
              )
+ summarise(pc_inc = (dfci$X2015/dfci$1994))

#first graph sandbox

p = ggplot(dfb, aes(Year, Crude.rate))
p + geom_point()
p + geom_smooth(aes(colour = factor(dfb$Sex)))
p + geom_line(aes(colour = factor(dfb$Sex))) +scale_color_manual(values=c("red", "blue"))

labels <- data.frame(mpg = mtcars[which(mtcars$hp == max(mtcars$hp)), "mpg"]+7, hp = mtcars[which(mtcars$hp == max(mtcars$hp)), "hp"],text = paste0("Max value at mpg = ", mtcars[which(mtcars$hp == max(mtcars$hp)), "mpg"], " and hp = ", max(mtcars$hp)))


ggplot(mtcars, aes(mpg, hp))+
  geom_line()+
  geom_text(data = labels, aes(label = text))+
  annotate("segment", 
           x=mtcars[which(mtcars$hp == max(mtcars$hp)), "mpg"]+2,
           xend=mtcars[which(mtcars$hp == max(mtcars$hp)), "mpg"]+.2, 
           y= mtcars[which(mtcars$hp == max(mtcars$hp)), "hp"],
           yend= mtcars[which(mtcars$hp == max(mtcars$hp)), "hp"], 
           arrow=arrow(), color = "blue")
labels <- data.frame(mpg = mtcars[which(dfb$Crude.rate == max(dfb$Crude.rate)), "mpg"]+7, hp = mtcars[which(mtcars$hp == max(mtcars$hp)), "hp"],text = paste0("Max value at mpg = ", mtcars[which(mtcars$hp == max(mtcars$hp)), "mpg"], " and hp = ", max(mtcars$hp)))

ggplot(dfb, aes(Year, Crude.rate))+
  geom_line()+
  geom_text(data = labels, aes(label = text))+
  annotate("segment", 
           x=mtcars[which(mtcars$hp == max(mtcars$hp)), "mpg"]+2,
           xend=mtcars[which(mtcars$hp == max(mtcars$hp)), "mpg"]+.2, 
           y= mtcars[which(mtcars$hp == max(mtcars$hp)), "hp"],
           yend= mtcars[which(mtcars$hp == max(mtcars$hp)), "hp"], 
           arrow=arrow(), color = "blue")















