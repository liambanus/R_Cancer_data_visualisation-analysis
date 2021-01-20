setwd("~/Dropbox/Data Science Semester 2/Data Visualisation/Tionscanamh")
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

#Import first spreadsheet used, combined incidence for all cancer categorised by sex
A_SEXANDYEAR = as.data.frame(read.csv("All_sex_and_year.csv"))

ASY = A_SEXANDYEAR[-c(67:69),]#remove comment notes
ASY= as.data.frame(ASY)
)
ASY$SEX = factor(ASY$Sex)
df = as.data.frame(ASY[c(1:3,5)]) #isolate the columns we want; sex, year and crude rate
df$Sex = factor(df$Sex)




#Category both was simply an average of male/female and was considered distracting
dfb = df %>% 
  group_by(Sex) %>% 
  filter(Sex != 'Both')
View(dfb)
levels(dfb$Sex)
dfb$Sex = droplevels(dfb$Sex, "Both")

dfb$Sex = factor(dfb$Sex, levels = c("Males","Females"))


#plot of increase during study period
p = ggplot(dfb, aes(Year, Crude.rate))
p + geom_point()
p + geom_smooth(aes(colour = factor(dfb$Sex)))
p + geom_smooth(aes(colour = Sex))


p + geom_line(aes(colour = Sex)) +ggtitle("Overall Diagnosis Rates") +scale_color_manual(name = '',labels = c("Males","Females"),values=c('Males' = "blue", 'Females'='red')) + labs(x = NULL, y = 'New Diagnoses p/100000', legend = NULL) +theme(plot.title = element_text(hjust = .5),legend.justification=c(1,0),legend.position=c(0.95, 0.05), legend.background = element_blank(),legend.key = element_blank())

p + geom_line(aes(colour = Sex)) +ggtitle("") +scale_color_manual(name = '',labels = c("Males","Females"),values=c('Males' = "blue", 'Females'='red')) + labs(x = NULL, y = 'New Diagnoses p/100000', legend = NULL) +theme(plot.title = element_text(hjust = .5),axis.text = element_text(size=12, face = "bold"),legend.justification=c(1,0),legend.position=c(0.95, 0.05), legend.background = element_blank(),legend.key = element_blank())

#Barchart
crude_totals = dfb %>% 
  group_by(Sex) %>% 
  df_94_15 = filter(Year == 1994 | Year ==2015 ) %>% 
  df_94_15 %>% group_by(Year) %>% 
  summarize(total = mean(Crude.rate))

crude_totals94 = dfb %>% 
  group_by(Sex) %>% 
  df_94_15 = filter(Year == 1994 | Year ==2015 ) %>% 
  df_94_15 %>% group_by(Year) %>% 
  summarize(total = mean(Crude.rate))

crude_totals94 =dfb %>% 
  group_by(Sex) %>% 
  filter(Year == 1994 | Year == 2015) %>% 
  group_by(Year)


  
ggplot(data = crude_totals94, mapping = aes(x = Sex, y = Crude.rate)) +ggtitle("Average Incidence: 2015")+ scale_fill_manual(values=c('Males' = "blue", 'Females'='red'))+labs(x ='',y = "Incidence p/100000",fill = "") +geom_bar(aes(fill = factor(Sex)), stat="identity") + theme(plot.title = element_text(hjust = 0.5)) # very important, if you leave this blank the bar he

ggplot(data = crude_totals, mapping = aes(x = Sex, y = total)) +ggtitle("Average Incidence: 2015")+ scale_fill_manual(values=c('Males' = "blue", 'Females'='red'))+labs(x ='',y = "Incidence p/100000",fill = "") +geom_bar(aes(fill = factor(Sex)), stat="identity") + theme(plot.title = element_text(hjust = 0.5)) # very important, if you leave this blank the bar heights are proportional not actual numbers. if you set to identity you also need to instruct y mapping.


ggplot(data = crude_totals, mapping = aes(x = Sex, y = total)) + geom_histogram(aes(fill = factor(Sex)), stat="identity")


#Code for getting the lifetime risk
Male_risk = c((as.numeric(sub("%", "e-2", ASY[1,8]))), (as.numeric(sub("%", "e-2",ASY[22,8]))))
Fem_risk =  c((as.numeric(sub("%", "e-2", ASY[23,8]))), (as.numeric(sub("%", "e-2",ASY[44,8]))))

#Crude rate % increase risk
male_crude_rate_inc = print(df[22,4]/df[1,4])

#Increased LT risk in time period
Men = (((Male_risk[2] - Male_risk[1]) /Male_risk[1])*100)
print(round(Men, digits = 2))

http://stat545.com/block023_dplyr-do.html

#Import second spreadsheet for countywise data, histogram here might be cool to see spread of county data?

ACOUNTY = as.data.frame(read.csv("All_Region_BothSexes.csv"))
ACOUNTY = ACOUNTY[-c(771:773),-c(3:4,6:9)] # SHED THE SHITE
#Gather/group_by might be easier
counties = ACOUNTY %>% 
  group_by(Region) %>% 
  summarise(mean = mean(Crude.rate, na.rm = T))

counties
Look to see if county data is there, make a masterdata frame or spreadsheet

#Mastersheet import

master = as.data.frame(read.csv("Mastersheetxls.csv"))
master2 = master
master$mpc_inv_prostate = NULL
#prostate accounted for just 4.83% of all cancers in 1994 but jumped to 12.1% in 2015, need this for lungs!
big_hitter = master %>% 
  filter(State =='Ireland') %>% 
  filter(Year ==2015 |Year == 1994) %>% 
  summarise(arrange(desc(big_hitter$M4_melan_crude)))

  
  
  
arrange(Inc = big_hitter[2,]/big_hitter[1,])

#county incidence tables
countyincidence_94to15 = as.data.frame(read.csv("INCREASEDATA.csv"))
dfci = countyincidence_94to15 
dfci$Percent_Inc = round(100*((dfci$Rate.2015/dfci$Rate.1994)-1),2)

#Top 10 in 1994
dfci %>% group_by(County) %>%
  arrange(desc(Rate.1994))

#Top 10 in 2015

dfci %>% group_by(County) %>%
  arrange(desc(Rate.2015))
#Biggest Rel Inc
dfci %>% group_by(County) %>%
  arrange(desc(Percent_Inc))
#Smallest Rel Inc
dfci %>% group_by(County) %>%
  arrange((Percent_Inc))




#Back to back bar chart

figure2 = mdf2 %>% 
  filter(State =='Ireland') %>% 
  filter(Year == 2015  | Year == 1994) 

figure2 = mdf %>% 
  filter(State =='Ireland') %>% 
  filter(Year == 2015) 

install.packages("plotrix")
library(plotrix)


xy.pop<-c(figure2[2,3:7])
xx.pop<-c(figure2[2,8:12])

xy.pop<-sort(as.numeric(c(figure2[2,3:7]), decreasing = TRUE))#to have prostate and breast on top
xx.pop<-sort(as.numeric(c(figure2[2,8:12]),decreasing = TRUE))
agelabels<-c("Prostate","Colorectal","Lung","Melanoma","Non-Hodgkins Lymphoma","Breast","Lung","Melanoma","Colorectal", "Uterine")
mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)
par(mar=pyramid.plot(xy.pop,xx.pop,unit="Cases p/100,000",top.labels = c("Male","","Female"),
                     main="Top 5 Cancers 2015",laxlab=c(25,50,75,100,125,150),raxlab=c(0,20,40,60,80,100,120,140),lxcol=mcol,rxcol='red',
                     gap=0.5,show.values=TRUE))


#Incidence Heatmaps
library(rgeos)
library(maptools)

#spdf <- readShapePoly("data/Census2011_Admin_Counties.shp")
setwd("~/Dropbox/Data Science Semester 2/Data Visualisation/Tionscanamh/Census2011_Admin_Counties_generalised20m")
spdf <- readOGR(dsn = ".", layer = "Census2011_Admin_Counties_generalised20m") 

# make it ggplot-friendly

trojans = as.data.frame(read.csv("Heatmap_Incidence_Data.csv"))

#Attempts to fully recode the spdf file with my data were not successful, resorted to processing data in excel until it's format exactly matched those in the spdf@data columns. 
trojan1994 = trojans$All_1994
trojan2005 = trojans$All_2005
trojan2015 = trojans$All_2015

spdf@data$All_1994 = trojan1994
spdf@data$All_2005 = trojan2005
spdf@data$All_2015 = trojan2015

spdf@data$id <- rownames(spdf@data)
spdf.points <- fortify(spdf, region="id")
counties <- inner_join(spdf.points, spdf@data, by="id")
#Create a baselayer template
heatmap_baselayer <- ggplot(counties) + 
  aes(long, lat, group=group) +
  geom_polygon(colour="grey")  + theme_void()
heatmap_baselayer
#rejig the counties here!

#Combined Cancer Incidence, probably want average rate underneath graph in presentation

heatmap_baselayer + aes(fill=All_1994) +
  scale_fill_gradient(limits=c(250,625), low="yellow", high="blue") +
  labs(title = "New Diagnoses 1995", fill="p/100,000")+ theme(plot.title = element_text(hjust = 0.65))

heatmap_baselayer + aes(fill=All_2005) +
  scale_fill_gradient(limits=c(250,625), low="yellow", high="blue") +
  labs(title = "New Diagnoses 2005", fill="p/100,000")+ theme(plot.title = element_text(hjust = 0.65))


heatmap_baselayer + aes(fill=All_2015) +
  scale_fill_gradient(limits=c(250,625), low="yellow", high="blue") +
  labs(title = "New Diagnoses 2015", fill="p/100,000")+ theme(plot.title = element_text(hjust = 0.65))

trojan_mel_2005 = trojans$M4_Melan_2005
trojan_mel_2015 = trojans$M4_Melan_2015
trojan_mel_1994 = trojans$M4_Melan_1994

spdf@data$Melan1994 = trojan_mel_1994
spdf@data$Melan2005 = trojan_mel_2005
spdf@data$Melan2015 = trojan_mel_2015

spdf@data$id <- rownames(spdf@data)
spdf.points <- fortify(spdf, region="id")
counties <- inner_join(spdf.points, spdf@data, by="id")

heatmap_baselayer + aes(fill=Melan1994) +
  scale_fill_gradient(limits=c(1,30), low="yellow",high="red") +
  labs(title = "Melanoma Males 1995", fill="p/100,000")+ theme(plot.title = element_text(hjust = 0.65))

heatmap_baselayer + aes(fill=Melan2005) +
  scale_fill_gradient(limits=c(1,30), low="yellow", high="red") +
  labs(title = "Melanoma Males 2005", fill="p/100,000")+ theme(plot.title = element_text(hjust = 0.65))


heatmap_baselayer + aes(fill=Melan2015) +
  scale_fill_gradient(limits=c(1,30),low="yellow", high="red", oob = scales::squish) +
  labs(title = "Melanoma Males 2015", fill="p/100,000")+ theme(plot.title = element_text(hjust = 0.65))

#Boxplot of the relative increases to spot melanoma as an outlier?
melan3 = master %>% 
  filter(State !='Ireland') %>% 
  filter(Year ==2015 |Year == 1994) %>% 
  group_by(Year,State,.dots=c("M4_melan_crude", "F4_melan_crude")) %>% 
  summarise()

master = as.data.frame(read.csv("Mastersheetxls.csv"))

melan3$State = NULL
melan3$Year = factor(melan3$Year)
melan3$group <- row.names(melan3)
#Show mean rates for each sex in 1994 & 2015
group_by(melan3,Year) %>% 
  summarise(mean = mean(M4_melan_crude))
group_by(melan3,Year) %>% 
  summarise(mean = mean(F4_melan_crude))
dfm

dfm = melt(melan3, id.vars = c("Year"),
             measure.vars = c("M4_melan_crude", "F4_melan_crude"))

bpm = boxplot(dfm$value~dfm$variable*dfm$Year, outline = FALSE, col =(c("blue","red","blue","red")),main='Distribution of Melanoma Rates Across Counties', xlab = "", names = c("1994","1994","2015","2015"), ylab = "Rate p/100000")

legend('topleft', bty = 'n',legend = c("Male","Female"), col =c("blue", "red"),pch=c(15,15), cex = 1.5)

levels(DT.m1$variable)
rel_increases = as.numeric(big_hitter[2,])/as.numeric(big_hitter[1,])
boxplot(rel_increases, col = 'purple',outcol = 'red', main = "Distribution of Relative Increases 1994:2015",ylab = "Relative Increase in Cancer Rate", horizontal = T)
abline(v = 1.34, col = 'yellow', lwd = 3)

dat.m <- melt(melan3, id.vars = "Year")
ggplot(DT.m1, aes(Year, value)) + geom_boxplot(outlier.colour = 'red')

bp <- ggplot(DT.m1, aes(x=id~variable, y=value)) + 
  geom_boxplot()
#Looks like it works!

group_by(melan3,Year) %>% 
  summarise(mean = mean(M4_melan_crude))

group_by(melan3,Year) %>% 
  summarise(mean = mean(F4_melan_crude))


