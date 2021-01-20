setwd("~/Dropbox/Data Science Semester 2/Data Visualisation/Tionscanamh")
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

#Import first spreadsheet used, combined incidence for all cancer categorised by sex
A_SEXANDYEAR = as.data.frame(read.csv("All_sex_and_year.csv"))
ASY = A_SEXANDYEAR[-c(67:69),]#remove comment notes
ASY$SEX = factor(ASY$Sex)
df = as.data.frame(ASY[c(1:3,5)]) #isolate the columns we want; sex, year and crude rate 
df$Sex = factor(df$Sex)

#Category both was simply an average of male/female and was considered distracting so removed it for graphs
dfb = df %>% 
  group_by(Sex) %>% 
  filter(Sex != 'Both')
View(dfb)
levels(dfb$Sex)
dfb$Sex = droplevels(dfb$Sex, "Both")

dfb$Sex = factor(dfb$Sex, levels = c("Males","Females")) #necessary for correct order in line graph legend


#Figure 1 Code: Plot of overall increase during study period
p = ggplot(dfb, aes(Year, Crude.rate))

p + geom_line(aes(colour = Sex)) +ggtitle("Overall Diagnosis Rates") +scale_color_manual(name = '',labels = c("Males","Females"),values=c('Males' = "blue", 'Females'='red')) + labs(x = NULL, y = 'New Diagnoses p/100000', legend = NULL) +theme(plot.title = element_text(hjust = .5),legend.justification=c(1,0),legend.position=c(0.95, 0.05), legend.background = element_blank(),legend.key = element_blank())

#Barchart of male/female figures (not used in final report)


crude_totals = dfb %>% 
  group_by(Sex) %>% 
  df_94_15 = filter(Year == 1994 | Year ==2015 ) %>% 
  df_94_15 %>% group_by(Year) %>% 
  summarize(total = mean(Crude.rate))

ggplot(data = crude_totals, mapping = aes(x = Sex, y = total)) +ggtitle("Average Incidence: 2015")+ scale_fill_manual(values=c('Males' = "blue", 'Females'='red'))+labs(x ='',y = "Incidence p/100000",fill = "") +geom_bar(aes(fill = factor(Sex)), stat="identity") + theme(plot.title = element_text(hjust = 0.5)) # very important, if you leave this blank the bar heights are proportional not actual numbers. if you set to identity you also need to instruct y mapping.

#Same but with histogram, not used in report
ggplot(data = crude_totals, mapping = aes(x = Sex, y = total)) + geom_histogram(aes(fill = factor(Sex)), stat="identity")


#Code for getting the lifetime risk
Male_risk = c((as.numeric(sub("%", "e-2", ASY[1,8]))), (as.numeric(sub("%", "e-2",ASY[22,8]))))
Fem_risk =  c((as.numeric(sub("%", "e-2", ASY[23,8]))), (as.numeric(sub("%", "e-2",ASY[44,8]))))

#Crude rate % increase risk
male_crude_rate_inc = print(df[22,4]/df[1,4])

#Increased LT risk in time period
Men = (((Male_risk[2] - Male_risk[1]) /Male_risk[1])*100)
print(round(Men, digits = 2))


#Import second spreadsheet for countywise data

ACOUNTY = as.data.frame(read.csv("All_Region_BothSexes.csv"))
ACOUNTY = ACOUNTY[-c(771:773),-c(3:4,6:9)] # Remove notes and un-necessary fields

counties = ACOUNTY %>% 
  group_by(Region) %>% 
  summarise(mean = mean(Crude.rate, na.rm = T))

#Mastersheet Import: Contains figures for each of top 10 most common cancers in 1995 & 2015 as well as 2005 for melanoma and combined cancer (which were needed for heatmaps)

master = as.data.frame(read.csv("Mastersheetxls.csv"))
master$mpc_inv_prostate = NULL #artifact column with risk of develeoping prostate cancer
#prostate accounted for just 4.83% of all cancers in 1994 but jumped to 12.1% in 2015, need this for lungs!

Top10_ireland_1994vs2015 = master %>% 
  filter(State =='Ireland') %>% 
  filter(Year ==2015 |Year == 1994) %>% 
  summarise(arrange(desc(Top10_ireland_1994vs2015$M4_melan_crude))) # change column here for cancer of interest


#county incidence tables
countyincidence_94to15 = as.data.frame(read.csv("INCREASEDATA.csv"))
dfci = countyincidence_94to15 
dfci$Percent_Inc = round(100*((dfci$Rate.2015/dfci$Rate.1994)-1),2) # Column giving relative increase for each cancer during study period

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




#Figure 2: Back to back bar chart of top 5 cancers in each sex, package was very cumbersome to work with and required a lot of external manipulation for report (seperate labels for each side was not allowed in plotrix)

hbar_chart = master %>% 
  filter(State =='Ireland') %>% 
  filter(Year == 2015  | Year == 1994) 

install.packages("plotrix")
library(plotrix)


xy.pop<-c(hbar_chart[2,3:7])
xx.pop<-c(hbar_chart[2,8:12])
xy.pop<-sort(as.numeric(c(hbar_chart[2,3:7]), decreasing = TRUE))#to have prostate and breast on top
xx.pop<-sort(as.numeric(c(hbar_chart[2,8:12]),decreasing = TRUE))
agelabels<-c("Prostate","Colorectal","Lung","Melanoma","Non-Hodgkins Lymphoma","Breast","Lung","Melanoma","Colorectal", "Uterine") # had to be added manually
mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)
par(mar=pyramid.plot(xy.pop,xx.pop,unit="Cases p/100,000",top.labels = c("Male","","Female"),
                     main="Top 5 Cancers 2015",laxlab=c(25,50,75,100,125,150),raxlab=c(0,20,40,60,80,100,120,140),lxcol=mcol,rxcol='red',
                     gap=0.5,show.values=TRUE))


#FIgure 3 and 5: Incidence Heatmaps

library(rgeos)
library(maptools)

#spdf <- readShapePoly("data/Census2011_Admin_Counties.shp")
setwd("~/Dropbox/Data Science Semester 2/Data Visualisation/Tionscanamh/Census2011_Admin_Counties_generalised20m")
spdf <- readOGR(dsn = ".", layer = "Census2011_Admin_Counties_generalised20m") #read in spdf data object containing map longitude & latitude. Source: http://rpubs.com/BrunoVoisin/csomaps



#Attempts to fully recode the spdf file with my data were not successful, resorted to processing data in excel until it's format exactly matched those in the spdf@data columns. 

trojans = as.data.frame(read.csv("Heatmap_Incidence_Data.csv")) #cancer figures with row names matching those used in CSO presentation cited above
trojan1995 = trojans$All_1995
trojan2005 = trojans$All_2005
trojan2015 = trojans$All_2015

spdf@data$All_1995 = trojan1995
spdf@data$All_2005 = trojan2005
spdf@data$All_2015 = trojan2015

#Join and process dataframe for use in ggplot
spdf@data$id <- rownames(spdf@data)
spdf.points <- fortify(spdf, region="id")
counties <- inner_join(spdf.points, spdf@data, by="id")
#Create a baselayer template
heatmap_baselayer <- ggplot(counties) + 
  aes(long, lat, group=group) +
  geom_polygon(colour="grey")  + theme_void()

#Combined Cancer Incidence Heatmap. Tweaked template to scale_fill_gradient rather than scale_fill_gradient2 as original function had superior image quality. Different limits and colour schemes tested systematically
heatmap_baselayer + aes(fill=All_1995) +
  scale_fill_gradient(limits=c(250,625), low="yellow", high="blue") +
  labs(title = "New Diagnoses 1995", fill="p/100,000")+ theme(plot.title = element_text(hjust = 0.65))

heatmap_baselayer + aes(fill=All_2005) +
  scale_fill_gradient(limits=c(250,625), low="yellow", high="blue") +
  labs(title = "New Diagnoses 2005", fill="p/100,000")+ theme(plot.title = element_text(hjust = 0.65))


heatmap_baselayer + aes(fill=All_2015) +
  scale_fill_gradient(limits=c(250,625), low="yellow", high="blue") +
  labs(title = "New Diagnoses 2015", fill="p/100,000")+ theme(plot.title = element_text(hjust = 0.65))

#FIgure 5: Melanoma Heatmap

trojan_mel_2005 = trojans$M4_Melan_2005
trojan_mel_2015 = trojans$M4_Melan_2015
trojan_mel_1994 = trojans$M4_Melan_1994

spdf@data$Melan1994 = trojan_mel_1994
spdf@data$Melan2005 = trojan_mel_2005
spdf@data$Melan2015 = trojan_mel_2015

spdf@data$id <- rownames(spdf@data)
spdf.points <- fortify(spdf, region="id")
counties <- inner_join(spdf.points, spdf@data, by="id")

#Scale & colour scheme chosen because of visual parallel with sunburning process

heatmap_baselayer + aes(fill=Melan1995) +
  scale_fill_gradient(limits=c(1,30), low="yellow",high="red") +
  labs(title = "Melanoma Males 1995", fill="p/100,000")+ theme(plot.title = element_text(hjust = 0.65))

heatmap_baselayer + aes(fill=Melan2005) +
  scale_fill_gradient(limits=c(1,30), low="yellow", high="red") +
  labs(title = "Melanoma Males 2005", fill="p/100,000")+ theme(plot.title = element_text(hjust = 0.65))


heatmap_baselayer + aes(fill=Melan2015) +
  scale_fill_gradient(limits=c(1,30),low="yellow", high="red", oob = scales::squish) +
  labs(title = "Melanoma Males 2015", fill="p/100,000")+ theme(plot.title = element_text(hjust = 0.65))

#Figure 4/5 Prep: Boxplot of the relative increases to spot melanoma as an outlier and then compare sex differences

melan3 = master %>% 
  filter(State !='Ireland') %>% #exclude the nationwide average rows
  filter(Year ==2015 |Year == 1994) %>% 
  group_by(Year,State,.dots=c("M4_melan_crude", "F4_melan_crude")) %>% #.dots argument required to select specific columns
  summarise()

melan3$State = NULL #remove unwanted regional data
melan3$Year = factor(melan3$Year)
melan3$group <- row.names(melan3)
#Show mean rates for each sex in 1994 & 2015
group_by(melan3,Year) %>% 
  summarise(mean = mean(M4_melan_crude))
group_by(melan3,Year) %>% 
  summarise(mean = mean(F4_melan_crude))

#Melt data into long format

dfm = melt(melan3, id.vars = c("Year"),
           measure.vars = c("M4_melan_crude", "F4_melan_crude"))

# Figure 5.  Insert * to compare different combinations of year & sex
bpm = boxplot(dfm$value~dfm$variable*dfm$Year, outline = FALSE, col =(c("blue","red","blue","red")),main='Distribution of Melanoma Rates Across Counties', xlab = "", names = c("1994","1994","2015","2015"), ylab = "Rate p/100000")
legend('topleft', bty = 'n',legend = c("Male","Female"), col =c("blue", "red"),pch=c(15,15), cex = 1.5)

# Figure 4

rel_increases = as.numeric(countyincidence_94to15[2,])/as.numeric(countyincidence_94to15[1,]) #divide 2015 rate by 1994 rate to get relative increase
boxplot(rel_increases, col = 'purple',outcol = 'red', main = "Distribution of Relative Increases 1994:2015",ylab = "Relative Increase in Cancer Rate", horizontal = T)
abline(v = 1.34, col = 'yellow', lwd = 3)



