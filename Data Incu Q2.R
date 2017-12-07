MERGED2013_14_PP <- read_csv("C:/Users/Isaac/Desktop/CollegeScorecard_Raw_Data/MERGED2013_14_PP.csv")
attach(MERGED2013_14_PP)

#########
####Average SAT Score
#########
new<-subset(MERGED2013_14_PP,SAT_AVG!='NULL')
attach(new)
mean(as.numeric(SAT_AVG))

#########
####DIFFERENCE IN COMPLETION RATE BETWEEN LOW AND HIGH INCOME COMPLETION RATE FOR 4 YEAR COLLEGE
########
diff<-subset(MERGED2013_14_PP,LO_INC_COMP_ORIG_YR4_RT!='PrivacySuppressed' & LO_INC_COMP_ORIG_YR4_RT!='NULL' & MD_INC_COMP_ORIG_YR4_RT!='PrivacySuppressed' &MD_INC_COMP_ORIG_YR4_RT!='NULL'& HI_INC_COMP_ORIG_YR4_RT!='PrivacySuppressed'& HI_INC_COMP_ORIG_YR4_RT!='NULL')
attach(diff)
str(LO_INC_COMP_ORIG_YR4_RT)
a<-mean(as.numeric(LO_INC_COMP_ORIG_YR4_RT))
b<-mean(as.numeric(HI_INC_COMP_ORIG_YR4_RT))
m=abs(a-b)
m

##########
###DIVERSITY OF RACE IN UNIVERSITIES
########
div<-subset(MERGED2013_14_PP,UGDS_WHITE!='NULL'&UGDS_WHITE!='0'& UGDS_BLACK!='NULL'&UGDS_BLACK!='0'& UGDS_HISP!='NULL'&UGDS_HISP!='0'& UGDS_ASIAN!='NULL'& UGDS_ASIAN!='0'& UGDS_AIAN!='NULL'&UGDS_AIAN!='0'& UGDS_NHPI!='NULL'&UGDS_NHPI!='0'& UGDS_2MOR!='NULL'& UGDS_2MOR!='0'& UGDS_NRA!='NULL'&UGDS_NRA!='0'& UGDS_UNKN!='NULL'&UGDS_UNKN!='0')
attach(div)
names(div)
div$max<-apply(div[,293:301],1,max)
head(div$max)
div$min<-apply(div[,293:301],1,min)
head(div$min)
div$diff<-as.numeric(div$max)-as.numeric(div$min)
head(div$diff)
max(div$diff)
which(div$diff=='0.9752')
div[1581,]


##########
####PEARSON CORRELATION BETWEEN AVERAGE SAT SCORE AND ENROLLMENT OF 2 YEAR UNIVERSITY PROGRAM 
#########
Pcor<-subset(MERGED2013_14_PP,SAT_AVG!='NULL' & ENRL_ORIG_YR2_RT!='PrivacySuppressed'&ENRL_ORIG_YR2_RT!='NULL')
cor(as.numeric(Pcor$SAT_AVG),as.numeric(Pcor$ENRL_ORIG_YR2_RT),method = "pearson")


###########
###TWO SAMPLE T TEST FOR COMPLETION RATE IN LOW AND HIGH INCOME UNIVERSITY
##########
t.test(as.numeric(diff$LO_INC_COMP_ORIG_YR4_RT),as.numeric(diff$HI_INC_COMP_ORIG_YR4_RT), var.equal=TRUE, paired=FALSE)
log10(2.2*exp(-16))



#########
####AVERAGE SHARE OF WOMEN ENROLLMENT IN UNIVERSITY FROM 2001 TO 2011
########
library(readr)
MERGED2001_02_PP <- read_csv("C:/Users/Isaac/Desktop/CollegeScorecard_Raw_Data/MERGED2001_02_PP.csv")
MERGED2002_03_PP <- read_csv("C:/Users/Isaac/Desktop/CollegeScorecard_Raw_Data/MERGED2002_03_PP.csv")
MERGED2003_04_PP <- read_csv("C:/Users/Isaac/Desktop/CollegeScorecard_Raw_Data/MERGED2003_04_PP.csv")
MERGED2004_05_PP <- read_csv("C:/Users/Isaac/Desktop/CollegeScorecard_Raw_Data/MERGED2004_05_PP.csv")
MERGED2005_06_PP <- read_csv("C:/Users/Isaac/Desktop/CollegeScorecard_Raw_Data/MERGED2005_06_PP.csv")
MERGED2006_07_PP <- read_csv("C:/Users/Isaac/Desktop/CollegeScorecard_Raw_Data/MERGED2006_07_PP.csv")
MERGED2007_08_PP <- read_csv("C:/Users/Isaac/Desktop/CollegeScorecard_Raw_Data/MERGED2007_08_PP.csv")
MERGED2008_09_PP <- read_csv("C:/Users/Isaac/Desktop/CollegeScorecard_Raw_Data/MERGED2008_09_PP.csv")
MERGED2009_10_PP <- read_csv("C:/Users/Isaac/Desktop/CollegeScorecard_Raw_Data/MERGED2009_10_PP.csv")
MERGED2010_11_PP <- read_csv("C:/Users/Isaac/Desktop/CollegeScorecard_Raw_Data/MERGED2010_11_PP.csv")
TT<-rbind(MERGED2001_02_PP,MERGED2002_03_PP,MERGED2003_04_PP,MERGED2004_05_PP,MERGED2005_06_PP,MERGED2006_07_PP,MERGED2007_08_PP,MERGED2008_09_PP,MERGED2009_10_PP,MERGED2010_11_PP)
Enrol<-subset(TT,TT$UGDS_WOMEN!='NULL')
mean(as.numeric(Enrol$UGDS_WOMEN))



