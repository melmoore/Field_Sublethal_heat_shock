#FIELD SUBLETHAL CLEANING SCRIPT

#load libraries

library(readr)
library(dplyr)
library(tidyr)



#----------------

#load data

slhs <- read_csv("data files/Field_slhs_comp_raw.csv", 
                 col_types = cols(test.temp = col_factor(levels = c("35", "40", "42", "43", "44")), 
                                  treat.hs = col_factor(levels = c("control", "shock")), 
                                  treat.para = col_factor(levels = c("np", "p"))))
View(slhs)


#---------

#remove empty rows at bottom of data frame

slhs$id[is.na(slhs$id)]<-0

slhs<-subset(slhs, id>0)



#------------------

#Transform date columns into julian date

##Converts x into julian date
j.date<-function(x){
  strptime(x, "%m/%d")$yday+1
}


#Takes all columns that have "date." in the name, and converts contents to Julian day using j.date function. Renames columns (adds a 
##j to end of column name), and binds the out put julian day columns to the original data set

lapj.date<-function(df){
  date.j<-lapply(df[,grep("date.",colnames(df))],j.date)
  date.j<-as.data.frame(date.j)
  colnames(date.j)<-paste(colnames(date.j), "j", sep = ".")
  output.df<-cbind(df,date.j)
  output.df
}

slhs<-lapj.date(slhs)

#--------------

#attempting to make the mongo tracking columns (date5.2wks, date.p6.wk1, etc) more cohesive and useful

#make NAs 0s in post 5th date and mass columns so that they're useable in ifelse calls
slhs$date5.2wks.j[is.na(slhs$date5.2wks.j)]<-0
slhs$date5.3wks.j[is.na(slhs$date5.3wks.j)]<-0
slhs$date.p6.wk1.j[is.na(slhs$date.p6.wk1.j)]<-0
slhs$date.p6.wk2.j[is.na(slhs$date.p6.wk2.j)]<-0
slhs$date.p6.wk3.j[is.na(slhs$date.p6.wk3.j)]<-0
slhs$date.p7.wk1.j[is.na(slhs$date.p7.wk1.j)]<-0
slhs$date.cull.j[is.na(slhs$date.cull.j)]<-0
slhs$date.died.j[is.na(slhs$date.died.j)]<-0
slhs$date.wand.j[is.na(slhs$date.wand.j)]<-0
slhs$date.6.j[is.na(slhs$date.6.j)]<-0

slhs$mass5.2wks[is.na(slhs$mass5.2wks)]<-0
slhs$mass5.3wks[is.na(slhs$mass5.3wks)]<-0
slhs$mass.p6.wk1[is.na(slhs$mass.p6.wk1)]<-0
slhs$mass.p6.wk2[is.na(slhs$mass.p6.wk2)]<-0
slhs$mass.p6.wk3[is.na(slhs$mass.p6.wk3)]<-0
slhs$mass.p7.wk1[is.na(slhs$mass.p7.wk1)]<-0
slhs$mass.cull[is.na(slhs$mass.cull)]<-0
slhs$mass.wand[is.na(slhs$mass.wand)]<-0
slhs$mass.6[is.na(slhs$mass.wand)]<-0


#Make binary sorting columns based on if the mongo was weighed post 5th, post 6th or post 7th
slhs$p5<-ifelse(slhs$date5.2wks.j > 0, 1, 0)
slhs$p6<-ifelse(slhs$date.p6.wk1.j > 0, 1, 0)
slhs$p7<-ifelse(slhs$date.p7.wk1.j > 0, 1, 0)
slhs$pwand<-ifelse(slhs$date.wand.j > 0, 1, 0)

#subset to only parasitized individuals
slhs.para<-subset(slhs, treat.para=="p")


check<-slhs.para[,c("date5.2wks.j", "date.p6.wk1.j", "date.p7.wk1.j", "date.wand.j", "p5", "p6", "p7", "pwand")]


#subset by combination of post 5th binary columns
slhs.p5<-subset(slhs.para, p5=="1" & p6=="0" & p7=="0")
slhs.p5p6<-subset(slhs.para, p5=="1" & p6=="1" & p7=="0")
slhs.p6<-subset(slhs.para, p5=="0" & p6=="1" & p7=="0")
slhs.p6p7<-subset(slhs.para, p5=="0" & p6=="1" & p7=="1")
slhs.pwand<-subset(slhs.para, p5=="0" & p6=="0" & p7=="0" & pwand=="1")
slhs.rest<-subset(slhs.para, p5=="0" & p6=="0" & p7=="0" & pwand=="0")

#create columns in the format of date.p5.1, mass.p5.1, inst.p5.1, date.p5.2.....etc

#Mongos that did not molt to 6th--post 5, 1st measurement
slhs.p5$date.p5.1<-slhs.p5$date5.2wks.j
slhs.p5$mass.p5.1<-slhs.p5$mass5.2wks
slhs.p5$inst.p5.1<-"5"

#Mongos that did not molt to 6th--post 5, 2nd measurement
slhs.p5$date.p5.2<-ifelse(slhs.p5$date5.3wks.j > 0, slhs.p5$date5.3wks.j,
                          ifelse(slhs.p5$date.cull.j > 0, slhs.p5$date.cull.j,
                                 ifelse(slhs.p5$date.died.j > 0, slhs.p5$date.died.j,
                                        ifelse(slhs.p5$date.wand.j >0, slhs.p5$date.wand.j, 0))))

slhs.p5$mass.p5.2<-ifelse(slhs.p5$mass5.3wks > 0, slhs.p5$mass5.3wks,
                          ifelse(slhs.p5$mass.wand > 0, slhs.p5$mass.wand,
                          ifelse(slhs.p5$mass.cull > 0, slhs.p5$mass.cull, 0)))

slhs.p5$inst.p5.2<-ifelse(slhs.p5$date5.3wks.j > 0, 5,
                          ifelse(slhs.p5$date.cull.j > 0, "cull",
                                 ifelse(slhs.p5$date.died.j > 0, "died",
                                        ifelse(slhs.p5$date.wand.j > 0, "wand", 0))))

#Mongos that did not molt to 6th--post 5, 3rd and 4th measurement proxy columns

slhs.p5$date.p5.3<-0
slhs.p5$mass.p5.3<-0
slhs.p5$inst.p5.3<-0

slhs.p5$date.p5.4<-0
slhs.p5$mass.p5.4<-0
slhs.p5$inst.p5.4<-0

slhs.p5$date.p5.5<-0
slhs.p5$mass.p5.5<-0
slhs.p5$inst.p5.5<-0


#Mongos that molted 6th before p5.2wk cut off, and did not molt to 7th
  ##post 5th, 1st measurement

slhs.p6$date.p5.1<-slhs.p6$date.6.j
slhs.p6$mass.p5.1<-slhs.p6$mass.6
slhs.p6$inst.p5.1<-"6"


#Mongos that molted 6th before p5.2wk cut off, and did not molt to 7th
##post 5th, 2nd measurement

slhs.p6$date.p5.2<-slhs.p6$date.p6.wk1.j
slhs.p6$mass.p5.2<-slhs.p6$mass.p6.wk1
slhs.p6$inst.p5.2<-"6"


#Mongos that molted 6th before p5.2wk cut off, and did not molt to 7th
##post 5th, 3rd measurement

slhs.p6$date.p5.3<-ifelse(slhs.p6$date.p6.wk2.j > 0, slhs.p6$date.p6.wk2.j,
                          ifelse(slhs.p6$date.cull.j > 0, slhs.p6$date.cull.j,
                                 ifelse(slhs.p6$date.wand.j > 0, slhs.p6$date.wand.j,
                                        ifelse(slhs.p6$date.died.j > 0, slhs.p6$date.died.j, 0))))

slhs.p6$mass.p5.3<-ifelse(slhs.p6$mass.p6.wk2 > 0, slhs.p6$mass.p6.wk2,
                          ifelse(slhs.p6$mass.cull > 0, slhs.p6$mass.cull,
                                 ifelse(slhs.p6$mass.wand > 0, slhs.p6$mass.wand, 0)))

slhs.p6$inst.p5.3<-ifelse(slhs.p6$date.p6.wk2.j > 0, "6",
                          ifelse(slhs.p6$date.cull.j > 0, "cull",
                                 ifelse(slhs.p6$date.wand.j > 0, "wand",
                                        ifelse(slhs.p6$date.died.j > 0, "died", 0))))


#Mongos that molted 6th before p5.2wk cut off, and did not molt to 7th
##post 5th, 4th measurement

slhs.p6$date.p5.4<-ifelse(slhs.p6$date.p6.wk3.j > 0, slhs.p6$date.p6.wk3.j,
                          ifelse(slhs.p6$date.cull.j > 0 & slhs.p6$inst.p5.3 !="cull", 
                                 slhs.p6$date.cull.j,
                                 ifelse(slhs.p6$date.wand.j > 0 & slhs.p6$inst.p5.3 != "wand", 
                                        slhs.p6$date.wand.j,
                                        ifelse(slhs.p6$date.died.j > 0 & slhs.p6$inst.p5.3 !="died", 
                                               slhs.p6$date.died.j, 0))))

slhs.p6$mass.p5.4<-ifelse(slhs.p6$mass.p6.wk3 > 0, slhs.p6$mass.p6.wk3,
                          ifelse(slhs.p6$mass.cull > 0 & slhs.p6$inst.p5.3 !="cull",
                                 slhs.p6$mass.cull,
                                 ifelse(slhs.p6$mass.wand > 0 & slhs.p6$inst.p5.3 !="wand",
                                        slhs.p6$mass.wand, 0)))


slhs.p6$inst.p5.4<-ifelse(slhs.p6$date.p6.wk3.j > 0, "6",
                          ifelse(slhs.p6$date.cull.j > 0 & slhs.p6$inst.p5.3 != "cull",
                                 "cull",
                                 ifelse(slhs.p6$date.wand.j > 0 & slhs.p6$inst.p5.3 != "wand",
                                        "wand",
                                        ifelse(slhs.p6$date.died.j > 0 & slhs.p6$inst.p5.3 !="died", 
                                               "died", 0))))


#Mongos that molted 6th before p5.2wk cut off, and did not molt to 7th
##post 5th, 4th measurement

slhs.p6$date.p5.5<-ifelse(slhs.p6$date.cull.j > 0 & slhs.p6$inst.p5.4 != "cull" & slhs.p6$inst.p5.3 != "cull",
                          slhs.p6$date.cull.j,
                                 ifelse(slhs.p6$date.wand.j > 0 & slhs.p6$inst.p5.4 != "wand" & 
                                          slhs.p6$inst.p5.3 != "wand", slhs.p6$date.wand.j,
                                        ifelse(slhs.p6$date.died.j > 0 & slhs.p6$inst.p5.4 !="died", 
                                               slhs.p6$date.died.j, 0)))

slhs.p6$mass.p5.5<-ifelse(slhs.p6$mass.cull > 0 & slhs.p6$inst.p5.4 != "cull" & slhs.p6$inst.p5.3 != "cull",
                          slhs.p6$mass.cull,
                                   ifelse(slhs.p6$mass.wand > 0 & slhs.p6$inst.p5.4 != "wand" &
                                            slhs.p6$inst.p5.3 != "wand",
                                          slhs.p6$mass.wand, 0))

slhs.p6$inst.p5.5<-ifelse(slhs.p6$date.cull.j > 0 & slhs.p6$inst.p5.2 != "cull" & slhs.p6$inst.p5.4 != "cull",
                          "cull",
                          ifelse(slhs.p6$date.wand.j > 0 & slhs.p6$inst.p5.4 != "wand" & slhs.p6$inst.p5.3 != "wand", 
                                 "wand",
                                 ifelse(slhs.p6$date.died.j > 0 & slhs.p6$inst.p5.4 !="died" & slhs.p6$inst.p5.3 != "died",
                                 "died", 0)))



#Mongos that molted to 5th, reached 2wks and molted to 6th
##post 5th, 1st measurement

slhs.p5p6$date.p5.1<-slhs.p5p6$date5.2wks.j
slhs.p5p6$mass.p5.1<-slhs.p5p6$mass5.2wks
slhs.p5p6$inst.p5.1<-"5"


#Mongos that molted to 5th, reached 2wks and molted to 6th
##post 5th, 2nd measurement
slhs.p5p6$date.p5.2<-slhs.p5p6$date.p6.wk1.j
slhs.p5p6$mass.p5.2<-slhs.p5p6$mass.p6.wk1
slhs.p5p6$inst.p5.2<-"6"

#Mongos that molted to 5th, reached 2wks and molted to 6th
##post 5th, 3rd measurement
slhs.p5p6$date.p5.3<-slhs.p5p6$date.cull.j
slhs.p5p6$mass.p5.3<-slhs.p5p6$mass.cull
slhs.p5p6$inst.p5.3<-"cull"


#Mongos that molted to 5th, reached 2wks and molted to 6th
##post 5th, 4th and 5th measurement proxy

slhs.p5p6$date.p5.4<-0
slhs.p5p6$mass.p5.4<-0
slhs.p5p6$inst.p5.4<-0

slhs.p5p6$date.p5.5<-0
slhs.p5p6$mass.p5.5<-0
slhs.p5p6$inst.p5.5<-0


#Mongos that molted to 6th and 7th
##post 5th, 1st measurement

slhs.p6p7$date.p5.1<-slhs.p6p7$date.6.j
slhs.p6p7$mass.p5.1<-slhs.p6p7$mass.6
slhs.p6p7$inst.p5.1<-"6"


#Mongos that molted to 6th and 7th
##post 5th, 2nd measurement

slhs.p6p7$date.p5.2<-slhs.p6p7$date.p6.wk1.j
slhs.p6p7$mass.p5.2<-slhs.p6p7$mass.p6.wk1
slhs.p6p7$inst.p5.2<-"6"


#Mongos that molted to 6th and 7th
##post 5th, 3rd measurement

slhs.p6p7$date.p5.3<-slhs.p6p7$date.7.j
slhs.p6p7$mass.p5.3<-slhs.p6p7$mass.7
slhs.p6p7$inst.p5.3<-"7"


#Mongos that molted to 6th and 7th
##post 5th, 4th measurement

slhs.p6p7$date.p5.4<-slhs.p6p7$date.p7.wk1.j
slhs.p6p7$mass.p5.4<-slhs.p6p7$mass.p7.wk1
slhs.p6p7$inst.p5.4<-"7"


#Mongos that molted to 6th and 7th
##post 5th, 5th measurement

slhs.p6p7$date.p5.5<-slhs.p6p7$date.cull.j
slhs.p6p7$mass.p5.5<-slhs.p6p7$mass.cull
slhs.p6p7$inst.p5.5<-"cull"




#Mongos that wandered before post 5th time points
  ## 1st measurement

slhs.pwand$date.p5.1<-ifelse(slhs.pwand$date.6.j > 0, slhs.pwand$date.6.j,
                             ifelse(slhs.pwand$date.wand.j > 0, slhs.pwand$date.wand.j, 0))

slhs.pwand$mass.p5.1<-ifelse(slhs.pwand$mass.6 > 0, slhs.pwand$mass.6,
                             ifelse(slhs.pwand$mass.wand > 0, slhs.pwand$mass.wand, 0))

slhs.pwand$inst.p5.1<-ifelse(slhs.pwand$date.6.j > 0, "6",
                             ifelse(slhs.pwand$date.wand.j > 0, "wand", 0))


#Mongos that wandered before post 5th time points
## 2nd measurement

slhs.pwand$date.p5.2<-ifelse(slhs.pwand$date.wand.j > 0 & slhs.pwand$inst.p5.1 != "wand",
                             slhs.pwand$date.wand.j, 0)

slhs.pwand$mass.p5.2<-ifelse(slhs.pwand$mass.wand > 0 & slhs.pwand$inst.p5.1 != "wand",
                             slhs.pwand$mass.wand, 0)

slhs.pwand$inst.p5.2<-ifelse(slhs.pwand$date.wand.j > 0 & slhs.pwand$inst.p5.1 != "wand",
                             "wand", 0)



#Making dummy columns for slhs.pwand to match the column numbers of the other datasets

slhs.pwand$date.p5.3<-0
slhs.pwand$mass.p5.3<-0
slhs.pwand$inst.p5.3<-0

slhs.pwand$date.p5.4<-0
slhs.pwand$mass.p5.4<-0
slhs.pwand$inst.p5.4<-0

slhs.pwand$date.p5.5<-0
slhs.pwand$mass.p5.5<-0
slhs.pwand$inst.p5.5<-0



#Making dummy columns for non-mongos and dead individuals, so they can be recombined with the mongo data

slhs.rest$date.p5.1<-0
slhs.rest$mass.p5.1<-0
slhs.rest$inst.p5.1<-0

slhs.rest$date.p5.2<-0
slhs.rest$mass.p5.2<-0
slhs.rest$inst.p5.2<-0

slhs.rest$date.p5.3<-0
slhs.rest$mass.p5.3<-0
slhs.rest$inst.p5.3<-0

slhs.rest$date.p5.4<-0
slhs.rest$mass.p5.4<-0
slhs.rest$inst.p5.4<-0

slhs.rest$date.p5.5<-0
slhs.rest$mass.p5.5<-0
slhs.rest$inst.p5.5<-0


#Recombining slhs.para from the subsetted data that now has post 5th columns

slhs.para<-rbind(slhs.p5, slhs.p5p6, slhs.p6, slhs.p6p7, slhs.pwand, slhs.rest)



#------------------

#Recombine para and nonpara data sets, with the post 5th mongo columns


#Subset out np caterpillars

slhs.np<-subset(slhs, treat.para=="np")


#Make dummy post 5th columns

slhs.np$date.p5.1<-0
slhs.np$mass.p5.1<-0
slhs.np$inst.p5.1<-0

slhs.np$date.p5.2<-0
slhs.np$mass.p5.2<-0
slhs.np$inst.p5.2<-0

slhs.np$date.p5.3<-0
slhs.np$mass.p5.3<-0
slhs.np$inst.p5.3<-0

slhs.np$date.p5.4<-0
slhs.np$mass.p5.4<-0
slhs.np$inst.p5.4<-0

slhs.np$date.p5.5<-0
slhs.np$mass.p5.5<-0
slhs.np$inst.p5.5<-0


#recombine slhs.para and slhs.np datasets

slhs<-rbind(slhs.np, slhs.para)


#--------------

#Make sorting column to clean dead individuals that did not die as mongos


slhs$died.toss<-ifelse(slhs$date.died.j > 0 & slhs$inst.p5.1 != "died" & slhs$inst.p5.2 != "died"
                       & slhs$inst.p5.3 != "died" & slhs$inst.p5.4 != "died" & slhs$inst.p5.5 != "died", 1, 0)



#------------------

#Converting clock time to decimal time

#Function that turns turns time (x) into a character, splits it at the :, and adds it together to get decimal time
dec.time<-function(x) {
  x<-as.character(x)
  sapply(strsplit(x,":"),function(x){
    x <- as.numeric(x)
    y<-x[1]+x[2]/60
    
  })
}


#Function that applies the dec.time function to every column with "time." in the name, and adds decminal time columns to 
##dataframe
dec.time.col<-function(df){
  dct<-lapply(df[,grep("time.",colnames(df))],dec.time)
  dct<-as.data.frame(dct)
  colnames(dct)<-paste(colnames(dct), "dec", sep = ".")
  output.df<-cbind(df,dct)
  output.df
}


slhs<-dec.time.col(slhs)


#--------------

#Making columns that have the time each individual spent in the test temperature

slhs<-slhs %>% mutate(hrs.in.test = (((date.out.test.j - date.in.test.j)*24) + (time.out.test.dec - time.in.test.dec)))


#Making column that has change in mass during time in test temp

slhs<-slhs %>% mutate(delta.mass.test = (mass.out.test - mass.in.test))


#Making column for time from massing at 3rd to time in test

slhs<-slhs %>% mutate(time.bf.test = (time.in.test.dec - time.3.dec))


#Making column for time from oviposition to time in test

slhs<-slhs %>% mutate(time.ovpbf.test = (time.in.test.dec - time.ovp.dec))


#Making column to change in mass from mass at 3rd to mass going in to test
slhs$mass.3<-as.numeric(slhs$mass.3)

slhs<-slhs %>% mutate(delta.mass.bf.test = (mass.in.test - mass.3))

#---------------

#Making mongo sorting column

slhs$mongo<-ifelse(slhs$date.p5.1=="0", 0, 1)

#A few mongos with messy data did not have a 1 added in their binary data--adding now.
  ##sometime should go back and fix in above mess of code

which(slhs$id==101)
slhs[252,37]<-1

which(slhs$id==376)
slhs[293,37]<-1

which(slhs$id==284)
slhs[417,37]<-1


#-----------------

#Removing extraneous columns--date and time columns that are in original format, sorting columns that aren't useful any more

slhs[, c("date.hatch", "date.died", "date.coll", "date.ovp", "time.ovp", "date.3", "time.3", "date.in.test",
         "time.in.test", "date.out.test", "time.out.test", "date.4", "date.5", "date.6", "date.7", 
         "date.died.mongo", "date5.2wks", "date5.3wks", "date.p6.wk1", "date.p6.wk2", "date.p6.wk3", 
         "date.p7.wk1", "date.em", "date.ecl", "date.died.mongo.j", "p5", "p6", "p7", "pwand")] <-list(NULL)


#Fixing typo in date wander for id 426: mass was put into date column, which converted into a date string
#when downloaded from Google drive. Went back to the original google drive sheet, found the error
#and original mass. Searched through google drive version history, and found that the mass was input
#on 9/8/18 (Julian day 251), which makes this the most likely date of wandering

which(slhs$id==426)

slhs[208, 56]<-251
slhs[208, 21]<-5099.05


#removing some individuals with missing data

#getting rid of an individual with incomplete data that slipped through the cleaning
slhs<-subset(slhs, id!=195 & id!=83 & id!=492 &id!=331 & id!=342 & id!=308 & id!=60)


#--------------------

#Calculate dev times

slhs$ttwand<-slhs$date.wand.j - slhs$date.hatch.j
slhs$ttem.w<-slhs$date.em.j - slhs$date.ovp.j
slhs$ttem.h<-slhs$date.em.j - slhs$date.hatch.j
slhs$tt3<-slhs$date.3.j - slhs$date.hatch.j
slhs$tt4<-slhs$date.4.j - slhs$date.hatch.j
slhs$tt5<-slhs$date.5.j - slhs$date.hatch.j
slhs$tt6<-slhs$date.6.j - slhs$date.hatch.j
slhs$tt7<-slhs$date.7.j - slhs$date.hatch.j
slhs$ttcull<-slhs$date.cull.j - slhs$date.hatch.j

slhs$ttp5.1<-slhs$date.p5.1 - slhs$date.hatch.j
slhs$ttp5.2<-slhs$date.p5.2 - slhs$date.hatch.j
slhs$ttp5.3<-slhs$date.p5.3 - slhs$date.hatch.j
slhs$ttp5.4<-slhs$date.p5.4 - slhs$date.hatch.j
slhs$ttp5.5<-slhs$date.p5.5 - slhs$date.hatch.j


#Removing negative ages 
slhs$tt3[is.na(slhs$tt3)]<-0
slhs$tt4[is.na(slhs$tt4)]<-0
slhs$tt5[is.na(slhs$tt5)]<-0
slhs$tt6[is.na(slhs$tt6)]<-0
slhs$tt7[is.na(slhs$tt7)]<-0
slhs$ttem.w[is.na(slhs$ttem.w)]<-0
slhs$ttem.h[is.na(slhs$ttem.h)]<-0
slhs$ttwand[is.na(slhs$ttwand)]<-0
slhs$ttcull[is.na(slhs$ttcull)]<-0
slhs$ttp5.1[is.na(slhs$ttp5.1)]<-0
slhs$ttp5.2[is.na(slhs$ttp5.2)]<-0
slhs$ttp5.3[is.na(slhs$ttp5.3)]<-0
slhs$ttp5.4[is.na(slhs$ttp5.4)]<-0
slhs$ttp5.5[is.na(slhs$ttp5.5)]<-0


slhs$tt3<-ifelse(slhs$tt3 < 0, 0, slhs$tt3)
slhs$tt4<-ifelse(slhs$tt4 < 0, 0, slhs$tt4)
slhs$tt5<-ifelse(slhs$tt5 < 0, 0, slhs$tt5)
slhs$tt6<-ifelse(slhs$tt6 < 0, 0, slhs$tt6)
slhs$tt7<-ifelse(slhs$tt7 < 0, 0, slhs$tt7)
slhs$ttem.w<-ifelse(slhs$ttem.w < 0, 0, slhs$ttem.w)
slhs$ttem.h<-ifelse(slhs$ttem.h < 0, 0, slhs$ttem.h)
slhs$ttwand<-ifelse(slhs$ttwand < 0, 0, slhs$ttwand)
slhs$ttcull<-ifelse(slhs$ttcull < 0, 0, slhs$ttcull)
slhs$ttp5.1<-ifelse(slhs$ttp5.1 < 0, 0, slhs$ttp5.1)
slhs$ttp5.2<-ifelse(slhs$ttp5.2 < 0, 0, slhs$ttp5.2)
slhs$ttp5.3<-ifelse(slhs$ttp5.3 < 0, 0, slhs$ttp5.3)
slhs$ttp5.4<-ifelse(slhs$ttp5.4 < 0, 0, slhs$ttp5.4)
slhs$ttp5.5<-ifelse(slhs$ttp5.5 < 0, 0, slhs$ttp5.5)


slhs$tt3[(slhs$tt3)=="0"]<-NA
slhs$tt4[(slhs$tt4)=="0"]<-NA
slhs$tt5[(slhs$tt5)=="0"]<-NA
slhs$tt6[(slhs$tt6)=="0"]<-NA
slhs$tt7[(slhs$tt7)=="0"]<-NA
slhs$ttem.w[(slhs$ttem.w)=="0"]<-NA
slhs$ttem.h[(slhs$ttem.h)=="0"]<-NA
slhs$ttwand[(slhs$ttwand)=="0"]<-NA
slhs$ttcull[(slhs$ttcull)=="0"]<-NA
slhs$ttp5.1[(slhs$ttp5.1)=="0"]<-NA
slhs$ttp5.2[(slhs$ttp5.2)=="0"]<-NA
slhs$ttp5.3[(slhs$ttp5.3)=="0"]<-NA
slhs$ttp5.4[(slhs$ttp5.4)=="0"]<-NA
slhs$ttp5.5[(slhs$ttp5.5)=="0"]<-NA



#---------------------

#Making 0s in the post 5th measurements turn to NAs
slhs$date.p5.1[slhs$date.p5.1=="0"]<-NA
slhs$date.p5.2[slhs$date.p5.2=="0"]<-NA
slhs$date.p5.3[slhs$date.p5.3=="0"]<-NA
slhs$date.p5.4[slhs$date.p5.4=="0"]<-NA
slhs$date.p5.5[slhs$date.p5.5=="0"]<-NA

slhs$mass.p5.1[slhs$mass.p5.1=="0"]<-NA
slhs$mass.p5.2[slhs$mass.p5.2=="0"]<-NA
slhs$mass.p5.3[slhs$mass.p5.3=="0"]<-NA
slhs$mass.p5.4[slhs$mass.p5.4=="0"]<-NA
slhs$mass.p5.5[slhs$mass.p5.5=="0"]<-NA

slhs$inst.p5.1[slhs$inst.p5.1=="0"]<-NA
slhs$inst.p5.2[slhs$inst.p5.2=="0"]<-NA
slhs$inst.p5.3[slhs$inst.p5.3=="0"]<-NA
slhs$inst.p5.4[slhs$inst.p5.4=="0"]<-NA
slhs$inst.p5.5[slhs$inst.p5.5=="0"]<-NA

#------------------

#Calculating survival in each treatment before removing dead individuals for clean data set

#calculating % host survival for each treatment

surv<-slhs %>% count(treat.para, test.temp)

died<-slhs %>% count(treat.para, test.temp, died.toss)

died<-subset(died,died.toss=="1")
died.n<-died[,4]


surv<-surv %>% mutate(died.n = died$n)
surv<-dplyr::rename(surv, tot.n=n)


surv<-surv %>% mutate(surv.prop = 1-(died.n/tot.n))


write.csv(surv, "field-slhs_treat-surv-table.csv",row.names = FALSE)


#-------------

#creating an "end" category for age and mass, to use for plotting purposes
  ##Plotting with all instar data leads to a mess, since only a fraction have 6+ instars or post 5th
  ##measurements

#set appropriate mass column 0s to NAs so colaesce() will read them as missing values
slhs$mass.wand[slhs$mass.wand==0]<-NA
slhs$mass.cull[slhs$mass.cull==0]<-NA
slhs$mass.48em[slhs$mass.48em==0]<-NA

#coalesce approp. mass columns into an end mass column
slhs$mass.end<-coalesce(slhs$mass.wand, slhs$mass.cull, slhs$mass.48em)

#set appropriate date column 0s to NAs so colaesce() will read them as missing values
slhs$date.wand.j[slhs$date.wand.j==0]<-NA
slhs$date.cull.j[slhs$date.cull.j==0]<-NA
slhs$date.em.j[slhs$date.em.j==0]<-NA

#coalesce approp. date columns into an end date column
slhs$date.end<-coalesce(slhs$date.wand.j, slhs$date.cull.j, slhs$date.em.j)

#calculating dev time from hatching to end
slhs$ttend<-slhs$date.end - slhs$date.hatch.j

#I'm ignoring individuals that died as mongos before weighing--there are only 8 of them,
  ## and it doesn't make sense to put them in the same category as the others


#--------------

#creating long data set of mass and age (keeping np and p in same dataframe, so including mongo
  ## columns even for np individuals)


#long mass data
slhs.mlng<-slhs %>% gather(instar, mass, mass.3, mass.4, mass.5, mass.6, mass.7, mass.wand, mass.48em,
                                mass.end, mass.p5.1, mass.p5.2, mass.p5.3, mass.p5.4, mass.p5.5)


#rename instar column levels
slhs.mlng$instar<-gsub("mass.", "", slhs.mlng$instar)
slhs.mlng$instar<-gsub("48", "", slhs.mlng$instar)


#long age data (ttem will be for hosts (from hatching to emergence))
slhs.alng<-slhs %>% gather(instar, age, tt3, tt4, tt5, tt6, tt7, ttwand, ttem.h, ttend,
                                ttp5.1, ttp5.2, ttp5.3, ttp5.4, ttp5.5)

#rename instar column levels so they match long mass data frame
slhs.alng$instar<-gsub("tt", "", slhs.alng$instar)
slhs.alng$instar<-gsub(".h", "", slhs.alng$instar)

#long data frame for post 5th stages (so they have their "stage"/"class"--died, wander, emerge, etc)
slhs.stage<-slhs %>% gather(instar, stage, mass.3, mass.4, mass.5, mass.6, mass.7, mass.wand, mass.48em,
                                 mass.end, inst.p5.1, inst.p5.2, inst.p5.3, inst.p5.4, inst.p5.5)

#rename columns
slhs.stage$instar<-gsub("mass.", "", slhs.stage$instar)
slhs.stage$instar<-gsub("48", "", slhs.stage$instar)
slhs.stage$instar<-gsub("inst.", "", slhs.stage$instar)


#Make stage = NA for those rows that aren't inst.p5.X (removes masses that I included to make the 
  ## row numbers the same between data frames)

#make stage == NAs 0s so the ifelse will run properly
slhs.stage$stage[is.na(slhs.stage$stage)]<-0

#ifelse statement to replace dummy mass values with 0s, but keep actual stage classifications
slhs.stage$stage<-ifelse(slhs.stage$stage!="5" & slhs.stage$stage!="6" & slhs.stage$stage!="7" & 
               slhs.stage$stage!="cull" & slhs.stage$stage!="died" & slhs.stage$stage!="wand", 
             0, slhs.stage$stage)

#replace stage 0s with NAs
slhs.stage$stage[slhs.stage$stage=="0"]<-NA


#Merging various long datasets together

slhs.mlng<-select(slhs.mlng, id, test.temp, instar, mass)
slhs.alng<-select(slhs.alng, id, test.temp, instar, age)
slhs.lng<-merge(slhs.mlng, slhs.alng, by=c("id", "test.temp", "instar"))
slhs.lng<-merge(slhs.lng, slhs.stage, by=c("id", "test.temp", "instar"))

View(slhs.lng)

#-----------------


#Saving cleaned slhs data (keeping dead individuals for investigating survival metrics)
  ##saving long data frame as well

write.csv(slhs, "Field_SLHS_clean_wdead.csv", row.names=FALSE)
write.csv(slhs.lng, "Field_SLHS_clean_wdead_LONG.csv", row.names=FALSE)




                          