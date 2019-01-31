#Prelim look at para field sublethal expt:


#load libraries

library(readr)
library(Rmisc)
library(ggplot2)
library(dplyr)
library(tidyr)

#------------

#load data


slhs <- read_csv("data files/Field_SLHS_clean.csv", 
                 col_types = cols(test.temp = col_factor(levels = c("35", "40", "42", "43", "44")), 
                                  treat.hs = col_factor(levels = c("control", "shock")), 
                                  treat.para = col_factor(levels = c("np", "p"))))
View(slhs)



#---------------------


#Calculate dev times

slhs$ttwand<-slhs$date.wand.j - slhs$date.hatch.j
slhs$ttem<-slhs$date.em.j - slhs$date.ovp.j
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
slhs$ttem[is.na(slhs$ttem)]<-0
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
slhs$ttem<-ifelse(slhs$ttem < 0, 0, slhs$ttem)
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
slhs$ttem[(slhs$ttem)=="0"]<-NA
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

#Creating a dataset without individuals that died (not as mongos)

slhs.cl<-subset(slhs, died.toss=="0")

#-------------

#Creating a long data set of mass and age for np
slhs.np<-subset(slhs.cl, treat.para=="np")
slhs.para<-subset(slhs.cl, treat.para=="p")


#long mass data
slhs.lng<-slhs.np %>% gather(instar, mass, mass.3, mass.4, mass.5, mass.6, mass.7, mass.wand)

slhs.plng<-slhs.para %>% gather(instar, mass, mass.3, mass.4, mass.5, mass.6, mass.7, mass.48em,
                                mass.p5.1, mass.p5.2, mass.p5.3, mass.p5.4, mass.p5.5)

#rename instar column levels
slhs.lng$instar<-gsub("mass.", "", slhs.lng$instar)
slhs.lng$instar<-gsub("48", "", slhs.lng$instar)

slhs.plng$instar<-gsub("mass.", "", slhs.plng$instar)
slhs.plng$instar<-gsub("48", "", slhs.plng$instar)


#long age data

slhs.age<-slhs.np %>% gather(instar, age, tt3, tt4, tt5, tt6, tt7, ttwand)

slhs.page<-slhs.para %>% gather(instar, age, tt3, tt4, tt5, tt6, tt7, ttem, 
                                ttp5.1, ttp5.2, ttp5.3, ttp5.4, ttp5.5)

slhs.age$instar<-gsub("tt", "", slhs.age$instar)
slhs.page$instar<-gsub("tt", "", slhs.page$instar)




#post 5 stage
slhs.stage<-slhs.para %>% gather(instar, stage, mass.3, mass.4, mass.5, mass.6, mass.7, mass.48em,
                                 inst.p5.1, inst.p5.2, inst.p5.3, inst.p5.4, inst.p5.5)


slhs.stage$instar<-gsub("mass.", "", slhs.stage$instar)
slhs.stage$instar<-gsub("48", "", slhs.stage$instar)
slhs.stage$instar<-gsub("inst.", "", slhs.stage$instar)


#Merging various long datasets together

slhs.lng<-select(slhs.lng, id, test.temp, instar, mass)
slhs.lng<-merge(slhs.age, slhs.lng, by=c("id", "test.temp", "instar"))

slhs.page<-select(slhs.page, id, test.temp, instar, age)
slhs.stage<-select(slhs.stage, id, test.temp, instar, stage)
slhs.plng<-merge(slhs.plng, slhs.page, by=c("id", "test.temp", "instar"))
slhs.plng<-merge(slhs.plng, slhs.stage, by=c("id", "test.temp", "instar"))

#sort out the masses form the stage column

slhs.plng$stage[is.na(slhs.plng$stage)]<-0

slhs.plng$stage<-ifelse(slhs.plng$stage == "5" | slhs.plng$stage == "6" |
                          slhs.plng$stage == "7" | slhs.plng$stage == "died" |
                          slhs.plng$stage == "cull" | slhs.plng$stage == "wand",
                      slhs.plng$stage, 0)

#------------------

#Quick look at dist of time to wandering amongst treatments; and prop that had emergence at each treat

ttw.sum<-summarySE(slhs.cl, measurevar = "ttwand",
                   groupvars = c("test.temp", "treat.para"),
                   na.rm = TRUE)
ttw.sum



#plot of ttwand and mass.wand, group by treat.para

ttw.plot<-ggplot(slhs.cl, aes(x=ttwand, y=mass.wand, group=treat.para, color=treat.para))
ttw.plot+geom_point(
       )+geom_smooth(method="lm", se=FALSE
       )+facet_wrap(~test.temp)



#plot of ttwand and mass.wand, group by test.temp

ttw.plot2<-ggplot(slhs.cl, aes(x=ttwand, y=mass.wand, group=test.temp, color=test.temp))
ttw.plot2+geom_point(
)+geom_smooth(method="lm", se=FALSE
)+facet_wrap(~treat.para)



#box plot of ttwand

ttw.boxplot<-ggplot(slhs.cl, aes(x=test.temp, y=ttwand, group=interaction(test.temp, treat.para),color=treat.para))
ttw.boxplot+geom_boxplot()



#-------------------

#Plotting mass X age for para treatment
  ##for individuals

#removing individual with typo in mass

slhs.plng<-subset(slhs.plng, mass < 60000)

#logging mass

slhs.plng$log.mass<-log(slhs.plng$mass)

indmass.plot<-ggplot(slhs.plng, aes(x=age, y=log.mass, group=interaction(id, test.temp), color=test.temp))
indmass.plot+geom_line(size=1
)+geom_point(aes(shape=stage), size=3
)+facet_wrap(~mongo)


#Plotting the mean mass at each measurement

#SummarySE of mass

lm.sum<-summarySE(slhs.plng, measurevar = "log.mass",
                  groupvars = c("test.temp", "instar"),
                  na.rm=TRUE)
lm.sum


age.sum<-summarySE(slhs.plng, measurevar = "age",
                   groupvars = c("test.temp", "instar"),
                   na.rm=TRUE)
age.sum

lm.sum$age<-age.sum[,4]
lm.sum$age.se<-age.sum[,6]


lmsum.plot<-ggplot(lm.sum, aes(x=age, y=log.mass, group=test.temp, color=test.temp))
lmsum.plot+geom_point(
)+geom_line()



