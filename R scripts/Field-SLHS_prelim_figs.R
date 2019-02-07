#Prelim look at para field sublethal expt:


#load libraries

library(readr)
library(Rmisc)
library(ggplot2)
library(dplyr)
library(tidyr)

#------------

#load data


slhs <- read_csv("data files/Field_SLHS_clean_wdead.csv", 
                 col_types = cols(test.temp = col_factor(levels = c("35", "40", "42", "43", "44")), 
                                  treat.hs = col_factor(levels = c("control", "shock")), 
                                  treat.para = col_factor(levels = c("np", "p"))))
View(slhs)


slhs_lng <- read_csv("data files/Field_SLHS_clean_wdead_LONG.csv", guess_max = 6000,
                      col_types = cols(test.temp = col_factor(levels = c("35", "40", "42", "43", "44")), 
                                       treat.hs = col_factor(levels = c("control", "shock")), 
                                       treat.para = col_factor(levels = c("np", "p"))))
                                                                                                                                                                                  
View(slhs_long)


#--------------------

#create a clean data frame with no dead individuals 
slhs.cl<-subset(slhs, died.toss=="0")

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

ttw.boxplot<-ggplot(slhs.cl, aes(x=test.temp, y=ttwand, group=interaction(test.temp, treat.para),fill=treat.para))
ttw.boxplot+geom_boxplot(
)+scale_fill_manual(values = c("grey", "red"),
                    breaks=c("np", "p"),
                    name="Para treatment",
                    label=c("NP", "P"))



#box plot of mass wand

slhs.cl$mass.wand[slhs.cl$mass.wand=="0"]<-NA

massw.boxplot<-ggplot(slhs.cl, aes(x=test.temp, y=mass.wand, 
                                   group=interaction(test.temp, treat.para),fill=treat.para))
massw.boxplot+geom_boxplot(
)+scale_fill_manual(values = c("grey", "red"),
                    breaks=c("np", "p"),
                    name="Para treatment",
                    label=c("NP", "P"))




#-------------------


#Plotting mass X age for para treatment
  ##for individuals

#removing individual with typo in mass
which(slhs_lng$mass > 60000)
slhs_lng<-slhs_lng[-816,]
slhs_lng<-slhs_lng[-6049,]

#Make 0s in mass column == NAs 
slhs_lng$mass[slhs_lng$mass==0]<-NA

#logging mass
slhs_lng$log.mass<-log(slhs_lng$mass)

#removing dead individuals
slhs_lngc<-subset(slhs_lng, died.toss==0)



#individuals plot with test temp by color and panel by para
indmass.plot<-ggplot(slhs_lngc, aes(x=age, y=log.mass, group=interaction(id, test.temp), color=test.temp))
indmass.plot+geom_line(size=1
)+geom_point(size=3
)+facet_wrap(~treat.para)


#individuals plot with para by color and panel by test temp
indmass.plot2<-ggplot(slhs_lngc, aes(x=age, y=log.mass, group=interaction(id, treat.para), color=treat.para))
indmass.plot2+geom_line(size=1
)+geom_point(size=3
)+facet_wrap(~test.temp)


#Plotting the mean mass at each measurement

#SummarySE of mass

lm.sum<-summarySE(slhs_lngc, measurevar = "log.mass",
                  groupvars = c("test.temp", "treat.para", "instar"),
                  na.rm=TRUE)
lm.sum

#subsetting to only the 3, 4, 5 and end data, to try and make more meaningful plot
lm.sum.neat<-subset(lm.sum, instar=="3" | instar=="4" | instar=="5" | instar=="end")


age.sum<-summarySE(slhs_lngc, measurevar = "age",
                   groupvars = c("test.temp", "treat.para","instar"),
                   na.rm=TRUE)
age.sum

#subsetting to only the 3, 4, 5 and end data, to try and make more meaningful plot
age.sum.neat<-subset(age.sum, instar=="3" | instar=="4" | instar=="5" | instar=="end")


lm.sum.neat$age<-age.sum.neat[,5]
lm.sum.neat$age.se<-age.sum.neat[,7]


#Color by test temp, panel by para
lmsum.plot<-ggplot(lm.sum.neat, aes(x=age, y=log.mass, group=test.temp,
                               color=test.temp))
lmsum.plot+geom_point(aes(shape=instar),
                      size=3
)+geom_line(size=1.2
)+facet_wrap(~treat.para)

#Color by para, panel by test temp
lmsum.plot2<-ggplot(lm.sum.neat, aes(x=age, y=log.mass, group=treat.para,
                                    color=treat.para))
lmsum.plot2+geom_point(size=3
)+geom_line(size=1.2
)+geom_errorbar(aes(ymin=log.mass-se, ymax=log.mass+se),
                width=.4, size=1
)+geom_errorbarh(aes(xmin=age-age.se, xmax=age+age.se),
                 height=.4, size=1
)+facet_wrap(~test.temp)

#--------------------------

#plotting the change in mass during heat shock

#scatterplot
dmass.plot<-ggplot(slhs.cl, aes(x=test.temp, y=delta.mass.test, color=treat.para))
dmass.plot+geom_jitter()

#boxplot
dmass.plot2<-ggplot(slhs.cl, aes(x=test.temp, y=delta.mass.test, fill=treat.para))
dmass.plot2+geom_boxplot(
)+scale_fill_manual(values=c("grey", "orangered3"),
                    breaks=c("np", "p"),
                    label=c("NP", "P"),
                    name="Parasitization")


#plotting with dead individuals included
#boxplot

#making died.toss a factor for plotting purposes
slhs$died.toss<-as.factor(slhs$died.toss)

#load surv data frame to add labels with sample size
surv <- read_csv("data files/field-slhs_treat-surv-table.csv")

dmass.dead.plot<-ggplot(slhs, aes(x=test.temp, y=delta.mass.test, 
                                  group=interaction(test.temp, died.toss),
                                  fill=died.toss))
dmass.dead.plot+geom_boxplot(
)+scale_fill_manual(values=c("grey", "orangered3"),
                    breaks=c("0", "1"),
                    label=c("Lived", "Died"),
                    name="Survival"
)+geom_text(
)+facet_wrap(~treat.para)

