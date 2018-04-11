library(SAScii)
setwd("C:\\Users\\shaohua\\Documents\\TSE II\\dataset\\")

# Input the data
hrs06 <- read.SAScii("H06LB_R.da", "H06LB_R.sas")
hrs10 <- read.SAScii("H10LB_R.da", "H10LB_R.sas")
hrs14 <- read.SAScii("H14LB_R.da", "H14LB_R.sas")
tracker <- read.SAScii("TRK2014TR_R.da", "TRK2014TR_R.sas")

save(hrs06, file = "hrs06.RData")
save(hrs10, file = "hrs10.RData")
save(hrs14, file = "hrs14.RData")
save(tracker, file = "tracker.RData")

load("hrs06.RData")
load("hrs10.RData")
load("hrs14.RData")
load("tracker.RData")

# Include only the variables we want
hrs06_1 <- hrs06[,c("HHID", "PN", "KLB019F", "KLB019G", "KLB019H", "KLB019I", "KLB019J", "KLB019K",
                    "KLB022A", "KLB022A", "KLB022C", "KLB022D", "KLB022E", "KLB023A", "KLB023B",
                    "KLB023C", "KLB023D", "KLB023E",
                    "KLB035A", "KLB035B", "KLB035C", "KLB035D", "KLB035E", "KLB035F", "KLB035G")]
names(hrs06_1) <- c("HHID", "PN", "optN1", "optP1", "optP2", "optP3", "optN2", "optN3",
                    "conN1", "conN2", "conN3", "conN4", "conN5", "conP1", "conP2", "conP3", "conP4",
                    "conP5",
                    "purP1", "purN1", "purP2", "purN2", "purN3", "purN4", "purP3")
hrs06_1 <- hrs06_1[apply(hrs06_1[, 3:25], 1, function(x){!all(is.na(x)) | !any(is.na(x))}),]

hrs10_1 <- hrs10[,c("HHID", "PN", "MLB019F", "MLB019G", "MLB019H", "MLB019I", "MLB019J", "MLB019K",
                      "MLB022A", "MLB022A", "MLB022C", "MLB022D", "MLB022E", "MLB023A", "MLB023B",
                      "MLB023C", "MLB023D", "MLB023E",
                      "MLB035A", "MLB035B", "MLB035C", "MLB035D", "MLB035E", "MLB035F", "MLB035G")]
names(hrs10_1) <- c("HHID", "PN", "optN1", "optP1", "optP2", "optP3", "optN2", "optN3",
                    "conN1", "conN2", "conN3", "conN4", "conN5", "conP1", "conP2", "conP3", "conP4",
                    "conP5",
                    "purP1", "purN1", "purP2", "purN2", "purN3", "purN4", "purP3")
hrs10_1 <- hrs10_1[apply(hrs10_1[, 3:25], 1, function(x){!all(is.na(x)) | !any(is.na(x))}),]

hrs14_1 <- hrs14[,c("HHID", "PN", "OLB018A", "OLB018B", "OLB018C", "OLB018D", "OLB018E", "OLB018F",
                    "OLB021A", "OLB021A", "OLB021C", "OLB021D", "OLB021E", "OLB022A", "OLB022B",
                    "OLB022C", "OLB022D", "OLB022E",
                      "OLB033A", "OLB033B", "OLB033C", "OLB033D", "OLB033E", "OLB033F", "OLB033G")]
names(hrs14_1) <- c("HHID", "PN", "optN1", "optP1", "optP2", "optP3", "optN2", "optN3",
                    "conN1", "conN2", "conN3", "conN4", "conN5", "conP1", "conP2", "conP3", "conP4",
                    "conP5",
                    "purP1", "purN1", "purP2", "purN2", "purN3", "purN4", "purP3")
hrs14_1 <- hrs14_1[apply(hrs14_1[, 3:25], 1, function(x){!all(is.na(x)) | !any(is.na(x))}),]

hrs06_1$wave <- 1
hrs10_1$wave <- 2
hrs14_1$wave <- 3



# Find the intersection of the respondents
resplist <- merge(hrs06_1, hrs10_1, by = c("HHID", "PN"))
resplist <- merge(resplist, hrs14_1, by = c("HHID", "PN"))

hrs06_1 <- merge(hrs06_1, resplist[,c("HHID", "PN")], by = c("HHID", "PN"))
hrs10_1 <- merge(hrs10_1, resplist[,c("HHID", "PN")], by = c("HHID", "PN"))
hrs14_1 <- merge(hrs14_1, resplist[,c("HHID", "PN")], by = c("HHID", "PN"))

mergedata <- rbind(hrs06_1, hrs10_1, hrs14_1)
tracker$educ[tracker$DEGREE == 0 | tracker$DEGREE == 1] <- "LT HS" 
tracker$educ[tracker$DEGREE == 2] <- "HS" 
tracker$educ[tracker$DEGREE > 2] <- "GT HS" 
mergedata <- merge(mergedata, tracker[,c("HHID", "PN", "educ")])

save(mergedata, file = "mergedata.RData")

load("mergedata.RData")

mergedata$opt <- 0
mergedata$con <- 0
mergedata$pur <- 0

# Creating ARS variable
for (i in c("optP1", "optP2", "optP3")){
  for (j in c("optN1", "optN2", "optN3")){
    for (r in seq(1,nrow(mergedata))){
      if (mergedata[r,i] == 5 & mergedata[r,j] == 5 & !is.na(mergedata[r,i]) & !is.na(mergedata[r,j])){
        mergedata[r,"opt"] = mergedata[r,"opt"] + 1
      }
      if (((mergedata[r,i] == 6 & mergedata[r,j] == 5) | (mergedata[r,i] == 5 & mergedata[r,j] == 6))
          & !is.na(mergedata[r,i]) & !is.na(mergedata[r,j])){
        mergedata[r,"opt"] = mergedata[r,"opt"] + 2
      }
      if (mergedata[r,i] == 6 & mergedata[r,j] == 6 & !is.na(mergedata[r,i]) & !is.na(mergedata[r,j])){
        mergedata[r,"opt"] = mergedata[r,"opt"] + 3
      }
    }
  }
}

for (i in c("conN1", "conN2", "conN3", "conN4", "conN5")){
  for (j in c("conP1", "conP2", "conP3", "conP4", "conP5")){
    for (r in seq(1,nrow(mergedata))){
      if (mergedata[r,i] == 5 & mergedata[r,j] == 5 & !is.na(mergedata[r,i]) & !is.na(mergedata[r,j])){
        mergedata[r,"con"] = mergedata[r,"con"] + 1
      }
      if (((mergedata[r,i] == 6 & mergedata[r,j] == 5) | (mergedata[r,i] == 5 & mergedata[r,j] == 6))
          & !is.na(mergedata[r,i]) & !is.na(mergedata[r,j])){
        mergedata[r,"con"] = mergedata[r,"con"] + 2
      }
      if (mergedata[r,i] == 6 & mergedata[r,j] == 6 & !is.na(mergedata[r,i]) & !is.na(mergedata[r,j])){
        mergedata[r,"con"] = mergedata[r,"con"] + 3
      }
    }
  }
}

for (i in c("purP1", "purP2", "purP3")){
  for (j in c("purN1", "purN2", "purN3", "purN4")){
    for (r in seq(1,nrow(mergedata))){
      if (mergedata[r,i] == 5 & mergedata[r,j] == 5 & !is.na(mergedata[r,i]) & !is.na(mergedata[r,j])){
        mergedata[r,"pur"] = mergedata[r,"pur"] + 1
      }
      if (((mergedata[r,i] == 6 & mergedata[r,j] == 5) | (mergedata[r,i] == 5 & mergedata[r,j] == 6))
          & !is.na(mergedata[r,i]) & !is.na(mergedata[r,j])){
        mergedata[r,"pur"] = mergedata[r,"pur"] + 2
      }
      if (mergedata[r,i] == 6 & mergedata[r,j] == 6 & !is.na(mergedata[r,i]) & !is.na(mergedata[r,j])){
        mergedata[r,"pur"] = mergedata[r,"pur"] + 3
      }
    }
  }
}

mergedata$ARS <- mergedata$opt + mergedata$con + mergedata$pur
summary(mergedata$ARS)
sd(mergedata$ARS)

library(reshape)
mergedata2 <- reshape(mergedata, idvar = c("HHID", "PN"), timevar = "wave", direction = "wide")
mergedata2$educ <- mergedata2$educ.1
mergedata2 <- mergedata2[, -c(which(names(mergedata2) == "educ.1"),which(names(mergedata2) == "educ.2"),
                              which(names(mergedata2) == "educ.3"))]

mergedata2$wave1 <- 2006
mergedata2$wave2 <- 2010
mergedata2$wave3 <- 2014


summary(mergedata2$ARS.1)
summary(mergedata2$ARS.2)
summary(mergedata2$ARS.3)

sd(mergedata2$ARS.1)
sd(mergedata2$ARS.2)
sd(mergedata2$ARS.3)



save(mergedata2, file = "mergedata.RData")
library(foreign)
write.foreign(mergedata2, "mergedata.txt", "mergedata.sas",   package="SAS")

apply(mergedata2[,3:83], MARGIN = 2, FUN = function(x){c(mean(x,na.rm = TRUE), sd(x,na.rm =TRUE))})

# Preliminary result
aggregate(mergedata$ARS~mergedata$wave, FUN = "mean")
anova(lm(mergedata$ARS~factor(mergedata$wave)))

aggregate(mergedata$ARS~mergedata$wave + mergedata$educ, FUN = "mean")
anova(lm(mergedata$ARS~factor(mergedata$wave)*factor(mergedata$educ)))

hist(mergedata2$ARS.1)
hist(mergedata2$ARS.2)
hist(mergedata2$ARS.3)

# Spagetti Plot - a 10% sample
library(ggplot2)
dff = mergedata2[sample(nrow(mergedata2), 43, replace = FALSE),]
dff$HHID <- as.numeric(dff$HHID)
dff$PN <- as.numeric(dff$PN)
sammerg = merge(mergedata, dff, by = c("HHID", "PN"))
sammerg$wave[sammerg$wave == 1] <- 2006
sammerg$wave[sammerg$wave == 2] <- 2010
sammerg$wave[sammerg$wave == 3] <- 2014
ggplot(data=sammerg, aes(x=wave, y=ARS, 
                           group=paste0(as.character(HHID),as.character(PN)))) +
  geom_line() + geom_point(size=4, shape=21, fill="white") +    
  ylab("ARS Scores")


# After assigning the class
classdata <- read.csv("class.csv")
classdata$regroup[classdata$GROUP == 1 | classdata$GROUP == 2 | classdata$GROUP == 3] <- 1
classdata$regroup[classdata$GROUP == 4] <- 2
classdata$regroup[classdata$GROUP == 5] <- 3
classdata$regroup[classdata$GROUP == 6] <- 4
mergedata$HHID <- as.numeric(mergedata$HHID)
mergedata$PN <- as.numeric(mergedata$PN)
combinedata <- merge(mergedata, classdata[, c("HHID", "PN", "regroup")], by = c("HHID", "PN"))
combinedata$regroup <- as.factor(combinedata$regroup)

mm <- aggregate(ARS ~ wave + regroup, mean, data = combinedata)
mm$wave[mm$wave == 1] <- 2006
mm$wave[mm$wave == 2] <- 2010
mm$wave[mm$wave == 3] <- 2014

ggplot(mm, aes(x = wave, y = ARS, group = regroup, color = regroup)) + 
  geom_line() +labs(col = "class") 

combined <- merge(mergedata2, tracker[,c("HHID", "PN", "GENDER", "HISPANIC", "RACE", "OAGE", "KIWLANG", "MIWLANG", "OIWLANG")],
                  by = c("HHID", "PN"))
combined$HHID <- as.numeric(combined$HHID)
combined$PN <- as.numeric(combined$PN)
combined <- merge(combined, classdata[, c("HHID", "PN", "regroup")], by = c("HHID", "PN"))
combined$intlang <- apply(combined[,c("KIWLANG", "MIWLANG", "OIWLANG")], MARGIN = 1, 
                          FUN = function(x){ifelse(sum(x == 1)==2, "eng", "spa")})

combined$his<-"NH"
combined$his[combined$HISPANIC==1|combined$HISPANIC==2|combined$HISPANIC==3]<-"HIS"

combined$hisrace<-"NH O"
combined$hisrace[combined$his=="HIS" & combined$intlang=="eng"]<-"HIS in English"
combined$hisrace[combined$his=="HIS" & combined$intlang=="spa"]<-"HIS in Spanish"
combined$hisrace[combined$his!="HIS" & combined$RACE==1]<-"NH W"
combined$hisrace[combined$his!="HIS" & combined$RACE==2]<-"NH B"

table(combined$regroup, combined$GENDER)
prop.table(table(combined$regroup, combined$GENDER))
CrossTable(combined$regroup, combined$GENDER, chisq = TRUE)

table(combined$regroup, combined$educ)
prop.table(table(combined$regroup, combined$educ))
CrossTable(combined$regroup, combined$educ, chisq = TRUE)

table(combined$regroup, combined$hisrace)
prop.table(table(combined$regroup, combined$hisrace))
CrossTable(combined$regroup, combined$hisrace, chisq = TRUE)

aggregate(OAGE ~ regroup, mean, data = combined)

# Multinomial Regression Model
require(nnet)

combined$hisrace <- as.factor(combined$hisrace)
combined$hisrace <- relevel(combined$hisrace, ref = "NH W")
m <- multinom(factor(regroup) ~ factor(GENDER) + hisrace + educ + OAGE, data = combined)
summary(m)
z <- summary(m)$coefficients/summary(m)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
m.rrr = exp(coef(m))
m.rrr
library(stargazer)
stargazer(m, type="text", coef=list(m.rrr), p.auto=FALSE, out="multi1rrr.htm")



# Descriptive analysis
table(combined$GENDER)
prop.table(table(combined$GENDER))

table(combined$hisrace)
prop.table(table(combined$hisrace))

summary(combined$OAGE)
sd(combined$OAGE)
