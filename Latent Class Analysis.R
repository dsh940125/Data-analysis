########################### Clean the data ##############################
# Input the HRS Tracker file
setwd("/Users/dongshaohua/Documents/Michigan-Survey/Advanced statistical Modeling/SEM")
library(SAScii)
hrstracker <- read.SAScii("TRK2014TR_R.da", "TRK2014TR_R.sas")

# Subset the target sample: the re-contacted respondents in 2004 wave who completes the survey and older than 50
# JAGE: Age
# KINSAMP: Sample status in 2004 (included the status of deceased respondents and dropouts)
# KIWWAVE: Whether or not the sample member provided any type of interview
# JRESCODE: Final diposition of the case
# FIRSTIW: The year of the first interview of the respondent
samp <- subset(hrstracker[, c("HHID", "PN", "JAGE", "KINSAMP", "KIWWAVE", "JRESCODE", "FIRSTIW")], 
               JAGE >= 50 & JRESCODE == 1001 & FIRSTIW < "2004")

# Create the attrition variable for the re-contacted respondents in 2004 wave 
# Attrition is defined as the dropouts or nonrespondents in 2006 wave, which consists of the following categories:
# KIWWAVE == 0: sampled but not responded to the survey in 2004 wave
# KINSAMP == 4: members of HRS-AHEAD overlap households in which no one was interviewed at AHEAD wave 1, and who were dropped from the sample at subsequent wave
# KINSAMP == 6: permanently dropped from the sample per request of the sample person, his/her spouse/partner, or other gatekeeper
# KINSAMP == 8: permanently dropped from the sample for any other reason
samp$attrition <- 0
samp$attrition[samp$KIWWAVE == 0 | (samp$KINSAMP == 4 | samp$KINSAMP == 6 | samp$KINSAMP ==8)] <- 1
table(samp$attrition) # See the attrition distribution

# Input the Section C (Physical Health) of the 2004 HRS Core dataset 
hrs2004 <- read.SAScii("H04C_R.da", "H04C_R.sas")

# Subset the physical health dataset. The subsetted dataset contains the variables of household ID (HHID), person ID (PN), hypertension (JC005), diabetes (JC010), cardiovasular disease (JC036), arthritis (JC070), and urinary incontinence (JC087)
hrs2004sub <- hrs2004[, c("HHID", "PN", "JC005", "JC010", "JC036", "JC070", "JC087")]

# Merge the tracker file subsetted dataset with 2004 Core physical health subsetted dataset by household ID and person ID
samptot <- merge(samp, hrs2004sub, by = c("HHID", "PN"))

### Recode the five health disease variables into binary ones. Originally, the categories were: 1) Yes, 3) Disputes previous wave record, but now has condition, 4) Disputes previous wave record, does not have condition, 5) No, 8) DK, and 9) RF. The category 1 and 3 are recoded as 1 (Yes, has the condition), and category 4 and 5 are recoded as 0 (No, does not have the condition).

# Recode hypertension variable into binary variable
samptot$highblood[samptot$JC005 == 1 | samptot$JC005 == 3] <-1
samptot$highblood[samptot$JC005 == 4 | samptot$JC005 == 5] <-0

# Recode diabetes variable into binary variable
samptot$diabetes[samptot$JC010 == 1 | samptot$JC010 == 3] <-1
samptot$diabetes[samptot$JC010 == 4 | samptot$JC010 == 5] <-0

# Recode cardiovasular disease variable into binary variable
samptot$heart[samptot$JC036 == 1 | samptot$JC036 == 3] <-1
samptot$heart[samptot$JC036 == 4 | samptot$JC036 == 5] <-0

# Recode arthritis variable into binary variable
samptot$arthritic[samptot$JC070 == 1 | samptot$JC070 == 3] <-1
samptot$arthritic[samptot$JC070 == 4 | samptot$JC070 == 5] <-0

# Recode urinary incontinence variable into binary variable
samptot$incontinence[samptot$JC087 == 1] <-1
samptot$incontinence[samptot$JC087 == 5] <-0

# The study assumes that all the missing data is missing completely at random (MCAR). Thus, this study employs complete case analysis.
# Delete all the cases with missing data
dim(samptot) # 16364 rows
samptotcom <- samptot[!(samptot$JAGE == 8 | samptot$JAGE == 9 | is.na(samptot$JAGE))
                      & !(is.na(samptot$highblood)) & !(is.na(samptot$diabetes == 8)) 
                      & !(is.na(samptot$heart)) & !(is.na(samptot$arthritic))
                      & !(is.na(samptot$incontinence)),]
dim(samptotcom) # 16258 rows 
# Deleted 106 cases, accounting for 0.6478% out of the total sample size.

####################### Descriptive analysis #########################
library(dplyr)
library(purrr)
# Age
samptotcom %>%
  summarise(mean = mean(JAGE), sd = sd(JAGE), min = min(JAGE), max = max(JAGE))
# Attrition
samptotcom %>%
  summarise(mean = mean(attrition), sd = sd(attrition), min = min(attrition), max = max(attrition)) 
# Hypertension
samptotcom %>%
  summarise(mean = mean(highblood), sd = sd(highblood), min = min(highblood), max = max(highblood)) 
# Diabetes
samptotcom %>%
  summarise(mean = mean(diabetes), sd = sd(diabetes), min = min(diabetes), max = max(diabetes)) 
# Cardiovasular disease
samptotcom %>%
  summarise(mean = mean(heart), sd = sd(heart), min = min(heart), max = max(heart)) 
# Arthritis
samptotcom %>%
  summarise(mean = mean(arthritic), sd = sd(arthritic), min = min(arthritic), max = max(arthritic)) 
# Urinary incontinence
samptotcom %>%
  summarise(mean = mean(incontinence), sd = sd(incontinence), min = min(incontinence), max = max(incontinence)) 


############################ Do the LCA model ##############################
### Use the five binary variables (hypertension, diabetes, cardiovasular disease, arthritis, and urinary incontinence) to do the latent class analysis to estimate a latent health status for each respondent

library(poLCA)
library(ggplot2)

# Set random seed to the number 12345
set.seed(12345)

# Make a two class model
fit2 = poLCA(cbind(highblood = highblood + 1, diabetes = diabetes + 1, heart = heart + 1, 
                   arthritic = arthritic + 1, incontinence = incontinence + 1) ~ 1, maxiter=50000, nclass=2, 
             nrep=10, data=samptotcom)

# Make a three class model
fit3 = poLCA(cbind(highblood = highblood + 1, diabetes = diabetes + 1, heart = heart + 1, 
                   arthritic = arthritic + 1, incontinence = incontinence + 1) ~ 1, maxiter=50000, nclass=3, 
             nrep=10, data=samptotcom)
# Make a four class model
fit4 = poLCA(cbind(highblood = highblood + 1, diabetes = diabetes + 1, heart = heart + 1, 
                   arthritic = arthritic + 1, incontinence = incontinence + 1) ~ 1, maxiter=50000, nclass=4, 
             nrep=10, data=samptotcom)

# Make a five class model
fit5 = poLCA(cbind(highblood = highblood + 1, diabetes = diabetes + 1, heart = heart + 1, 
                   arthritic = arthritic + 1, incontinence = incontinence + 1) ~ 1, maxiter=50000, nclass=5, 
             nrep=10, data=samptotcom)

###### Calculate p value for likelihood ratio test and Pearson's chi square test of model fit ########################

# P-value for likelihood ratio test in two class model
1-pchisq(316.7397, 20) # <0.001
# P-value for Pearson's Chi-square test in two class model
1-pchisq(326.4889, 20) # <0.001

# P-value for likelihood ratio test in three class model
1-pchisq(102.5265, 14) # <0.001
# P-value for Pearson's Chi-square test in three class model
1-pchisq(102.6632, 14) # <0.001

# P-value for likelihood ratio test in two class model
1-pchisq(316.7397, 20) # <0.001
# P-value for Pearson's Chi-square test in two class model
1-pchisq(326.4889, 20) # <0.001

# P-value for likelihood ratio test in four class model
1-pchisq(9.826884, 8) # 0.2774
# P-value for Pearson's Chi-square test in four class model
1-pchisq(10.40525, 8) # 0.2377

# P-value for likelihood ratio test in five class model
1-pchisq(1.844472, 2) # 0.3976
# P-value for Pearson's Chi-square test in five class model
1-pchisq(1.930902, 2) # 0.3808

######################### Entropy Calculation for four and five class model####################
# Entropy calculation function
entropy<-function (p) sum(-p*log(p))

### Calculate the entropy for two-class model
error_prior<-entropy(fit2$P) 
error_post<-mean(apply(fit2$posterior,1, entropy),na.rm = TRUE)
entropy2 <- round(((error_prior-error_post) / error_prior),4)
entropy2 # 0.369

### Calculate the entropy for three-class model
error_prior<-entropy(fit3$P) 
error_post<-mean(apply(fit3$posterior,1, entropy),na.rm = TRUE)
entropy3 <- round(((error_prior-error_post) / error_prior),4)
entropy3 # 0.3989

### Calculate the entropy for four-class model
error_prior<-entropy(fit4$P) 
error_post<-mean(apply(fit4$posterior,1, entropy),na.rm = TRUE)
entropy4 <- round(((error_prior-error_post) / error_prior),4)
entropy4 # 0.3248

### Calculate the entropy for five-class model
error_prior<-entropy(fit5$P)
error_post<-mean(apply(fit5$posterior,1, entropy),na.rm = TRUE)
entropy5 <- round(((error_prior-error_post) / error_prior),4)
entropy5 # 0.3518

### According to the AIC, BIC, likelihood ratio test and Pearson's Chi-square test  , the four class model is the best fit for our study. Thus, in the paper, we'll just show the results of four and five class model.

############################## Make the solution plot ###########################

### The solution plot for four class model
# Create a data frame of the probability of being diagnostic of the five diseases in each of the four classes
ff4 <- data.frame(c(rep("Hypertension", 4), rep("Diabetes", 4), rep("Cardiovasular disease",4), rep("Arthritis",4), 
                    rep("Urinary incontinence", 4)),
                  c(fit4$probs$highblood[1,2], fit4$probs$highblood[2,2], fit4$probs$highblood[3,2], fit4$probs$highblood[4,2], 
                    fit4$probs$diabetes[1,2], fit4$probs$diabetes[2,2], fit4$probs$diabetes[3,2], fit4$probs$diabetes[4,2],
                    fit4$probs$heart[1,2], fit4$probs$heart[2,2], fit4$probs$heart[3,2], fit4$probs$heart[4,2],
                    fit4$probs$arthritic[1,2], fit4$probs$arthritic[2,2], fit4$probs$arthritic[3,2], fit4$probs$arthritic[4,2], 
                    fit4$probs$incontinence[1,2], fit4$probs$incontinence[2,2], fit4$probs$incontinence[3,2], fit4$probs$incontinence[4,2]), 
                  c(rep(c("class1", "class2","class3", "class4"), 5)))
# Create the column name of the data frame
names(ff4) <- c("variable", "probability", "class")
# Plot the solution plot based on the data frame
ggplot(ff4, aes(x = variable, y = probability, group = class, lty = class, color = class)) + 
  geom_line() + geom_point()


### The solution plot for five class model
# Create a data frame of the probability of being diagnostic of the five diseases in each of the five classes
ff5 <- data.frame(c(rep("Hypertension", 5), rep("Diabetes", 5), rep("Cardiovasular disease",5), rep("Arthritis",5), 
                    rep("Urinary incontinence", 5)),
                  c(fit5$probs$highblood[1,2], fit5$probs$highblood[2,2], fit5$probs$highblood[3,2], fit5$probs$highblood[4,2], fit5$probs$highblood[5,2],
                    fit5$probs$diabetes[1,2], fit5$probs$diabetes[2,2], fit5$probs$diabetes[3,2], fit5$probs$diabetes[4,2],fit5$probs$diabetes[5,2],
                    fit5$probs$heart[1,2], fit5$probs$heart[2,2], fit5$probs$heart[3,2], fit5$probs$heart[4,2],fit5$probs$heart[5,2],
                    fit5$probs$arthritic[1,2], fit5$probs$arthritic[2,2], fit5$probs$arthritic[3,2], fit5$probs$arthritic[4,2], fit5$probs$arthritic[5,2], 
                    fit5$probs$incontinence[1,2], fit5$probs$incontinence[2,2], fit5$probs$incontinence[3,2], fit5$probs$incontinence[4,2], fit5$probs$incontinence[5,2]), 
                  c(rep(c("class1", "class2","class3", "class4", "class5"), 5)))
# Create the column name of the data frame
names(ff5) <- c("variable", "probability", "class")
# Plot the solution plot based on the data frame
ggplot(ff5, aes(x = variable, y = probability, group = class, lty = class, color = class)) + 
  geom_line() + geom_point()


####### Average Latent Class Probabilities for Most Likely Membership by Latent Class ####################

# Generate the variables for the posterior membership probabilities for each latent class of four class model
samptotcom$posterior41 <- fit4$posterior[,1]
samptotcom$posterior42 <- fit4$posterior[,2]
samptotcom$posterior43 <- fit4$posterior[,3]
samptotcom$posterior44 <- fit4$posterior[,4]

# Make two way table for average class membership probability across latent class of four class model
prob4 <- data.frame(aggregate(samptotcom$posterior41 ~ samptotcom$class, FUN = mean)[,2],
           aggregate(samptotcom$posterior42 ~ samptotcom$class, FUN = mean)[,2],
           aggregate(samptotcom$posterior43 ~ samptotcom$class, FUN = mean)[,2],
           aggregate(samptotcom$posterior44 ~ samptotcom$class, FUN = mean)[,2])
names(prob4) <- c('1', '2', '3', '4')
prob4

# Generate the variables for the posterior membership probabilities for each latent class
samptotcom$posterior51 <- fit5$posterior[,1]
samptotcom$posterior52 <- fit5$posterior[,2]
samptotcom$posterior53 <- fit5$posterior[,3]
samptotcom$posterior54 <- fit5$posterior[,4]
samptotcom$posterior55 <- fit5$posterior[,5]

# Make two way table for average class membership probability across latent class
prob5 <- data.frame(aggregate(samptotcom$posterior51 ~ samptotcom$class, FUN = mean)[,2],
                    aggregate(samptotcom$posterior52 ~ samptotcom$class, FUN = mean)[,2],
                    aggregate(samptotcom$posterior53 ~ samptotcom$class, FUN = mean)[,2],
                    aggregate(samptotcom$posterior54 ~ samptotcom$class, FUN = mean)[,2],
                    aggregate(samptotcom$posterior55 ~ samptotcom$class, FUN = mean)[,2])
names(prob5) <- c('1', '2', '3', '4', '5')
prob5

##################### Logistic regression model - use four class model result #################################
# Do the logistic regression model to see if we can use latent health status to predict pane; attrition. Use age as the control variable in the model

# Create a new variable for the estimated latent class of health status in the four class model (which is the best fit for our study)
samptotcom$class <- fit4$predclass

# Regressed the latent health status on panel attrition, controlling for age
lm <- glm(attrition ~ factor(class) + JAGE, family = binomial(link = "logit"), data = samptotcom)
# See details of logistic models
summary(lm)
