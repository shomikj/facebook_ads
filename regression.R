library(foreign)
library(estimatr)
library(sandwich)
library(lmtest)
library(stargazer)
library(xtable)
library(arm)
library(grDevices)
library("dplyr")
library("car")

x1 <- read.csv(file = "fb_data_shareable.csv", stringsAsFactors = F, header = T)

# Pick output variable
x1$sentiment <- x1$sentiment_vader
#x1$sentiment <- x1$llm_polarity
#x1$sentiment <- x1$llm_stance
#x1$sentiment <- x1$llm_fear

# Create dummies for advertiser types
x1$corp <- as.numeric(x1$adv_type == "og_corp")
x1$irs501 <- as.numeric(x1$adv_type == "og_irs501(c)")
x1$PAC <- as.numeric(x1$adv_type == "og_PAC")
x1$superPAC <- as.numeric(x1$adv_type == "og_superPAC")
x1$hybridPAC <- as.numeric(x1$adv_type == "og_hybridPAC")

x1$outside <- as.numeric(x1$adv_type != "candidate" & x1$adv_type != "party")
x1$outsideNotPAC <- as.numeric(x1$outside == 1 & x1$adv_type != "og_PAC")
x1$party <- as.numeric(x1$adv_type == "party")
x1$candidate <- as.numeric(x1$adv_type == "candidate")

# Create dummies for dark and disappearing
x1$disappear <- as.numeric(x1$adv_disappeared == "True")
x1$dark <- as.numeric(x1$adv_disclosure == "n" | x1$adv_disclosure == "na")
x1$gray <- as.numeric(x1$adv_disclosure == "p")

x1$dark[x1$candidate == 1] <- 0
x1$dark[x1$party == 1] <- 0
x1$gray[x1$candidate == 1] <- 0
x1$gray[x1$party == 1] <- 0
x1$disappear[x1$candidate == 1] <- 0
x1$disappear[x1$party == 1] <- 0

x1$dark_persistant = as.numeric(x1$dark == 1 & x1$disappear == 0)
x1$transparent_disappearing = as.numeric(x1$dark == 0 & x1$disappear == 1)
x1$dark_disappearing = as.numeric(x1$dark == 1 & x1$disappear == 1)


# 1 -- All Advertisers

#LABEL_0, fear
lm1.rob <- lm(sentiment ~ dark_disappearing + dark_persistant + transparent_disappearing, data = x1)
cl_vcov_mat1.rob <- vcovCL(lm1.rob, cluster = ~x1$adv_id)
lm1.rob.coeffs_cl <- coeftest(lm1.rob, vcov = cl_vcov_mat1.rob)
print(lm1.rob.coeffs_cl)

linearHypothesis(lm1.rob, vcov = cl_vcov_mat1.rob, "dark_disappearing = 0")
linearHypothesis(lm1.rob, vcov = cl_vcov_mat1.rob, "dark_persistant = 0")
linearHypothesis(lm1.rob, vcov = cl_vcov_mat1.rob, "transparent_disappearing = 0")

linearHypothesis(lm1.rob, vcov = cl_vcov_mat1.rob, "transparent_disappearing - dark_disappearing = 0")
linearHypothesis(lm1.rob, vcov = cl_vcov_mat1.rob, "transparent_disappearing - dark_persistant = 0")
linearHypothesis(lm1.rob, vcov = cl_vcov_mat1.rob, "dark_persistant - dark_disappearing = 0")


# 2 -- All Advertisers with Fixed Effects 
lm2.rob <- lm(sentiment ~ dark_disappearing + dark_persistant + transparent_disappearing + 
                 + party + PAC + hybridPAC + superPAC + irs501 + corp, data = x1)
cl_vcov_mat2.rob <- vcovCL(lm2.rob, cluster = ~x1$adv_id)
lm2.rob.coeffs_cl <- coeftest(lm2.rob, vcov = cl_vcov_mat2.rob)
print(lm2.rob.coeffs_cl)

linearHypothesis(lm2.rob, vcov = cl_vcov_mat2.rob, "dark_disappearing = 0")
linearHypothesis(lm2.rob, vcov = cl_vcov_mat2.rob, "dark_persistant = 0")
linearHypothesis(lm2.rob, vcov = cl_vcov_mat2.rob, "transparent_disappearing = 0")

linearHypothesis(lm2.rob, vcov = cl_vcov_mat2.rob, "transparent_disappearing - dark_disappearing = 0")
linearHypothesis(lm2.rob, vcov = cl_vcov_mat2.rob, "transparent_disappearing - dark_persistant = 0")
linearHypothesis(lm2.rob, vcov = cl_vcov_mat2.rob, "dark_persistant - dark_disappearing = 0")

# 3 -- Corporations and IRS 501(c) ONLY
x2 <- subset(x1, corp == 1 | irs501 ==1)
lm3.rob <- lm(sentiment ~ dark_persistant + transparent_disappearing + dark_disappearing, data = x2)
cl_vcov_mat3.rob <- vcovCL(lm3.rob, cluster = ~x2$adv_id)
lm3.rob.coeffs_cl <- coeftest(lm3.rob, vcov = cl_vcov_mat3.rob)
print(lm3.rob.coeffs_cl)

linearHypothesis(lm3.rob, vcov = cl_vcov_mat3.rob, "dark_disappearing = 0")
linearHypothesis(lm3.rob, vcov = cl_vcov_mat3.rob, "dark_persistant = 0")
linearHypothesis(lm3.rob, vcov = cl_vcov_mat3.rob, "transparent_disappearing = 0")

linearHypothesis(lm3.rob, vcov = cl_vcov_mat3.rob, "transparent_disappearing - dark_disappearing = 0")
linearHypothesis(lm3.rob, vcov = cl_vcov_mat3.rob, "transparent_disappearing - dark_persistant = 0")
linearHypothesis(lm3.rob, vcov = cl_vcov_mat3.rob, "dark_persistant - dark_disappearing = 0")

######################

# Dark only
lm1a <- lm(sentiment ~ dark, data = x1)
cl_vcov_mat1a <- vcovCL(lm1a, cluster = ~x1$adv_id)
lm1acoeffs_cl <- coeftest(lm1a, vcov = cl_vcov_mat1a)
print(cl_vcov_mat1a)
print(lm1acoeffs_cl)
linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "(Intercept) - dark = 0")


# Disappear only
lm1a <- lm(sentiment ~ disappear, data = x1)
cl_vcov_mat1a <- vcovCL(lm1a, cluster = ~x1$adv_id)
lm1acoeffs_cl <- coeftest(lm1a, vcov = cl_vcov_mat1a)
print(cl_vcov_mat1a)
print(lm1acoeffs_cl)
linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "(Intercept) - disappear = 0")


# By Advertiser Type
lm1a <- lm(sentiment ~ party + outside, data = x1)
cl_vcov_mat1a <- vcovCL(lm1a, cluster = ~x1$adv_id)
lm1acoeffs_cl <- coeftest(lm1a, vcov = cl_vcov_mat1a)
print(lm1acoeffs_cl)

lm1a <- lm(sentiment ~ party + PAC + outsideNotPAC, data = x1)
cl_vcov_mat1a <- vcovCL(lm1a, cluster = ~x1$adv_id)
lm1acoeffs_cl <- coeftest(lm1a, vcov = cl_vcov_mat1a)
print(lm1acoeffs_cl)

# By Outside Group Type
lm1a <- lm(sentiment ~ party + PAC + hybridPAC + superPAC + corp + irs501, data = x1)
cl_vcov_mat1a <- vcovCL(lm1a, cluster = ~x1$adv_id)
lm1acoeffs_cl <- coeftest(lm1a, vcov = cl_vcov_mat1a)
print(lm1acoeffs_cl)

linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "(Intercept) - party = 0")
linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "(Intercept) - PAC = 0")
linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "(Intercept) - hybridPAC = 0")
linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "(Intercept) - superPAC = 0")
linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "(Intercept) - irs501 = 0")
linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "(Intercept) - corp = 0")

linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "party - PAC = 0")
linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "party - hybridPAC = 0")
linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "party - superPAC = 0")
linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "party - irs501 = 0")
linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "party - corp = 0")

linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "PAC - hybridPAC = 0")
linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "PAC - superPAC = 0")
linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "PAC - irs501 = 0")
linearHypothesis(lm1a, test = c("F"), vcov = cl_vcov_mat1a, "PAC - corp = 0")
