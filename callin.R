library(shiny)
library(metafor)
library(xtable)
library(ggplot2)
library(bayesmeta)
library(shinysky)
library(DT)
library(shinythemes)

# read study data from CSV file
#metainput <- read.csv(file="meta-update_16aug2019.csv", header=TRUE, sep=",")
metainput <- read.csv(file="meta_update-20200206.csv", header=TRUE, sep=",")

# make 'author' a character variable even though it should already be a character variable
metainput$author_year <- as.character(metainput$author_year)

# create an HTML link to PubMed for each paper, using PMID
metainput$url <- paste0("<a href='https://www.ncbi.nlm.nih.gov/pubmed/?term=",metainput$pmid,"'>",metainput$pmid,"</a>")

# calculate log relative risk
metainput$logrr <- log(metainput$rrpoint)

# calculate standard errors and variances from width of 95% confidence intervals
metainput$se_deriv <- (log(metainput$ucl95)-log(metainput$lcl95))/3.92
metainput$var_deriv <- metainput$se_deriv^2

# calculate log confidence limits based on derived standard error
metainput$lnlcl95 <- log(metainput$rrpoint)-(1.96*metainput$se_deriv)
metainput$lnucl95 <- log(metainput$rrpoint)+(1.96*metainput$se_deriv)

## define vectors of study author, year based on specific groups of interest

# 1) studies with biologically plausible associations (RR<=2)
plausibles <- metainput[metainput$rrpoint <= 2,1]

# 2) studies of Caucasians genotyping the *4 variant
metainput$starfour <- grepl("\\*4|AmpliChip",metainput$variants)
starfour <- as.character(metainput[(metainput$starfour==TRUE & metainput$caucasian==1),1])

# 3) studies of Asians genotyping the *10 variant
metainput$starten <- grepl("\\*10|AmpliChip",metainput$variants)
starten <- as.character(metainput[(metainput$starten==TRUE & metainput$asian==1),1])

# 4) studies genotyping from tumor
tumordna <- metainput[metainput$dnasource=="Tumor",1]

# 5) studies genotyping from non-neoplastic tissue
normaldna <- metainput[metainput$dnasource=="Non-neoplastic",1]