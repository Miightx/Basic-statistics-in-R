# Clearing data
rm(list=ls(all=TRUE))

# Load data
dataset <- read.table(file="~/Projet_humanstats/Blood_test_bis.csv", sep=";", dec=",", header=TRUE, skip=1)
# Display column names
names(dataset)


# Set seed for reproducibility
set.seed(222)

# Sample 10 rows from the dataset (store the sampled data in a new variable)
sample_data <- dplyr::sample_n(dataset, 10)

#Categorized the dataset
BS <- Blood_test_bis$BS
BP <- Blood_test_bis$BP
Ins <- Blood_test_bis$Ins_dose
Gln <- Blood_test_bis$Gln_dose

# MANOVA on blood dataset
res.man <- manova(cbind(BS,BP) ~ (Ins*Gln ), data = dataset)

# Obtain MANOVA summary
summary.manova(res.man)

#Pearson correlation
cor.test(BS,BP, alternative="greater")


#ANOVA on blood dataset
res.anova <- aov(BS ~ Ins*Gln, data = dataset)
summary(res.anova)


