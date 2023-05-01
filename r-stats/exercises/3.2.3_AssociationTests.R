# This dataframe reflects the allele status (either AA/Aa or aa) and the case/control status for 72 individuals.
d = read.csv("assoctest.csv")
head(d)

# Q1: Compute the Chi-square test for the association of genotype with case/control status 
# (using the table() function and the chisq.test() function). Examine the table to see if it looks enriched for association by eye.
# What is the X-squared statistic?
allele_table <- table(d)
chisq.test(allele_table) # X-squared = 3.3437, df = 1, p-value = 0.06746
## 3.3437


# Q2: Compute the Fisher's exact test ( fisher.test() ) for the same table. What is the p-value?
fisher.test(allele_table) # p-value = 0.05194, alternative hypothesis: true odds ratio is not equal to 1, 95 percent confidence interval: 0.940442 8.493001, sample estimates: , odds ratio 2.758532 
## 0.05194