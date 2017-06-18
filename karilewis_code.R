### JESSICA MILLER ###
### KARI LEWIS PROJECT ###
### JUNE 16, 2017 ###

setwd("C:/Users/Elena/Desktop/Github")

data <- read.csv("karilewis_data.csv", header = T, stringsAsFactors = F)

###PAIRED T-TESTS
#GPA
gpa.ttest <- t.test(data$GPA.Fall.2016, data$GPA.Spring.2017, paired=T)
#Self esteem
selfesteem.ttest <- t.test(data$pre.self.esteem, data$post.self.esteem, paired=T)
#Step test
step.ttest <- t.test(data$pre.steptest, data$post.steptest, paired=T)
#ANX
anx.ttest <- t.test(data$pre.ANX, data$post.ANX, paired=T)
#ATT
att.ttest <- t.test(data$pre.ATT, data$post.ATT, paired=T)
#INP
inp.ttest <- t.test(data$pre.INP, data$post.INP, paired=T)
#MOT
mot.ttest <- t.test(data$pre.MOT, data$post.MOT, paired=T)
#SMI
smi.ttest <- t.test(data$pre.SMI, data$post.SMI, paired=T)
#SFT
sft.ttest <- t.test(data$pre.SFT, data$post.SFT, paired=T)
#TST
tst.ttest <- t.test(data$pre.TST, data$post.TST, paired=T)
#TMT
tmt.ttest <- t.test(data$pre.TMT, data$post.TMT, paired=T)
#UAR
uar.ttest <- t.test(data$pre.UAR, data$post.UAR, paired=T)

###CORRELATION BETWEEN STEP TEST AND SELF ESTEEM
attach(data)
hist(pre.self.esteem)
hist(post.self.esteem)
hist(pre.steptest)
hist(post.steptest)
pre.corr.p <- cor(pre.self.esteem, pre.steptest, use = "complete.obs", method="pearson")
post.corr.p <- cor(post.self.esteem, post.steptest, use = "complete.obs", method="pearson")
pre.corr.s <- cor(pre.self.esteem, pre.steptest, use = "complete.obs", method="spearman")
post.corr.s <- cor(post.self.esteem, post.steptest, use = "complete.obs", method="spearman")


###