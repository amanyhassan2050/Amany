# MHC-I level experssion in EBLs during EHEC colonisation 
# Experimental conditions (Control, WT, escN, stcE, and each strain supernatent) at MOI (!00)
# Experment was repeated three times with three replicates/ condition
# Individual analysis for each experiment using one way ANOVA and linear model analysis, followed by collective annalysis 
# for the three runs using mixed model analysis

# Analysing first run on 26_July_2019


library("tidyverse")

MOI <- read.csv("M:/my results/Flow Cyometry/MOI 100/MOI_100_Repeats_2019/MOI_100_26_July_2019.csv")


colnames(c("Condition", "MFI"))

boxplot(MFI ~ Condition, data = MOI,
        xlab = "Condition", ylab = "MFI",
        main = "Level of MHC-I expression during EHEC O157:H7 colonisation")
pdf("MOI_output_26_July.pdf")

aov.out = aov(MFI~Condition, data = MOI)
summary(aov.out)
TukeyHSD(aov.out, conf.level = 0.99)

lm.out<-lm(MFI~Condition,data=MOI)

summary(lm.out)

plot(lm.out)

ggplot() +
  geom_histogram(aes(x=lm.out$residuals))

# Analysing second Run on 31_July_2019

MOI_1 <- read.csv("M:/my results/Flow Cyometry/MOI 100/MOI_100_Repeats_2019/MOI_100_31_July_2019.csv")
colnames(c("Condition", "MFI"))

boxplot(MFI ~ Condition, data = MOI_1,
        xlab = "Condition", ylab = "MFI",
        main = "Level of MHC-I expression during EHEC O157:H7 colonisation")
pdf("MOI_output_31_July.pdf")

lm.out<-lm(MFI~Condition,data=MOI_1)

summary(lm.out)

plot(lm.out)

ggplot() +
  geom_histogram(aes(x=lm.out$residuals))

# Analysing third Run on 01_Aug_2019

MOI_II<- read.csv("M:/my results/Flow Cyometry/MOI 100/MOI_100_Repeats_2019/MOI_100_01_Aug_2019.csv")


colnames(c("Condition", "MFI"))

boxplot(MFI ~ Condition, data = MOI_II,
        xlab = "Condition", ylab = "MFI",
        main = "Level of MHC-I expression during EHEC O157:H7 colonisation")
pdf("MOI_output_01_Aug_2019.pdf")


lm.out<-lm(MFI~Condition,data=MOI_II)

summary(lm.out)

plot(lm.out)

ggplot(MOI_II, aes(x=lm.out$residuals)) +
  geom_histogram(bins = 10)

# Applying mixed model analysis for the three runs 

MOI_stats <- read.csv("M:/my results/Flow Cyometry/MOI 100/MOI_100_Repeats_2019/MOI_3_Runs_2019.csv")

colnames(MOI_stats)<-c("Run","Condition","MFI")


Control<-MOI_stats %>%
  group_by(Run,Condition) %>%
  summarise(ControlMFI=mean(MFI)) %>%
  filter(Condition=="Control")

MFI<-MOI_stats %>%
  full_join(Control,by="Run")

MFI<-MFI %>%
  mutate(NormMFI=MFI/ControlMFI) %>%
  select(Run,Condition.x,NormMFI)

results<-ggplot(MFI) +
  geom_boxplot(aes(x=Condition.x,y=NormMFI)) +
  facet_wrap(~Run)

library(nlme)

MFI_model<-lme(NormMFI~Condition.x, data=MFI, random=~1|Run)

summary(MFI_model)

residuals<-as.data.frame(MFI_model$residuals[,1])

colnames(residuals)<-"Resids"

residuals<-ggplot(residuals) +
  geom_histogram(aes(x=Resids))

pdf("MOI_3_Runs.pdf")

results
residuals

dev.off()

