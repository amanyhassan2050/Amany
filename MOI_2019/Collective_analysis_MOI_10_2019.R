# MHC-I level experssion in EBLs during EHEC colonisation 
# Experimental conditions (Control, WT, escN, stcE, and each strain supernatent, at MOI (10)
# Experment was repeated three times with three replicates/ condition
# Individual analysis for each experiment using one way ANOVA and linear model analysis, followed by collective annalysis 
# for the three runs using mixed model analysis

# Analysing first run on 31_July_2019

library("tidyverse")

MOI_10_I <- read.csv("M:/my results/Flow Cyometry/MOI 100/MOI_10_31_July_2019/31_July_2019.csv")

colnames(c("Condition", "MFI"))

boxplot(MFI ~ Condition, data = MOI_10_I,
        xlab = "Condition", ylab = "MFI",
        main = "Level of MHC-I expression during EHEC O157:H7 colonisation")

pdf("MOI_100_output_31_July_2019.pdf")


lm.out<-lm(MFI~Condition,data=MOI_10_I)

summary(lm.out)

plot(lm.out)

ggplot(MOI_10_I, aes(x=lm.out$residuals)) +
  geom_histogram(bins = 30)

# Analysing secondrun on 01_Aug_2019

MOI_10_II <- read.csv("M:/my results/Flow Cyometry/MOI 100/MOI_10_01_Aug_2019/MOI_10_01_Aug_2019.csv")

colnames(c("Condition", "MFI"))

boxplot(MFI ~ Condition, data = MOI_10_II,
        xlab = "Condition", ylab = "MFI",
        main = "Level of MHC-I expression during EHEC O157:H7 colonisation")

pdf("MOI_100_output_01_Aug_2019.pdf")


lm.out<-lm(MFI~Condition,data=MOI_10_II)

summary(lm.out)

plot(lm.out)

ggplot(MOI_10_II, aes(x=lm.out$residuals)) +
  geom_histogram(bins = 30)

# Applying mixed model analysis for the two runs
MOI_10_stats <- read.csv("M:/my results/Flow Cyometry/MOI 100/MOI_10_Repeats_2019/MOI_10_Repeats_2019.csv")

colnames(MOI_10_stats)<-c("Run","Condition","MFI")


Control<-MOI_10_stats %>%
  group_by(Run,Condition) %>%
  summarise(ControlMFI=mean(MFI)) %>%
  filter(Condition=="Control")

MFI<-MOI_10_stats %>%
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

pdf("MOI_10_2_Runs.pdf")

results
residuals

dev.off()
