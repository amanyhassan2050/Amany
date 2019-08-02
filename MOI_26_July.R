
library("tidyverse")

MOI <- read.csv("M:/my results/Flow Cyometry/MOI 100/MOI_1/MOI_26_July.csv")

colnames(c("Condition", "MFI"))

boxplot(MFI ~ Condition, data = MOI,
        xlab = "Condition", ylab = "MFI",
        main = "Level of MHC-I expression during EHEC O157:H7 colonisation")
pdf("MOI_output.pdf")

aov.out = aov(MFI~Condition, data = MOI)
summary(aov.out)
TukeyHSD(aov.out, conf.level = 0.99)

lm.out<-lm(MFI~Condition,data=MOI)

summary(lm.out)

plot(lm.out)

ggplot() +
  geom_histogram(aes(x=lm.out$residuals))
