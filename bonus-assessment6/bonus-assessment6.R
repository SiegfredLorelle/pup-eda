# Group 2                       BSCOE 1-1             Engineering Data Analysis
# Lim, Shin I.
# Ilao,Kenji C.
# Esguerra,Edgar Jr. P.
# Mina, Siegfred Lorelle C.
# Agcaoili, Leon Adriel Franco M.

# Group Bonus Assessment #6


library(carData)
library(car)


# PROBLEM 1
# Data
before_aspirin <- c(12.3, 12.0, 12.0, 13.0, 13.0, 12.5, 11.3, 11.8, 11.5, 11.0)
after_aspirin  <- c(12.0, 12.3, 12.5, 12.0, 13.0, 12.5, 10.3, 11.3, 11.5, 11.5)

# 1. State null and alternative hypothesis
# Ha: mu_before != mu_after
# Ho: mu_before == mu_after

# 2. Set level of significance
# level of significance = 0.05
# confidence level = 0.95 

# 3. Determine the appropriate test
# Dependent t-test, because before and after aspirin are related and same subjects.

# 4. Determine p-value
# Test normality
shapiro.test(before_aspirin - after_aspirin)
# p-value = 0.2326
# 0.2326 > 0.05, fail to reject Ho, assume normal distribution

# Test hypothesis
t.test(before_aspirin, after_aspirin,
       alternative = "two.sided", paired = T, conf.level = 0.95)
# p-value = 0.4094

# 5. Make a statistical decision
# 0.4094 > 0.05
# fail to reject the null hypothesis

# 6. State conclusion
# There is NO sufficient evidence to conclude that aspirin
# significantly affects the mean time it takes for a clot to form.



# PROBLEM 2
# Data
tackle <- c(323, 295, 305, 308, 309,                                 # offensive
              320, 328, 313, 318, 305,                               # offensive
              289, 250, 305, 310,                                    # defensive
              295, 278, 300, 339)                                    # defensive

type_tackle <- c(replicate(10, "offensive"), replicate(8, "defensive"));type_tackle

df_tackle <- data.frame(tackle, type_tackle)
sub_offensive <- subset(df_tackle, type_tackle == "offensive");sub_offensive
sub_defensive <- subset(df_tackle, type_tackle == "defensive");sub_defensive

# 1. State null and alternative hypothesis
# Ha: mu_offensive > mu_defensive
# Ho: mu_offensive <= mu_defensive

# 2. Set level of significance
# level of significance = 0.05
# confidence level = 0.95 

# 3. Determine the appropriate test
# Independent t-test, because offensive and defensive group is not related.

# 4. Determine p-value
# Test normality of offensive tackle
shapiro.test(sub_offensive$tackle)
# p-value = 0.9455
# 0.9455 > 0.05, fail to reject Ho, assume normal distribution

# Test normality of defensive tackle
shapiro.test(sub_defensive$tackle)
# p-value = 0.9047
# 0.9047 > 0.05, fail to reject Ho, assume normal distribution

# Test Homogeneity of Variances
leveneTest(tackle ~ factor(type_tackle))
# p-value = 0.113
# 0.113 > 0.05, fail to reject Ho, assume equal variances

# Test hypothesis
t.test(sub_offensive$tackle, sub_defensive$tackle, 
       alternative = "greater", var.equal = T, conf.level = 0.95)
# p-value = 0.03852

# 5. Make a statistical decision
# 0.03852 < 0.05
# reject the null hypothesis

# 6. State conclusion
# There is sufficient evidence to conclude that the mean weight of NFL offensive tackle 
# was higher than the mean weight of an NFL defensive tackle.



# PROBLEM 3
# Data
new_system <- c(90.1, 80.6, 67.3, 95.5, 58.1, 86.8, 75.9, 70.2, 65.5, 70.1)

# 1. State null and alternative hypothesis
# Ha: mu < 84.3
# Ho: mu >= 84.3

# 2. Set level of significance
# level of significance = 0.10
# confidence level = 0.90 

# 3. Determine the appropriate test
# One Sample t-test, because only 1 sample & it is compared to a standard mean.

# 4. Determine p-value
# Test normality
shapiro.test(new_system)
# p-value = 0.8065
# 0.8065 > 0.10, fail to reject Ho, assume normal distribution

# Test hypothesis
t.test(new_system, mu = 84.3, alternative = "less", conf.level = 0.90)
# p-value = 0.0282

# 5. Make a statistical decision
# 0.0282 < 0.10
# reject the null hypothesis

# 6. State conclusion
# There is sufficient evidence to conclude that the new drive-through system will
# decrease wait time at the drive through of a fast-food restaurant.



# PROBLEM 4
# Data
anaerobic_thersholds <- c(185, 179, 192, 165, 174,             # distance runner
                            190, 209, 182, 178, 181,           # distance cyclist
                            166, 159, 170, 183, 160,           # distance swimmers
                            201, 195, 180, 187, 215)           # cross-country skiers

group_athlete <- c(replicate(5, "runner"), replicate(5, "cyclist"), 
                     replicate(5, "swimmer"), replicate(5, "skier"));group_athlete

# 1. State null and alternative hypothesis
# Ha: At least one of the  groups of elite athletes has a different 
#     mean anaerobic threshold from the others groups of elite athletes.
# Ho: mu_runners == mu_cyclists == mu_swimmers == mu_skiers

# 2. Set level of significance
# level of significance = 0.05
# confidence level = 0.95 

# 3. Determine the appropriate test
# ANOVA, because comparing more than 2 groups.

# 4. Determine p-value
# Test normality of runners
shapiro.test(c(185, 179, 192, 165, 174))
# p-value = 0.9962
# 0.9962 > 0.05, fail to reject Ho, assume normal distribution

# Test normality of cyclists
shapiro.test(c(190, 209, 182, 178, 181))
# p-value = 0.124
# 0.124 > 0.05, fail to reject Ho, assume normal distribution

# Test normality of swimmers
shapiro.test(c(166, 159, 170, 183, 160))
# p-value = 0.3784
# 0.3784 > 0.05, fail to reject Ho, assume normal distribution

# Test normality of skiers
shapiro.test(c(201, 195, 180, 187, 215))
# p-value = 0.9421
# 0.9421 > 0.05, fail to reject Ho, assume normal distribution

# Test Homogeneity of Variances
leveneTest(anaerobic_thersholds ~ factor(group_athlete))
# p-value = 0.944
# 0.944 > 0.05, fail to reject Ho, assume equal variances

# Test hypothesis
oneway.test(anaerobic_thersholds ~ factor(group_athlete), var.equal = T)
# p-value = 0.009307

# 5. Make a statistical decision
# 0.009307 < 0.05
# reject the null hypothesis

# 6. State conclusion
# There is sufficient evidence to conclude that 
# there is a difference in anaerobic thresholds among different groups of elite athletes. 



# PROBLEM 5
# Data
height             <- c(27.75, 24.50, 25.50, 26.00, 25.00, 27.75, 
                     26.50, 27.00, 26.75, 26.75, 27.50)
head_circumference <- c(17.5,  17.1,  17.1,  17.3,  16.9,  17.6, 
                     17.3,  17.5,  17.3,  17.5,  17.5)

# 1. State null and alternative hypothesis
# Ha: There is significant relationship between height and head circumference.
# Ho: There is no significant relationship between height and head circumference.
 

# 2. Set level of significance
# level of significance = 0.05
# confidence level = 0.95

# 3. Determine the appropriate test
# Pearson R, because relationship.

# 4. Determine p-value
# Test normality of height
shapiro.test(height)
# p-value = 0.4515
# 0.4515 > 0.05, fail to reject Ho, assume normal distribution

# Test normality of head circumference
shapiro.test(head_circumference)
# p-value = 0.1728
# 0.1728 > 0.05, fail to reject Ho, assume normal distribution

# Test hypothesis
cor.test(height, head_circumference, method = "pearson", conf.level = 0.95)
# p-value = 9.59e-05 = 0.0000959
# cor(r) = 0.9110727

# 5. Make a statistical decision
# 0.0000959 < 0.05
# reject the null hypothesis

# 6. State conclusion
# There is sufficient evidence to conclude that 
# there is a significant relationship between child's height and head circumference.
# The relationship is very strong (positive) based on correlation coefficient (0.9110727).


print("Answers are written as comments.")
