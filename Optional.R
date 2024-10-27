install.packages("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)
library(matahari)
dance_start(value = FALSE, contents = FALSE)
head(college,5)
str(college)
college$major <- as.factor(college$major)
college$major_code <- as.factor(college$major_code)
college$major_category <- as.factor(college$major_category)

boxplot(median/1000 ~ major_category, data = college, main = "Income vs. Major", ylab="Income (thousands of dollar)", las = 2)
unique(college$major_category)
college <- college[order(college$major_category),]

major_category_ref <- relevel(college$major_category, "Arts")
fit <- lm(median ~ major_category_ref, data = college)
summary(fit)$coef
A <- matrix(, nrow = 16, ncol = 16)

for (i in 1:16){
  major_category_ref <- relevel(college$major_category, as.character(unique(college$major_category)[i]))
  fit <- lm(median ~ major_category_ref, data = college)
  tmp <- summary(fit)$coef[,4]
  # swap the first element to the corresponding position in the diagonal matrix
  tmp1 <- tmp[1:i]
  tmp1 <- c(0,tmp1)
  tmp1 <- c(tmp1[-2],tmp1[2])
  tmp1 <- tmp1[-1]
  # save to A
  library(reshape)
  
  library(ggplot2)
  B <- data.frame(A)
  names(B) <- unique(college$major_category)
  B$major <- unique(college$major_category)
  Bmelt <- melt(B)
  g = ggplot(data=Bmelt, aes(x=variable, y=major, fill=value))
  g = g + geom_tile()
  g = g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("Major") + xlab("Major")
  g = g + ggtitle("Probability of difference in Income between Majors")
  g = g + coord_fixed()
  g
  g = ggplot(data=Bmelt, aes(x=variable, y=major, fill=value < 0.025))
  g = g + geom_tile()
  g = g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("Major") + xlab("Major")
  g = g + ggtitle("Difference in Income between Majors")
  g = g + coord_fixed()
  g
  major_category_ref <- relevel(college$major_category, "Business")
  fit <- lm(median ~ major_category_ref, data = college)
  summary(fit)$coef
  business_diff <- summary(fit)$coef[-1,]
  business_diff[order(business_diff[,4])[1:5],]
  A[,i] <- c(tmp1,tmp[-(1:i)])
}
dance_save("~/Desktop/college_major_analysis.rds")