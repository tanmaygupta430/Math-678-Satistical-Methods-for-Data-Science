install.packages("ISLR")
rownames (College )=College [,1]
fix (College)
summary(College)
pairs(College[1:10])
plot(college$Outstate,college$Private)
plot(College$Private, College$Outstate, xlab = "Private University", ylab ="Out of State tuition in USD", main = "Outstate Tuition Plot")
Elite =rep ("No",nrow(College ))
Elite [College$Top10perc >50]=" Yes"
Elite =as.factor (Elite)
College$Elite <- Elite
summary(College$Elite)
plot(College$Elite, College$Outstate, xlab = "Elite University", ylab ="Out of State tuition in USD", main = "Outstate Tuition Plot")
par(mfrow = c(2,2))
hist(College$Books, col = 3, xlab = "Books", ylab = "Count")
hist(College$PhD, col = 2, xlab = "PhD", ylab = "Count")
hist(College$Grad.Rate, col = 5, xlab = "Grad Rate", ylab = "Count")
hist(College$perc.alumni, col = 4, xlab = "% alumni", ylab = "Count")
summary(College$Grad.rate)