############################################################################
# NAME:  Anh Tuan Le                                                       #
# DATE:  03-28-24                                                          #
# PURPOSE: Chapter 2 example with Pizza and College GPA data set           #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

#Read in the data
gpa2<-read.table(file = "C:\\Users\\pepu2\\OneDrive\\Máy tính\\Self-Studying\\STAT 450 (870) - Regression Analysis\\[Ch. 2] Inferences in Regression Analysis\\College_GPA_pizza.txt", header=TRUE, sep = "")
head(gpa2)


#Fit the simple linear regression model and save the results in mod.fit
mod.fit<-lm(formula = College.GPA ~ pizza, data = gpa2)

#Summarize the information stored in mod.fit
summary(mod.fit)

#Scatter plot with sample model plotted
plot(x = gpa2$pizza, y = gpa2$College.GPA, xlab = "Times ate pizza", ylab = "College GPA", main = "College GPA vs. Pizza consumption", 
     ylim = c(0,4), col = "black", pch = 1, lwd = 2, panel.first = grid(col = "gray", lty = "dotted"))
curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x, col = "red", lty = "solid", lwd = 2, add = TRUE,
      c(min(gpa2$pizza), max(gpa2$pizza)))