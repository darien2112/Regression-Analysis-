##################################################################
# NAME: Anh Tuan Le                                              #  
# DATE: 26-03-2024                                               #
# PURPOSE: Chapter 1 example with the GPA data set               #
# NOTES: 1)                                                      #
#                                                                #
##################################################################

# Load data
file_path <- "C:/Users/pepu2/OneDrive/Máy tính/Self-Studying/STAT 450 (870) - Regression Analysis/[Ch. 1] Linear Regression with One Predictor Variable/Dataset/gpa.txt"

gpa <- gpa.txt

# Print data set
gpa

# Summary statistics for variables 
summary(gpa)

# Print one variable 
gpa$HS.GPA

gpa[,1]

# Simple Scatter Plot
plot(x = gpa$HS.GPA, y=gpa$College.GPA,
     xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA",
     xlim = c(0, 4.5), ylim = c(0, 4.5), col = "red", pch = 1, cex = 1.0,
     panel.first = grid(col = "gray", lty = "dotted"))

##################################################################
# Find estimated SLR Model
# Fit the SLR model and save the results in mod.fit

mod.fit <- lm(formula = College.GPA ~ HS.GPA, data = gpa)

mod.fit

# See the names of all of the object components
names(mod.fit)

mod.fit$coefficients

mod.fit$residuals

# Put some of the components into a data.frame object
save.fit <- data.frame(gpa, College.GPA.hat = round(mod.fit$fitted.values, 2), residuals = round(mod.fit$residuals, 2))

# Print contents of save.fit
save.fit

# Summarize the information stored in mod.fit
summary(mod.fit)

# Prediction 
predict(object = mod.fit)

new.data <- data.frame(HS.GPA = c(2,3))
save.pred <- predict(object = mod.fit, newdata = new.data)
round(save.pred, 2)

##################################################################
# Put sample model on plot

# Open a new graphics window 
win.graph(width = 6, height = 6, pointsize = 10)

# Same scatter plot as before
plot(x = gpa$HS.GPA, y = gpa$College.GPA,
     xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA", 
     xlim = c(0, 4.5), ylim = c(0, 4.5), col = "red", pch = 1, cex = 1.0, 
     panel.first = grid(col = "gray", lty = "dotted"))

# Puts the line y = a + bx on the plot
abline(a = mod.fit$coefficients[1], b = mod.fit$coefficients[2], lty = 1, col = "blue", lwd = 2)

# Draw a line from (x0, y0) to (x1, y1)
segments(x0 = min(gpa$HS.GPA), y0 = mod.fit$coefficients[1] + mod.fit$coefficients[2]*min(gpa$HS.GPA),
         x1 = max(gpa$HS.GPA), y1 = mod.fit$coefficients[1] + mod.fit$coefficients[2]*max(gpa$HS.GPA),
         lty = "solid", col = "blue", lwd = 2)

##################################################################
# Create a function to find the sample model and put the line on a scatter plot

my.reg.func <-function(x, y, data) {
  
  # Fit the SLR Model and save the results in mod.fit
  mod.fit <-lm(y ~ x, data = data)
  
  # Open a new graphics window 
  win.graph(width = 6, height = 6, pointsize = 10)
  
  # Same scatter plot as before 
  plot(x = x, y = y, xlab = "x", ylab = "y", main = "y vs. x", 
       col = "red", pch = 1, cex = 1.0,
       panel.first = grid(col = "gray", lty = "dotted"))
  
  # Draw a line from (x0, y0) to (x1, y1)
  segments(x0 = min(x), y0 = mod.fit$coefficients[1] + mod.fit$coefficients[2]*min(x),
           x1 = max(x), y1 = mod.fit$coefficients[1] + mod.fit$coefficients[2]*max(x),
           lty = 1, col = "blue", lwd = 2)
  
  # This is the object returned
  mod.fit
}

save.it <- my.reg.func(x = gpa$HS.GPA, y = gpa$College.GPA, data = gpa)
names(save.it)
summary(save.it)
