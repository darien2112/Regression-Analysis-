##################################################################
# NAME: Anh Tuan Le                                              #  
# DATE: 27-03-2024                                               #
# PURPOSE: 1.20 and 1.24 in KNN                                  #
# NOTES: 1)                                                      #
#                                                                #
##################################################################

# Read in the data 
copier <- read.table(file = "C:\\Users\\pepu2\\OneDrive\\Máy tính\\Self-Studying\\STAT 450 (870) - Regression Analysis\\Datasets\\Chapter  1 Data Sets\\CH01PR20.txt",
                     header = FALSE, col.names = c("minutes", "copiers"), sep = "")

# Check first few observations
head(copier)

# Fit the SLR Model and save the results in mod.fit
mod.fit <- lm(minutes ~ copiers, data = copier)
summary(mod.fit)

# Simple scatter plot
attach(copier)
plot(x = copiers, y = minutes,
     xlab = "Number of copiers", ylab = "Minutes spend by service person",
     main = "Minutes spent vs. Number of copiers",
     col = "black", pch = 1,
     panel.first = grid(col = "gray", lty = "dotted"))

# Add sample model to plot 
curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x,
      col = "red", lty = "solid", lwd = 2)

# Other ways to get the line on the plot 
# abline(a = mod.fit$coefficients[1], b = mod.fit$coefficients[2],
       # col = "red", lty = "solid", lwd = 2)

# segments(x0 = min(copiers), y0 = mod.fit$coefficients[2] + mod.fit$coefficients[2] * min(copiers),
        # x1 = max(copiers), y1 = mod.fit$coefficients[2] + mod.fit$coefficients[2] * max(copiers),
        # lty = 1, col = "blue", lwd = 2)

detach(copier)
# Residuals put into a dataset with the variables
save.resid <- data.frame(copier, residuals = round(mod.fit$residuals, 2))
head(save.resid)

# Check sum to 0
sum(mod.fit$residuals)

# Sum of residuals^2
sum(mod.fit$residuals^2)

summary.fit <- summary(mod.fit)
summary.fit$sigma
# MSE
summary.fit$sigma^2
