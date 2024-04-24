############################################################################
# NAME:  Anh Tuan Le                                                       #
# DATE:  03-28-24                                                          #
# PURPOSE: Chapter 2 example with Pizza and College GPA data set           #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

# Read in the data 
copier <- read.table(file = "C:\\Users\\pepu2\\OneDrive\\Máy tính\\Self-Studying\\STAT 450 (870) - Regression Analysis\\Datasets\\Chapter  1 Data Sets\\CH01PR20.txt",
                     header = FALSE, col.names = c("minutes", "copiers"), sep = "")

# Check first few observations
head(copier)

# Fit the SLR model -> save the results in mod.fit
mod.fit <- lm(minutes ~ copiers, data = copier)
sum.fit <- summary(mod.fit)
sum.fit

names(sum.fit)

# Get the estimated Var(b1)
sum.fit$coefficients

sd.bl <- sum.fit$coefficients[2, 2] # Estimated sqrt(Var(b1))
var.bl <- sum.fit$coefficients[2, 2]^2
data.frame(sd.bl, var.bl)

# Find C.I. for beta1
alpha <- 0.1

low <- mod.fit$coefficients[2] - qt(p = 1 - alpha/2, df = mod.fit$df.residual) * sqrt(var.bl)

up <- mod.fit$coefficients[2] + qt(p = 1 - alpha/2, df = mod.fit$df.residual) * sqrt(var.bl)

data.frame(bl = mod.fit$coefficients[2], low, up)

# Remove the "copiers" extra label in the output
low <- as.numeric(mod.fit$coefficients[2]) - qt(p = 1 - alpha/2, df = mod.fit$df.residual) * sqrt(var.bl)

up <- as.numeric(mod.fit$coefficients[2]) + qt(p = 1 - alpha/2, df = mod.fit$df.residual) * sqrt(var.bl)

data.frame(b1 = as.numeric(mod.fit$coefficients[2]), low, up)

# 90% CI for the mean service time on calls (six copiers are serviced)
save.ci <- predict(object = mod.fit, newdata = data.frame(copiers = 6), interval = "confidence", level = 0.90)
save.ci

# 90% PI for the mean service time on calls (six copiers are serviced)
save.pi <- predict(object = mod.fit, newdata = data.frame(copiers = 6), interval = "prediction", level = 0.90)
save.pi

# 90% CI for the mean time per copier (6 copiers are serviced)
save.ci[, 2:3]/6

# CI and PI bands plot 
plot(x = copier$copiers, y = copier$minutes,
     xlab = "Number of copiers", ylab = "Minutes spent by service person",
     main = "Minutes spent vs. Number of copiers",
     col = "black", pch = 1, cex = 1.0,
     panel.first = grid(col = "gray", lty = "dotted"))

curve(expr = predict(object = mod.fit, newdata = data.frame(copiers = x)),
      col = "red", lty = "solid", lwd = 1, add = TRUE,
      from = min(copier$copiers), to = max(copier$copiers))

curve(expr = predict(object = mod.fit, newdata = data.frame(copiers = x),
                     
                     interval = "confidence", level = 0.90)[,2], col = "darkgreen", lty =
        
        "dashed", lwd = 1, add = TRUE, from = min(copier$copiers), to =
        
        max(copier$copiers))

curve(expr = predict(object = mod.fit, newdata = data.frame(copiers = x),
                     
                     interval = "confidence", level = 0.90)[,3], col = "darkgreen", lty =
        
        "dashed", lwd = 1, add = TRUE, from = min(copier$copiers), to =
        
        max(copier$copiers))

curve(expr = predict(object = mod.fit, newdata = data.frame(copiers = x),
                     
                     interval = "prediction", level = 0.90)[,2], col = "blue", lty = "dashed",
      
      lwd = 1, add = TRUE, from = min(copier$copiers), to = max(copier$copiers))

curve(expr = predict(object = mod.fit, newdata = data.frame(copiers = x),
                     
                     interval = "prediction", level = 0.90)[,3], col = "blue", lty = "dashed",
      
      lwd = 1, add = TRUE, from = min(copier$copiers), to = max(copier$copiers))

legend(locator(1), legend = c("Sample model", "95% C.I.", "95% P.I."), col =
         
         c("red", "darkgreen", "blue"), lty = c("solid", "dashed", "dashed"), bty =
         
         "n", cex = 0.75)
