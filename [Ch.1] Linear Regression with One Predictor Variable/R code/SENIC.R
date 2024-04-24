senic <- read.table(file = "C:\\Users\\pepu2\\OneDrive\\Máy tính\\Self-Studying\\STAT 450 (870) - Regression Analysis\\Datasets\\Appendix C Data Sets\\APPENC01.txt",
                    header = FALSE, col.names = c("ID", "stay", "age", "infection", "culture", "xray", "beds", "school",
                                                  "region", "census", "nurses", "facilities"), sep = "")

# Check first few and last few observations
head(senic)

tail(senic)

par(mfrow = c(2,2))
attach(senic)

# SLR Model between stay and infection
mod.fit.infection <- lm(stay ~ infection, data = senic)
summary(mod.fit.infection)

plot(x = infection, y = stay,
     xlab = "Infection", ylab = "Stay", main = "Stay vs. Infection",
     col = "black", pch = 1,
     panel.first = grid(col = "gray", lty = "dotted"))

curve(expr = mod.fit.infection$coefficients[1] +
             mod.fit.infection$coefficients[2] * x,
             col = "red", lty = "solid", lwd = 2,
             add = TRUE, from = min(infection), to = max(infection))

# SLR model btw stay and facilities
mod.fit.facilities <- lm(facilities ~ stay, data = senic)
summary(mod.fit.facilities)

plot(x = facilities, y = stay,
     xlab = "Facilities", ylab = "Stay", main = "Stay vs. Facilities",
     col = "black", pch = 1,
     panel.first = grid(col = "gray", lty = "dotted"))

curve(expr = mod.fit.facilities$coefficients[1] + mod.fit.facilities$coefficients[2] * x,
      col = "red", lty = "solid", lwd = 2,
      add = TRUE, from = min(senic$facilities), to = max(senic$facilities))

# SLR model btw stay and xray
mod.fit.xray <- lm(xray ~ stay, data = senic)
summary(mod.fit.xray)

plot(x = xray, y = stay,
     xlab = "X-ray", ylab = "Stay", main = "Stay vs. X-ray",
     col = "black", pch = 1,
     panel.first = grid(col = "gray", lty = "dotted"))

curve(expr = mod.fit.xray$coefficients[1] + mod.fit.xray$coefficients[2] * x,
      col = "red", lty = "solid", lwd = 2,
      add = TRUE, from = min(senic$xray), to = max(senic$xray))
