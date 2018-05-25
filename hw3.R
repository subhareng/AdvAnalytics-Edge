cc<- read.csv("C:/Users/subha/Downloads/climate_change.csv", header = TRUE)
train <- subset(cc, cc$Year < 2007)
test <- subset(cc, cc$Year > 2006)
mean(train$CO2)
#  Temp, using MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI, and Aerosol
lmMod <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
head(train)
summary(lmMod)
cormat <- cor(train)
cormat$N2O
submodel <- lm(Temp ~ MEI + N2O + TSI + Aerosols, data=train)
summary(submodel)
p<- predict(step(lmMod), test)
# New dataset with predicted values
newdata<- data.frame (pred = p, variables = test)

# Linear model to predicted values
new.model<- lm (eruptions~., data = newdata)
summary (new.model)[[9]] ## R-squared from predicted values