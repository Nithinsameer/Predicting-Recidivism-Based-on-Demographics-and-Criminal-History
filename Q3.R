df = read.csv("NIJ_s_Recidivism_Challenge_Full_Dataset (5).csv")

install.packages("rpart")

library(rpart)

fit <- rpart(Recidivism_Within_3years ~ Jobs_Per_Year + Percent_Days_Employed, data = df, method = "class")

summary(fit)

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(fit, yesno=2, type=0, extra=1)

correlationMatrix <- cor(df[c("Recidivism_Within_3years", "Jobs_Per_Year", "Percent_Days_Employed")], use = "complete.obs")
print(correlationMatrix)
