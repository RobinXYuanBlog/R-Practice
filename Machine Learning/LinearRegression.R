LasVegas <- read.csv("/Users/robinxyuan/Downloads/LasVegas.csv", header = TRUE, sep = ";")
attach(LasVegas)

summary(LasVegas)

LasVegas$Pool = ifelse(Pool == 'YES', 1, 0)
LasVegas$Gym = ifelse(LasVegas$Gym == 'YES', 1, 0)
LasVegas$Tennis.court = ifelse(LasVegas$Tennis.court == 'YES', 1, 0)
LasVegas$Spa = ifelse(LasVegas$Spa == 'YES', 1, 0)
LasVegas$Casino = ifelse(LasVegas$Casino == 'YES', 1, 0)
LasVegas$Free.internet = ifelse(LasVegas$Free.internet == 'YES', 1, 0)

devices <- LasVegas[, c("Pool", "Gym", "Tennis.court", "Spa", "Casino", "Free.internet")]
device_score <- apply(devices, 1, sum)
LasVegas$Device.score = device_score

scatter.smooth(x = LasVegas$Device.score, y = LasVegas$Nr..hotel.reviews)
