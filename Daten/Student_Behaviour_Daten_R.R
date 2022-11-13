SB_Data <- read.csv("C:\\Users\\Paul\\OneDrive\\Dokumente\\GitHub\\ElmTest\\Daten\\Student_Behaviour.csv")
table(SB_Data$daily.studing.time)

SB_Data$daily.studing.time <- factor(SB_Data$daily.studing.time, 
                                levels = c("0 - 30 minute", "30 - 60 minute",
                                           "1 - 2 Hour", "2 - 3 hour", "3 - 4 hour",
                                           "More Than 4 hour"))

table(SB_Data$daily.studing.time)





nrow(SB_Data)
empty(SB_Data)
is.null(SB_Data)
