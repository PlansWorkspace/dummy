# Reads in data from excel file
ranges <- commandArgs(trailingOnly = TRUE)

PlnLossRateMap <- read.csv ("~/Gonkulator/ExportedFromAccessTempFiles/tbl_PlannedLossRate_Map.csv", header = TRUE,colClasses=c("character","character"))
Master_Data <- read.csv ("~/Gonkulator/ExportedFromAccessTempFiles/tbl_MasterDataFile.csv", header = TRUE, stringsAsFactors = FALSE)

Master_Data$Date_Ship   <- as.Date(Master_Data$Date_Ship,"%d/%m/%Y") # set elements to "date" class 
Master_Data$Date_Rcls_1 <- as.Date(Master_Data$Date_Rcls_1,"%d/%m/%Y") # set elements to "date" class
Master_Data$Date_Rcls_2 <- as.Date(Master_Data$Date_Rcls_2,"%d/%m/%Y") # set elements to "date" class
Master_Data$Date_Att    <- as.Date(Master_Data$Date_Att,"%d/%m/%Y") # set elements to "date" 

################################################################################################################################################                                                                                 sep = "-")

for (RPG in PlnLossRateMap$RPG ){
  Master_Data$RPG_Ship[Master_Data$RPG_Ship == RPG] <- PlnLossRateMap$Mapped[PlnLossRateMap$RPG == RPG]
  Master_Data$RPG_Rcls_1[Master_Data$RPG_Recclass1 == RPG] <- PlnLossRateMap$Mapped[PlnLossRateMap$RPG == RPG]
  Master_Data$RPG_Rcls_2[Master_Data$RPG_Recclass2 == RPG] <- PlnLossRateMap$Mapped[PlnLossRateMap$RPG == RPG] 
}

#Creates vector of RPGs to loop through (will need to change this, its basically just a place holder) 
RPGs <- names(table(Master_Data$RPG_Ship))

# User input
RTC_Start <- ranges[1]      #2012
RTC_End   <- ranges[2]      #2014
PRTC_Start <- ranges[3]     #2012
PRTC_End   <- ranges[4]     #2013
Rev <- ranges[5]
CFY <- ranges[6]

class(RTC_Start) <- "numeric"
class(RTC_End) <- "numeric"
class(PRTC_Start) <- "numeric"
class(PRTC_End) <- "numeric"
class(Rev) <- "character"
class(CFY) <- "numeric"

mode(RTC_Start) <- "numeric"
mode(RTC_End) <- "numeric"
mode(PRTC_Start) <- "numeric"
mode(PRTC_End) <- "numeric"
mode(Rev) <- "character"
mode(CFY) <- "numeric"

G_Total_RTC_Attrites.list        <- numeric(length(RPGs))
G_Total_RTC_ReclassOut.list      <- numeric(length(RPGs))
G_Total_RTC_Shipped.list         <- numeric(length(RPGs))
G_Total_PRTC_Attrites.list       <- numeric(length(RPGs))
G_Total_PRTC_Shipped.list        <- numeric(length(RPGs))
G_Total_PRTC_RTC_Attrites.list   <- numeric(length(RPGs))
G_Total_PRTC_RTC_ReclassOut.list <- numeric(length(RPGs))
G_Total_PRTC_ReclassOut.list     <- numeric(length(RPGs))

R_Total_RTC_Attrites.list        <- numeric(length(RPGs))
R_Total_RTC_ReclassOut.list      <- numeric(length(RPGs))
R_Total_RTC.list                 <- numeric(length(RPGs))
R_Total_PRTC_Attrites.list       <- numeric(length(RPGs))
R_Total_PRTC.list                <- numeric(length(RPGs))
R_Total_PRTC_RTC_Attrites.list   <- numeric(length(RPGs))
R_Total_PRTC_RTC_ReclassOut.list <- numeric(length(RPGs))
R_Total_PRTC_ReclassOut.list     <- numeric(length(RPGs))

# loops through the RPG list, calculates loss rates and stores them in the appropriate vector 
i <- 1  # Initialze indexing variable
for (RPG in RPGs){
  
# calculates the total shipped to RTC to be used as the denominator for the RTC loss rates 
# counts all instances that the RPG appears in the "RPG_Ship array between the RTC date ranges difined above 
G_Total_RTC_Shipped <- length(which(Master_Data$RPG_Ship == RPG & 
                                   Master_Data$Cohort >= RTC_Start &
                                   Master_Data$Cohort <= RTC_End)) 

# Calculates total RTC attrites for the RPG # counts all "RTC" entries in the "Attrites.Location" array that are in the RTC date range and where the service member did not reclass into thier current rating 
G_Total_RTC_Attrites <- length(which(Master_Data$RPG_Ship == RPG &
                                    Master_Data$Cohort >= RTC_Start &
                                    Master_Data$Cohort <= RTC_End &
                                    Master_Data$Location_Att == "RTC" &
                                    Master_Data$Location_Rcls_1 == "")) 

# calculates total RTC reclass outs for the RPG # counts all "RTC" entries in the "Location_Rcls_1" array that are in the RTC date range 
G_Total_RTC_ReclassOut <- length(which(Master_Data$RPG_Ship == RPG & 
                                      Master_Data$Cohort >= RTC_Start &
                                      Master_Data$Cohort <= RTC_End &
                                      Master_Data$Location_Rcls_1 == "RTC")) 

# counts the total number of people shipped to the RPG in the PRTC date range 
G_Total_PRTC_Shipped <- length(which(Master_Data$RPG_Ship == RPG &
                                    Master_Data$Cohort >= PRTC_Start &
                                    Master_Data$Cohort <= PRTC_End)) 

# counts the number of RTC attrites to the RPG in the PRTC date range 
G_Total_PRTC_RTC_Attrites <- length(which(Master_Data$RPG_Ship == RPG &
                                         Master_Data$Cohort >= PRTC_Start &
                                         Master_Data$Cohort <= PRTC_End &
                                         Master_Data$Location_Att == "RTC" &
                                         Master_Data$Location_Rcls_1 == "")) 

# counts the number of RTC reclass outs in the PRTC date range 
G_Total_PRTC_RTC_ReclassOut <- length(which(Master_Data$RPG_Ship == RPG & 
                                           Master_Data$Cohort >= PRTC_Start &
                                           Master_Data$Cohort <= PRTC_End &
                                           Master_Data$Location_Rcls_1 == "RTC")) 

# counts the number of PRTC attrites for the RPG in the PRTC date range 
G_Total_PRTC_Attrites <- length(which(Master_Data$RPG_Ship == RPG &
                                     Master_Data$Cohort >= PRTC_Start &
                                     Master_Data$Cohort <= PRTC_End &
                                     Master_Data$Location_Att == "Post" &
                                     Master_Data$Location_Rcls_1 == "")) 

# count the number of reclass outs from the RPG in the PRTC date range 
G_Total_PRTC_ReclassOut <- length(which(Master_Data$RPG_Ship == RPG & 
                                       Master_Data$Cohort >= PRTC_Start &
                                       Master_Data$Cohort <= PRTC_End &
                                       Master_Data$Location_Rcls_1 == "Post")) 


# The following caputures the raw numbers for display in the Loss_Rates data frame.
G_Total_RTC_Attrites.list[i]        <- G_Total_RTC_Attrites
G_Total_RTC_ReclassOut.list[i]      <- G_Total_RTC_ReclassOut
G_Total_RTC_Shipped.list[i]         <- G_Total_RTC_Shipped
G_Total_PRTC_Attrites.list[i]       <- G_Total_PRTC_Attrites
G_Total_PRTC_Shipped.list[i]        <- G_Total_PRTC_Shipped
G_Total_PRTC_RTC_Attrites.list[i]   <- G_Total_PRTC_RTC_Attrites
G_Total_PRTC_RTC_ReclassOut.list[i] <- G_Total_PRTC_RTC_ReclassOut
G_Total_PRTC_ReclassOut.list[i]     <- G_Total_PRTC_ReclassOut



######################################################################################################################################

R_Total_RTC <- length(which(Master_Data$RPG_Rcls_1 == RPG &
                           Master_Data$Cohort >= RTC_Start &
                           Master_Data$Cohort <= RTC_End &
                           Master_Data$Location_Rcls_1 != "")) 

R_Total_RTC_Attrites <- length(which(Master_Data$RPG_Rcls_1 == RPG &
                                    Master_Data$Cohort >= RTC_Start &
                                    Master_Data$Cohort  <= RTC_End &
                                    Master_Data$Location_Rcls_1 != "" &
                                    Master_Data$Location_Rcls_2 == "" &
                                    Master_Data$Location_Att == "RTC")) 

R_Total_RTC_ReclassOut <- length(which(Master_Data$RPG_Rcls_1 == RPG & 
                                      Master_Data$Cohort  >= RTC_Start &
                                      Master_Data$Cohort  <= RTC_End &
                                      Master_Data$Location_Rcls_2 == "RTC"))

R_Total_PRTC <- length(which(Master_Data$RPG_Rcls_1 == RPG &
                            Master_Data$Cohort  >= PRTC_Start &
                            Master_Data$Cohort  <= PRTC_End &
                            Master_Data$Location_Rcls_1 != ""))

R_Total_PRTC_RTC_Attrites <- length(which(Master_Data$RPG_Rcls_1 == RPG &
                                         Master_Data$Cohort  >= PRTC_Start &
                                         Master_Data$Cohort  <= PRTC_End &
                                         Master_Data$Location_Rcls_1 != "" &
                                         Master_Data$Location_Att == "RTC"))

R_Total_PRTC_RTC_ReclassOut <- length(which(Master_Data$RPG_Rcls_1 == RPG &
                                  Master_Data$Cohort  >= PRTC_Start &
                                  Master_Data$Cohort  <= PRTC_End &
                                  Master_Data$Location_Rcls_2 == "RTC")) 

R_Total_PRTC_Attrites <- length(which(Master_Data$RPG_Rcls_1 == RPG &
                                     Master_Data$Cohort  >= PRTC_Start &
                                     Master_Data$Cohort  <= PRTC_End &
                                     Master_Data$Location_Att == "Post")) 

R_Total_PRTC_ReclassOut <- length(which(Master_Data$RPG_Rcls_1 == RPG & 
                                       Master_Data$Cohort  >= PRTC_Start &
                                       Master_Data$Cohort  <= PRTC_End &
                                       Master_Data$Location_Rcls_2 == "Post"))


# The following caputures the raw numbers for display in the Loss_Rates data frame.
R_Total_RTC_Attrites.list[i]        <- R_Total_RTC_Attrites
R_Total_RTC_ReclassOut.list[i]      <- R_Total_RTC_ReclassOut
R_Total_RTC.list[i]                 <- R_Total_RTC
R_Total_PRTC_Attrites.list[i]       <- R_Total_PRTC_Attrites
R_Total_PRTC.list[i]                <- R_Total_PRTC
R_Total_PRTC_RTC_Attrites.list[i]   <- R_Total_PRTC_RTC_Attrites
R_Total_PRTC_RTC_ReclassOut.list[i] <- R_Total_PRTC_RTC_ReclassOut
R_Total_PRTC_ReclassOut.list[i]     <- R_Total_PRTC_ReclassOut

i <- i + 1
} # This closes the for loop

# Calculate loss rates for goal 
G_RTC_Attrition_Rate  <- G_Total_RTC_Attrites.list/G_Total_RTC_Shipped.list
G_RTC_ReclassOut_Rate <- G_Total_RTC_ReclassOut.list/G_Total_RTC_Shipped.list
G_PRTC_Attrition_Rate <- G_Total_PRTC_Attrites.list/G_Total_PRTC_Shipped.list
G_PRTC_ReclssOut_Rate <- G_Total_PRTC_ReclassOut.list/G_Total_PRTC_Shipped.list

# Calculate loss rates for Reclass ins
R_RTC_Attrition_Rate  <- R_Total_RTC_Attrites.list/R_Total_RTC.list
R_RTC_ReclassOut_Rate <- R_Total_RTC_ReclassOut.list/R_Total_RTC.list
R_PRTC_Attrition_Rate <- R_Total_PRTC_Attrites.list/R_Total_PRTC.list
R_PRTC_ReclssOut_Rate <- R_Total_PRTC_ReclassOut.list/R_Total_PRTC.list

# save loss rate data and amounts in data frame
Loss_Rates_UnMapped <- data.frame(FY = CFY,REV = Rev,RPGs, G_RTC_Attrition_Rate, G_RTC_ReclassOut_Rate, G_PRTC_Attrition_Rate, G_PRTC_ReclssOut_Rate,
                        G_Total_RTC_Attrites.list, G_Total_RTC_ReclassOut.list, G_Total_RTC_Shipped.list, G_Total_PRTC_Attrites.list,
                        G_Total_PRTC_Shipped.list, G_Total_PRTC_RTC_Attrites.list, G_Total_PRTC_RTC_ReclassOut.list,
                        G_Total_PRTC_ReclassOut.list, R_RTC_Attrition_Rate,R_RTC_ReclassOut_Rate,R_PRTC_Attrition_Rate,
                        R_PRTC_ReclssOut_Rate,R_Total_RTC_Attrites.list,R_Total_RTC_ReclassOut.list,R_Total_RTC.list,
                        R_Total_PRTC_Attrites.list,R_Total_PRTC.list,R_Total_PRTC_RTC_Attrites.list,R_Total_PRTC_RTC_ReclassOut.list,
                        R_Total_PRTC_ReclassOut.list )
                        Loss_Rates_UnMapped$RPGs <- as.character(Loss_Rates_UnMapped$RPGs)

num_Col <- length(names(Loss_Rates_UnMapped)) # number of columns for the output data frame
num_Rows <- length(PlnLossRateMap$RPG) # of rows for the output data frame

Loss_Rates_Mapped <- as.data.frame(setNames(replicate(num_Col ,numeric(num_Rows), simplify = F), names(Loss_Rates_UnMapped)))
Loss_Rates_Mapped$Mapped_From_RPG <- NA

i <- 1
for(RPG in PlnLossRateMap$RPG){
  Mapped_RPG <-PlnLossRateMap$Mapped[PlnLossRateMap$RPG == RPG]
  Loss_Rates_Mapped[i,] <- c(Loss_Rates_UnMapped[Loss_Rates_UnMapped$RPGs == Mapped_RPG,-1],Mapped_RPG)
  i <- i + 1
}

#############################################################################################################################################################################################################
write.csv(x = Loss_Rates_Mapped, file = "~/Gonkulator/ExportedFromRTempFiles/FY15PlannedLossRates.csv", row.names = FALSE)
