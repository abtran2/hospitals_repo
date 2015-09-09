
# Bring in csv

# For loop 

# Go through every line...


# If the first line isn't blank, then first column in row is name of hospital
# Check for Hospital Total Margins, Private Payment to Cost Ratio, Medicare Payment to Cost Ratio, Medicade Payment to Cost Ratio
# Uncompensated Care Cost, Uncompoensated Care % of Total Expenses, Discharges, ALOS, Staffed Beds, Occupancy of staffed beds
# Full Time Equivalent Employees, Total Case Mix Index, Medical Asistance, Medicaid, Champus/Tricare
# Emergency Room - Treated and Admitted, Emergency Room - Treated and Discharged, Total Emergency Room Visits
# If 16, 18, 20, 22, 24, 26, 28, 30... 70, then create new row.


a <- c("Hospital","Type","2010","2011","2012","2013")
b <- c("test name", "test type", "$282,123,120", "$282,123,120", "$282,123,120", "$282,123,120")
g <- rbind(a,b)

colnames(g) <- c("Hospital", "Type", "y2010", "y2011", "y2012", "y2013")


hospitals <- read.csv("hospital_list.csv", stringsAsFactors=F)

hospital_name <- hospitals[1,2]
hospital_name_num <- 1
type <- "test"


hosp_length <- 1:nrow(hospitals)

for (i in hosp_length) {

  if(hospitals[i,2] == "16" | hospitals[i,2] == "18"| hospitals[i,2] == "20" | hospitals[i,2] == "22" | 
       hospitals[i,2] == "24" | hospitals[i,2] == "26" | hospitals[i,2] == "28" | 
       hospitals[i,2] == "30" | hospitals[i,2] == "32" | hospitals[i,2] == "34" | hospitals[i,2] == "36" | hospitals[i,2] == "38" | hospitals[i,2] == "40" | hospitals[i,2] == "42" |
       hospitals[i,2] == "44" | hospitals[i,2] == "46" | hospitals[i,2] == "48" | hospitals[i,2] == "50" | hospitals[i,2] == "52" | hospitals[i,2] == "54" | hospitals[i,2] == "56" | 
       hospitals[i,2] == "58" | hospitals[i,2] == "60" | hospitals[i,2] == "62" | hospitals[i,2] == "64" | hospitals[i,2] == "66" | hospitals[i,2] == "68" | hospitals[i,2] == "70" | hospitals[i,2] =="72"
        ){
        hospital_name_num <- i+1
        hospital_name <- hospitals[hospital_name_num,2]
  } else if(hospitals[i,2]=="Hospital Total Margins" | hospitals[i,2] == "Private Payment to Cost Ratio" | 
              hospitals[i,2] == "Medicare Payment to Cost Ratio"| hospitals[i,2] == "Medicaid Payment to Cost Ratio" | 
              hospitals[i,2] == "Uncompensated Care Cost" | hospitals[i,2] == "Uncompensated Care % of Total Expenses" | 
              hospitals[i,2] == "Discharges" | hospitals[i,2] == "ALOS" | hospitals[i,2] == "Staffed Beds" | 
              hospitals[i,2] == "Occupancy of staffed beds" | hospitals[i,2] == "Full Time Equivalent Employees" | 
              hospitals[i,2] == "Total Case Mix Index" |  
              hospitals[i,2] == "Champus / TRICARE" | hospitals[i,2] == "Emergency Room - Treated and Admitted" | 
              hospitals[i,2] == "Emergency Room - Treated and Discharged" | 
              hospitals[i,2] == "Total Emergency Room Visits" ) {
        type <- hospitals[i,2]
        y2010 <- hospitals[i,3]
        y2011 <- hospitals[i,4]
        y2012 <- hospitals[i,5]
        y2013 <- hospitals[i,6]
        h <- c(hospital_name, type, y2010, y2011, y2012, y2013)
        g <- rbind (g,h)
  } else if(hospitals[i,2]=="Medicare" && hospitals[i+1,2]=="Medical Assistance") {
    type <- hospitals[i,2]
    y2010 <- hospitals[i,3]
    y2011 <- hospitals[i,4]
    y2012 <- hospitals[i,5]
    y2013 <- hospitals[i,6]
    h <- c(hospital_name, type, y2010, y2011, y2012, y2013)
    g <- rbind (g,h)
  } else if(hospitals[i,2]=="Medicaid" && hospitals[i+1,2]=="Other Medical Assistance") {
    type <- hospitals[i,2]
    y2010 <- hospitals[i,3]
    y2011 <- hospitals[i,4]
    y2012 <- hospitals[i,5]
    y2013 <- hospitals[i,6]
    h <- c(hospital_name, type, y2010, y2011, y2012, y2013)
    g <- rbind (g,h)
  }
}  
  
datag <- data.frame(g)
datag <- datag[-1,]
datag <- datag[-1,]


write.csv(datag, "datag.csv")



a <- c("Hospital","Type","2010","2011","2012","2013")
b <- c("test name", "test type", "$282,123,120", "$282,123,120", "$282,123,120", "$282,123,120")
g <- rbind(a,b)


colnames(g) <- c("Hospital", "Type", "y2006", "y2007", "y2008", "y2009")


hospitals <- read.csv("fs_report2009.csv", stringsAsFactors=F)

hospital_name <- hospitals[1,2]
hospital_name_num <- 1


hosp_length <- 1:nrow(hospitals)

for (i in hosp_length) {
  
  if(hospitals[i,2] == "21" | hospitals[i,2] == "24"| hospitals[i,2] == "27" | hospitals[i,2] == "30" | 
       hospitals[i,2] == "33" | hospitals[i,2] == "36" | hospitals[i,2] == "39" | 
       hospitals[i,2] == "42" | hospitals[i,2] == "45" | hospitals[i,2] == "48" | hospitals[i,2] == "51" | hospitals[i,2] == "54" | hospitals[i,2] == "57" | hospitals[i,2] == "60" |
       hospitals[i,2] == "63" | hospitals[i,2] == "66" | hospitals[i,2] == "69" | hospitals[i,2] == "72" | hospitals[i,2] == "75" | hospitals[i,2] == "78" | hospitals[i,2] == "81" | 
       hospitals[i,2] == "84" | hospitals[i,2] == "87" | hospitals[i,2] == "90" | hospitals[i,2] == "93" | hospitals[i,2] == "96" | hospitals[i,2] == "99" | hospitals[i,2] == "102" | hospitals[i,2] =="105"
  ){
    hospital_name_num <- i+1
    hospital_name <- hospitals[hospital_name_num,2]
  } else if(hospitals[i,2]=="Hospital Total Margins" | hospitals[i,2] == "Private Payment to Cost Ratio" | 
              hospitals[i,2] == "Medicare Payment to Cost Ratio"| hospitals[i,2] == "Medicaid Payment to Cost Ratio" | 
              hospitals[i,2] == "Uncompensated Care Cost" | hospitals[i,2] == "Uncompensated care % of total expenses" | 
              hospitals[i,2] == "Total Discharges" | hospitals[i,2] == "ALOS" | hospitals[i,2] == "Staffed Beds" | 
              hospitals[i,2] == "Occupancy of staffed beds" | hospitals[i,2] == "Full Time Equivalent Employees" | 
              hospitals[i,2] == "Total Case Mix Index" | hospitals[i,2] == "Emergency Room - Treated and Admitted" | 
              hospitals[i,2] == "Emergency Room - Treated and Discharged" | 
              hospitals[i,2] == "Total Emergency Room Visits" ) {
    type <- hospitals[i,2]
    y2010 <- hospitals[i,3]
    y2011 <- hospitals[i,4]
    y2012 <- hospitals[i,5]
    y2013 <- hospitals[i,6]
    h <- c(hospital_name, type, y2010, y2011, y2012, y2013)
    g <- rbind (g,h)
  } else if(hospitals[i,2]=="Medicare" && hospitals[i-2,2]=="DISCHARGES") {
    type <- hospitals[i,2]
    y2010 <- hospitals[i,3]
    y2011 <- hospitals[i,4]
    y2012 <- hospitals[i,5]
    y2013 <- hospitals[i,6]
    h <- c(hospital_name, type, y2010, y2011, y2012, y2013)
    g <- rbind (g,h)
  } else if(hospitals[i,2]=="Medicaid" && hospitals[i-4,2]=="DISCHARGES") {
    type <- hospitals[i,2]
    y2010 <- hospitals[i,3]
    y2011 <- hospitals[i,4]
    y2012 <- hospitals[i,5]
    y2013 <- hospitals[i,6]
    h <- c(hospital_name, type, y2010, y2011, y2012, y2013)
    g <- rbind (g,h)
  } else if(hospitals[i,2]=="Champus / TRICARE" && hospitals[i-6,2]=="DISCHARGES") {
    type <- hospitals[i,2]
    y2010 <- hospitals[i,3]
    y2011 <- hospitals[i,4]
    y2012 <- hospitals[i,5]
    y2013 <- hospitals[i,6]
    h <- c(hospital_name, type, y2010, y2011, y2012, y2013)
    g <- rbind (g,h)
  }
}  

datag <- data.frame(g)
datag <- datag[-1,]
datag <- datag[-1,]
datag$Type <- gsub("Total Discharges", "Discharges", datag$Type)
datag$Type <- gsub("Uncompensated care % of total expenses", "Uncompensated care % of Total Expenses", datag$Type)
datag$Hospital <- gsub("(Note: Johnson Memorial Hospital financial statements were not audited in FY 2007 - FY 2009.)", "LAWRENCE & MEMORIAL HOSPITAL", datag$Hospital)



write.csv(datag, "datah.csv")


# joining
library(plyr)
pre <- read.csv("datag.csv", stringsAsFactors=F)
post <- read.csv("datah.csv", stringsAsFactors=F) 

complete <- merge(post, pre, by=c("Hospital","Type"))

write.csv(complete, "complete.csv")
