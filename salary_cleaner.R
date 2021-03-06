# Creating test dataframe with fake data
a <- c("2034", "test name 2","100","Jedi","$1 billion","$50 million","$1.05 billion")
b <- c("2035", "test name", "1", "King President", "$282,123,120", "$282,123,120", "$282,123,120")
g <- rbind(a,b)
colnames(g) <- c("Year", "Hospital", "Rank", "Position Title", "Salary", "Fringe Benefits", "Total")

# setting directory to one with the csvs of salaries
files_list <- dir("~/Documents/Github/hospsalaries", full.names = TRUE) #creates a list of files
id <- 1:length(files_list)

# loop to go through all the files
for (i in id) {
  salaries <- read.csv(files_list[i], stringsAsFactors=F)

  #identifying the year and the name of the first hospital
  year <- salaries[6,1]
  salaries_name <- salaries[4,1]
  salaries_name_num <- 1
  rank <- "test"

  #finding the length of the individual spreadsheet
  sal_length <- 1:nrow(salaries)

  #loop to go through the whole spreadsheet line by line
  for (i in sal_length) {
  
  #new hospital name is identified every time the list cycle starts back at 1
  if(salaries[i,1] == "1"){
    #hospital name is always 6 rows above the no. 1
    salaries_name_num <- i-6
    #bringing in the column information to the right of the rank number
    salaries_name <- salaries[salaries_name_num,1]
    rank <- salaries[i,1]
    title <- salaries[i,2]
    salary <- salaries[i,4]
    fringe <- salaries[i,5]
    total <- salaries[i,6]
    #building a row 
    h <- c(year, salaries_name, rank, title, salary, fringe, total)
    #appending the row to the dataframe
    g <- rbind(g, h)
    #all other ranks are treated the same. Get the data in columns correspondening to the rank
  } else if(salaries[i,1]=="2" | salaries[i,1] == "3" | 
              salaries[i,1] == "4"| salaries[i,1] == "5" | 
              salaries[i,1] == "6" | salaries[i,1] == "7" | 
              salaries[i,1] == "8" | salaries[i,1] == "9" | salaries[i,1] == "10"
    ) {
    rank <- salaries[i,1]
    title <- salaries[i,2]
    salary <- salaries[i,4]
    fringe <- salaries[i,5]
    total <- salaries[i,6]
    h <- c(year, salaries_name, rank, title, salary, fringe, total)
    g <- rbind (g,h)
  }
}  
}
datag <- data.frame(g)

# getting rid of the test rows
datag <- datag[-1,]
datag <- datag[-1,]

#Cleaning up the data. Getting rid of FISCAL YEAR string so it leaves behind only the year
datag$Year <- gsub("FISCAL YEAR ", "", datag$Year)
write.csv(datag, "salaries-since-2009-updated.csv")

