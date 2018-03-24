download.file("https://ndownloader.figshare.com/files/2292169",
              "data/portal_data_joined.csv")

surveys <- read.csv("data/portal_data_joined.csv")

surveys

head(surveys)

str(surveys)

class(surveys)

surveys[1, 1]
surveys[3, ]
surveys[1, ]
surveys[1:3, 7]

surveys_200 <- surveys[200, ]
nrow(surveys_200)
surveys_last <- surveys[nrow(surveys), ]
surveys_middle <- surveys[nrow(surveys)/2, ]
surveys_head <- surveys[-(7:nrow(surveys)), ]

sex <- factor(c("male", "female", "female", "male"))
levels(sex)
nlevels(sex)
sex <- factor(sex, levels = c("male", "female"))
levels(sex)
nlevels(sex)
sex
as.character(sex)

f <- factor(c(1990, 1983, 1977, 1998, 1990))
as.numeric(female)               # Wrong! And there is no warning...
as.numeric(as.character(f)) # Works...
as.numeric(levels(f))[f]    # The recommended way.
plot(surveys$sex)

sex <- surveys$sex
head(sex)
levels(sex)
levels(sex) [1] <- "missing"
levels(sex)
head(sex)

levels(sex)[2:3] <- c("female", "male")
sex <- factor(sex, levels = c("female", "male", "missing"))
plot(sex)

## Compare the difference between our data read as `factor` vs `character`.
surveys <- read.csv("data/portal_data_joined.csv", stringsAsFactors = TRUE)
str(surveys)
surveys <- read.csv("data/portal_data_joined.csv", stringsAsFactors = FALSE)
str(surveys)
## Convert the column "plot_type" into a factor
surveys$plot_type <- factor(surveys$plot_type)

str(surveys)

library(lubridate)

my_date <- ymd("2015-01-01")
str(my_date)
my_date <- ymd(paste("2015", "1", "1", sep = "-")) # sep indicates the character to use to separate each component
str(my_date)

paste(surveys$year, surveys$month, surveys$day, sep = "-")
ymd(paste(surveys$year, surveys$month, surveys$day, sep = "-"))
surveys$date <- ymd(paste(surveys$year, surveys$month, surveys$day, sep = "-"))
str(surveys) # notice the new column, with 'date' as the class
summary(surveys$date)
head(surveys[is.na(surveys$date),  c("year", "month", "day")])
