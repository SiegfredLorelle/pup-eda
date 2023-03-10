# Group 2                       BSCOE 1-1             Engineering Data Analysis
# Lim, Shin I.
# Ilao,Kenji C.
# Esguerra,Edgar Jr. P.
# Mina, Siegfred Lorelle C.
# Agcaoili, Leon Adriel Franco M.

# Group Bonus Assessment #3

# Import library needed to read excel
library("readxl")

# Read the excel data
SAMPLE_DATA = read_excel("SAMPLE_DATA.xlsx")
attach(SAMPLE_DATA)

# 1.
DF <- data.frame(GENDER,                                     #1.1
                AGE,                                         #1.2
                FAVORITE_SUBJECT,                            #1.3
                FAVORITE_COLOR,                              #1.4
                GRADE_IN_MATH,                               #1.5
                GRADE_IN_STATISTICS_AND_PROBABILITY,         #1.6
                GRADE_IN_SCIENCE,                            #1.7
                GRADE_IN_MEDIA_AND_INFORMATION_LITERACY)     #1.8

#2.
#2.1
subset2.1 <- subset(DF, GENDER == "MALE"
                    & FAVORITE_COLOR == "BLUE"
                    & AGE >= 19)

#2.2
subset2.2 <- subset(DF, GENDER == "FEMALE"
                    & FAVORITE_SUBJECT == "ENGLISH"
                    & GRADE_IN_MATH <= 95 
                    & GRADE_IN_STATISTICS_AND_PROBABILITY <= 95)

#2.3
subset2.3 <- subset(DF, GENDER == "FEMALE"
                    & FAVORITE_COLOR != "RED"
                    & FAVORITE_SUBJECT == "MATH")

#2.4
subset2.4 <- subset(DF, GENDER == "MALE"
                    & FAVORITE_SUBJECT != "MATH"
                    & GRADE_IN_MATH >= 95
                    & GRADE_IN_STATISTICS_AND_PROBABILITY >= 95)

#2.5
subset2.5 <- subset(DF, FAVORITE_SUBJECT != "ENGLISH"
                    & GRADE_IN_MEDIA_AND_INFORMATION_LITERACY > 95)

#2.6
subset2.6 <- subset(DF, FAVORITE_SUBJECT == "SCIENCE"
                    & GRADE_IN_SCIENCE < 95)


#3.
GENDER_Extract <- factor(SAMPLE_DATA$GENDER)
FAVORITE_SUBJECT_Extract <- factor(SAMPLE_DATA$FAVORITE_SUBJECT)
FAVORITE_COLOR_Extract <- factor(SAMPLE_DATA$FAVORITE_COLOR)

list3.0 <- list(Gender= GENDER_Extract, Favorite_subject= FAVORITE_SUBJECT_Extract, Favorite_color= FAVORITE_COLOR_Extract)


#4
#4.1
vector4.1 <- c(1:20)
#4.2
vector4.2 <- c("Red","Blue", "Yellow", "Green")
#4.3
vector4.3 <- c("A", "B", "C", "D", "E")


#5
matrix5.0 <- matrix(vector4.1, nrow= 4, ncol= 5, byrow= F, dimnames= list(vector4.2,vector4.3))


#6
extract6.0 <- matrix5.0[ ,c("A", "C", "D")]


#7
#extract from num 6
extract7.0 <- extract6.0[c("Blue", "Yellow"), ]


#8
seq4list <- seq(1, 20, length= 40)
fav_arist <- c("Selena", "Olivia", "Taylor", "Jessi", "Billie")
fav_art_ranked4list <- factor(fav_arist, ordered= TRUE, levels= c("Billie", "Jessi", "Taylor", "Olivia", "Selena"))

list8.0 <- list(sequence= seq4list, favorite_artist= fav_art_ranked4list)


detach(SAMPLE_DATA)

print("View the data in the environment panel in Rstudio or manually print the variables.")