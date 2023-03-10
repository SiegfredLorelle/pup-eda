# Group 2                       BSCOE 1-1             Engineering Data Analysis
# Lim, Shin I.
# Ilao,Kenji C.
# Esguerra,Edgar Jr. P.
# Mina, Siegfred Lorelle C.
# Agcaoili, Leon Adriel Franco M.

# Group Bonus Assessment #4

# Import libraries
library("readxl")
library("dplyr")

# Read the excel data
SAMPLE_DATA = read_excel("SAMPLE_DATA.xlsx")
attach(SAMPLE_DATA)

# I.
NEWDATA <- SAMPLE_DATA %>% sample_frac(0.3, replace= F) %>%                                        #1
                            filter(FAVORITE_COLOR %in% c("RED", "BLUE", "YELLOW")) %>%             #2
                            select(RESPONDENT_NUMBER, FAVORITE_SUBJECT,                            #3
                                   GRADE_IN_MATH:GRADE_IN_MEDIA_AND_INFORMATION_LITERACY) %>%
                            rename(Student_number = RESPONDENT_NUMBER,                             #4
                                   Favorite_subject = FAVORITE_SUBJECT,
                                   Grade_in_math = GRADE_IN_MATH,
                                   Grade_in_statistics_and_probability = GRADE_IN_STATISTICS_AND_PROBABILITY,
                                   Grade_in_science = GRADE_IN_SCIENCE,
                                   Grade_in_media_and_information_literacy = GRADE_IN_MEDIA_AND_INFORMATION_LITERACY) %>%
                            mutate(Average = ((Grade_in_math + Grade_in_statistics_and_probability + Grade_in_science + Grade_in_media_and_information_literacy)/4))   #5

# II.
NEWDATA %>% group_by(Favorite_subject) %>% summarise(mean(Average, na.rm= T),
                                                        sd(Average, na.rm= T))

detach(SAMPLE_DATA)

print("View the new data in the environment panel in Rstudio. Note that chosen data are random. Mean and standard deviation per subjects are printed, if not run then line 32 manually.")
