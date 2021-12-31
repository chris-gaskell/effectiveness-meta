# Supplementary Table 1

library(dplyr)

## Data & clean
StudyData <- readxl::read_excel("Input/Extraction_Full.xlsx", sheet = "Table1", skip = 1)

## Recode any required values ####
StudyData$Sector <- recode(StudyData$Sector, "Secondary/Tertiary/Specialist/IOP/BehaviouralHealth" = "Primary")
StudyData$Sector <- recode(StudyData$Sector, "Primary/Health/Counselling/Voluntary" = "Secondary")
StudyData$Sector <- recode(StudyData$Sector, "Inpatient/DayHosp/PartialHospital" = "Residential")
StudyData$Sector <- recode(StudyData$Sector, "Uni Training Clinic/Uni Treatment Centre" = "Uni. Clinics")
StudyData$TherapyCategory <- recode(StudyData$TherapyCategory, "BT/CT/CBT" = "CBT")
StudyData$TherapyCategory <- recode(StudyData$TherapyCategory, "PDT" = "Dynamic")
StudyData$TherapyCategory <- recode(StudyData$TherapyCategory, "Multiple/NotSpecific/Other" = "Other")
StudyData$Continent <- recode(StudyData$Continent, "North America" = "N.America")


StudyData$Citations <- paste("", StudyData$Citation, " ", StudyData$Year, "", sep = "")

QualityData <- readxl::read_excel("Input/Extraction_Full.xlsx", sheet = "Quality", skip = 1)
QualityData$QualityScore <- as.numeric(QualityData$QualityScore)

TableData <- merge(StudyData, QualityData, by = "Key")

TableData <- TableData %>% select(Citations, Country, Service, DemographicN, Therapy, Sector, ITT, PrimaryProblem, QualityScore)


kable(TableData, booktabs = TRUE, escape = T, align = "l",
      longtable = TRUE,
      caption = "Table 1",
      col.names = c("Study", "Country", "Service", "N", "Treatment", "Sector", "ITT\nmethod", "Primary\nproblem", "Quality\nscore")) %>%
  kable_styling(full_width = T, font_size = 10) %>%
  column_spec(1, width = "8em")%>%
  column_spec(2, width = "8em")%>%
  column_spec(3, width = "15em")%>%
  column_spec(4, width = "5em")%>%
  column_spec(5, width = "3em")%>%
  column_spec(6, width = "5em")%>%
  column_spec(7, width = "2em")%>%
  column_spec(8, width = "5em")




