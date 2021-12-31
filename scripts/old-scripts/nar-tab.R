source("Scripts/DataCleaning.R")

# Clean
StudyData$ITT <- plyr::revalue(StudyData$ITT, c("Assume ITT" = "ITT", "Modified ITT" = "ITT"))
StudyData$SessionsAvg <- as.numeric(StudyData$SessionsAvg)


StudyData$Sector <- plyr::revalue(StudyData$Sector, c("EAP/OH" = "Primary",
                                                      "Private" = "Primary",
                                                      "Veterans" = "Primary",
                                                      "University Counselling" = "Primary",
                                                      "Check with team" = "Other",
                                                      "Mixed" = "Other"
                                                      ))



StudyData$TrainingExclusive <- plyr::revalue(StudyData$TrainingExclusive, c("No" = "No/NA",
                                                                            "NA" = "No/NA"))
StudyData <- StudyData %>% rename(Therapy = TherapyCategory)

#```{r NarrativeTable, echo=FALSE}
# Frequency/Count Table ####
## Sector Data
NarrativeTableData <- StudyData %>% filter(
  Sector == "Primary" | Sector == "Secondary" | Sector == "Residential" |
    Sector == "Private" | Sector ==  "Uni. Clinics" | Sector ==  "Other")

# Setting
NarTableSetting <- NarrativeTableData %>% tabyl(Setting, Sector) %>% adorn_totals(where = c("col")) %>%  as.data.frame()
NarTableSetting <- NarTableSetting %>% rename(Group = Setting)
NarTableSetting$Variable <- 'Setting'
# Continent
NarTableContinent <- NarrativeTableData %>% tabyl(Continent, Sector) %>% adorn_totals(where = c("row","col")) %>%
  as.data.frame()
NarTableContinent <- NarTableContinent %>% rename(Group = Continent)
NarTableContinent$Variable <- 'Continent'
# Completion
NarTableITT <- NarrativeTableData %>% tabyl(ITT, Sector) %>% adorn_totals(where = c("col")) %>%
  as.data.frame()
NarTableITT  <- NarTableITT  %>% rename(Group = ITT)
NarTableITT $Variable <- 'Completion'
# Therapy Category
NarTableTXcategory <- NarrativeTableData %>% tabyl(Therapy, Sector) %>% adorn_totals(where = c("col")) %>%
   as.data.frame()
NarTableTXcategory <- NarTableTXcategory %>% rename(Group = Therapy)
NarTableTXcategory$Variable <- 'Therapy'
# Hour-Glass Category
NarTableHourGlass <- NarrativeTableData %>% tabyl(HourGlass, Sector) %>% adorn_totals(where = c("col")) %>%
  as.data.frame()
NarTableHourGlass <- NarTableHourGlass %>% rename(Group = HourGlass)
NarTableHourGlass$Variable <- 'Hour Glass'
# Merge
NarrativeTableFactors <- bind_rows(NarTableSetting, NarTableITT, NarTableTXcategory, NarTableHourGlass, NarTableContinent)
NarrativeTableFactors <- NarrativeTableFactors %>%
  select(Variable, everything())
remove(NarTableSetting, NarTableITT, NarTableTXcategory, NarTableContinent, NarTableHourGlass)

NarrativeTableFactors <- NarrativeTableFactors %>% relocate(Variable, Group, `Uni. Clinics`, Primary, Secondary, Residential, Other, Total)

# Table
kable(NarrativeTableFactors, booktabs = TRUE, escape = FALSE, longtable = TRUE,caption = "Moderator Analyses")  %>%
  kable_styling(full_width = FALSE) %>%
  collapse_rows(columns = 1, valign = "top")



NarrativeTableFactors  <- as.data.frame(NarrativeTableFactors)

### More Factors for Factor Table = Retrospective, Hour Glass, Referral Type,
    ### Diagnoses, Diagnostic Tool, Therapy, Training Samples

# Integer Table ####
## Age
  ## Summ Stats
        NarTableAge <- NarrativeTableData %>% group_by(Sector) %>% get_summary_stats(AgeMean, type = c('full'))
        NarTableAge <- NarTableAge %>% select(Sector, variable, n, mean, sd, min, max)
        NarTableAge <-  as.data.frame(adorn_rounding(NarTableAge, digits = 1))
        ## Flip cols/rows
        NarTableAge <- NarTableAge %>% rename(samples = n)
        NarTableAge2 <-  data.frame(t(NarTableAge))
        colnames(NarTableAge2) <- NarTableAge[, 1]
  ## Clean
        NarTableAge <-  NarTableAge2[3:7,]
        NarTableAge$Variable <- 'Age'
        remove(NarTableAge2)

  ## Females
        NarrativeTableFemaleN <- NarrativeTableData %>% group_by(Sector) %>% tally(FemaleN)
        NarrativeTableFemaleN <-  NarrativeTableFemaleN %>% rename(Female = n)
  ## Females
        ## Flip cols/rows
        NarrativeTableFemaleN2 <-  data.frame(t(NarrativeTableFemaleN))
        colnames(NarrativeTableFemaleN2) <- c("Other", "Primary", "Residential", "Secondary", "Uni. Clinics")
 ## Clean
        NarrativeTableFemaleN <-  NarrativeTableFemaleN2[-1,]
        NarrativeTableFemaleN$Variable <- 'N'
        remove(NarrativeTableFemaleN2)

## TotalN
        DemographicN <- NarrativeTableData %>% group_by(Sector) %>% tally(DemographicN)
        DemographicN <-  DemographicN %>% rename(Total = n)
## Females
## Flip cols/rows
        DemographicN2 <-  data.frame(t(DemographicN))
        colnames(DemographicN2) <- c("Other", "Primary", "Residential", "Secondary", "Uni. Clinics")
## Clean
        DemographicN <-  DemographicN2[-1,]
        DemographicN$Variable <- 'N'
        remove(DemographicN2)



## Sessions
    Sessions <- NarrativeTableData %>% filter(DosageMetric == "Sessions")

        ## Summ Stats
    SessionsStats <- Sessions %>% group_by(Sector) %>% get_summary_stats(SessionsAvg, type = c('full'))
    SessionsStats <- SessionsStats %>% select(Sector, variable, n, mean, sd, min, max)
    SessionsStats <-  as.data.frame(adorn_rounding(SessionsStats, digits = 1))
        ## Flip cols/rows
    SessionsStats <- SessionsStats %>% rename(samples = n)
    SessionsStats2 <-  data.frame(t(SessionsStats))
        colnames(SessionsStats2) <- c("Other", "Primary", "Residential", "Secondary", "Uni. Clinics")
        ## Clean
        SessionsStats <-  SessionsStats2[3:7,]
        SessionsStats$Variable <- 'Sessions'
        remove(SessionsStats2)




        ## minority
        Minorities <- NarrativeTableData %>% filter(DosageMetric == "Sessions")

        ## Summ Stats
        Minorities <- NarrativeTableData %>% group_by(Sector) %>% get_summary_stats(Minority.perc, type = c('full'))
        Minorities <- Minorities %>% select(Sector, variable, n, mean, sd, min, max)
        Minorities <-  as.data.frame(adorn_rounding(Minorities, digits = 1))
        ## Flip cols/rows
        Minorities <- Minorities %>% rename(samples = n)
        Minorities2 <-  data.frame(t(Minorities))
        colnames(Minorities2) <- c("Other", "Primary", "Residential", "Secondary", "Uni. Clinics")
        ## Clean
        Minorities <-  Minorities2[3:7,]
        Minorities$Variable <- 'Minorities'
        remove(Minorities2)








# Table
  # Merge
    NarTableAge <-  cbind(rownames(NarTableAge), data.frame(NarTableAge, row.names=NULL))
         names(NarTableAge)[1] <- "Descriptive"
    NarrativeTableFemaleN <-  cbind(rownames(NarrativeTableFemaleN), data.frame(NarrativeTableFemaleN))
        names(NarrativeTableFemaleN)[1] <- "Descriptive"
    DemographicN <- cbind(rownames(DemographicN), data.frame(DemographicN, row.names=NULL))
      names(DemographicN)[1] <- "Descriptive"
    SessionsStats <- cbind(rownames(SessionsStats), data.frame(SessionsStats, row.names=NULL))
      names(SessionsStats)[1] <- "Descriptive"
    Minorities <- cbind(rownames(Minorities), data.frame(Minorities, row.names=NULL))
      names(Minorities)[1] <- "Descriptive"

      NarrativeTableIntegers <- full_join(NarrativeTableFemaleN, DemographicN)
      NarrativeTableIntegers <- full_join(NarrativeTableIntegers, NarTableAge)
      NarrativeTableIntegers <- full_join(NarrativeTableIntegers, SessionsStats)
      NarrativeTableIntegers <- full_join(NarrativeTableIntegers, Minorities)
  # Format Columns

    NarrativeTableIntegers <- NarrativeTableIntegers %>%
           select(Variable, everything())
  # Produce
    kable(NarrativeTableIntegers, booktabs = TRUE, escape = FALSE,
          longtable = TRUE,caption = "Moderator Analyses")  %>%
          kable_styling(full_width = FALSE) %>%
          collapse_rows(columns = 1, valign = "top")

    NarrativeTableIntegers <- NarrativeTableIntegers %>%
      relocate(Variable, Descriptive, `Uni..Clinics`, Primary, Secondary, Residential, Other)
      NarrativeTableIntegers <- NarrativeTableIntegers %>% rename('Uni. Clinics' = Uni..Clinics)

      NarrativeTableIntegers$`Uni. Clinics` <- as.numeric(NarrativeTableIntegers$`Uni. Clinics`)
      NarrativeTableIntegers$Other <- as.numeric(NarrativeTableIntegers$Other)
      NarrativeTableIntegers$Primary <- as.numeric(NarrativeTableIntegers$Primary)
      NarrativeTableIntegers$Secondary <- as.numeric(NarrativeTableIntegers$Secondary)
      NarrativeTableIntegers$Residential <- as.numeric(NarrativeTableIntegers$Residential)

      NarrativeTableIntegers <- NarrativeTableIntegers %>% adorn_totals(where = "col")
      NarrativeTableIntegers <- NarrativeTableIntegers %>% rename('Group' = 'Descriptive')

  NarrativeTable <- rbind(NarrativeTableIntegers, NarrativeTableFactors)


write.csv(NarrativeTable, "NarrativeTable.csv")
#```
