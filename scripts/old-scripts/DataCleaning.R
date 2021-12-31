# Data Cleaning
  ## 1. Depression ####
    ### Import
        Data <- read_excel("Input/Extraction_Full.xlsx", sheet = "Depression", skip = 1)
    ### Isolate required columns
        Data <- Data %>% select(Key, Citation, Year, Dep.Measure.Used, Dep.Measure.Group, Dep.Measure.Type, PreN,
                                PreMean, PreSD, PostN, PostMean, R, ESrequired,
                                ESproceadure, EffectSize, Sample.Variance)
    ### Isolate Anxiety Measures
        Data <- Data %>% filter(Dep.Measure.Used == "Yes")
    ### Re-format incorrect data structures
        Data$PreMean <- as.numeric(Data$PreMean)
        Data$PostMean <- as.numeric(Data$PostMean)
        Data$PreSD <- as.numeric(Data$PreSD)
        Data$PreN <- as.numeric(Data$PreN)
        Data$PostN <- as.numeric(Data$PostN)
        Data$R <- as.numeric(Data$R)
        Data$EffectSize <- as.numeric(Data$EffectSize)
        Data$ESproceadure <- as.numeric(Data$ESproceadure)
        Data$Sample.Variance <- as.numeric(Data$Sample.Variance)
    ### Split df
        Split <- Data %>% group_by(ESrequired)
        Split <- group_split(Split, .keep = TRUE)
        NoCalc = print(Split[[1]])
        Calc = print(Split[[2]])
    ### Use Coalesce to populate missing PostN values using the PreN
        Calc <- Calc %>% mutate(PostN = coalesce(PostN,PreN))
    ### Replace NAs with pre-defined value
        Calc$R[is.na(Calc$R)] <- .6
        Calc <- escalc(measure = "SMCR", ni = PreN, n2i = PostN,
                m1i = PreMean, m2i = PostMean, ri = R,
                sd1i = PreSD, slab = Key, var.names =
                c("EffectSize", "Sample.Variance"),data = Calc)
    ### Merge into complete depression outcomes df
        DepressionData <- full_join(Calc, NoCalc)
        remove(Calc, Data, NoCalc, Split)

DepressionData$Citation <- paste(DepressionData$Citation, DepressionData$Year, sep = ", ")



  ## 2. Anxiety ####
    ### Import
        Data <- read_excel("Input/Extraction_Full.xlsx", sheet = "Anxiety", skip = 1)
    ### Isolate required columns
        Data <- Data %>% select(Key, Citation, Year, Anx.Measure.Used, Anx.Measure.Group, Anx.Measure.Type, PreN,
                            PreMean, PreSD, PostN, PostMean, R, ESrequired,
                            ESproceadure, EffectSize, Sample.Variance)
    ### Isolate Anxiety Measures
        Data <- Data %>% filter(Anx.Measure.Used == "Yes")
    ### Re-format incorrect data structures
        Data$PreMean <- as.numeric(Data$PreMean)
        Data$PostMean <- as.numeric(Data$PostMean)
        Data$PreSD <- as.numeric(Data$PreSD)
        Data$PreN <- as.numeric(Data$PreN)
        Data$PostN <- as.numeric(Data$PostN)
        Data$R <- as.numeric(Data$R)
        Data$EffectSize <- as.numeric(Data$EffectSize)
        Data$ESproceadure <- as.numeric(Data$ESproceadure)
        Data$Sample.Variance <- as.numeric(Data$Sample.Variance)
    ### Split df
        Split <- Data %>% group_by(ESrequired)
        Split <- group_split(Split, .keep = TRUE)
        NoCalc = print(Split[[1]])
        Calc = print(Split[[2]])
    ### Use Coalesce to populate missing PostN values using the PreN
        Calc <- Calc %>% mutate(PostN = coalesce(PostN,PreN))
    ### Replace NAs with pre-defined value
        Calc$R[is.na(Calc$R)] <- .6
        Calc <- escalc(measure = "SMCR", ni = PreN, n2i = PostN,
                  m1i = PreMean, m2i = PostMean, ri = R,
                  sd1i = PreSD, slab = Key, var.names =
                  c("EffectSize", "Sample.Variance"),data = Calc)
    ### Merge into complete depression outcomes df
    AnxietyData <- full_join(Calc, NoCalc)
    remove(Calc, Data, NoCalc, Split)

    AnxietyData$Citation <- paste(AnxietyData$Citation, AnxietyData$Year, sep = ", ")


  ## 3. General ####
    ### Import
        Data <- read_excel("Input/Extraction_Full.xlsx", sheet = "General", skip = 1)
    ### Isolate required columns
        Data <- Data %>% select(Key, Year, Citation, Gen.Measure.Used, Gen.Measure.Group, Gen.Measure.Type,PreN,
                                PreMean, PreSD, PostN, PostMean,
                                R, EffectSize, ESrequired, ESproceadure, Sample.Variance)
    ### Isolate General Measures
        Data <- Data %>% filter(Gen.Measure.Used == "Yes")
    ### as integer or numeric
        Data$PreMean <- as.numeric(Data$PreMean)
        Data$PostMean <- as.numeric(Data$PostMean)
        Data$PreSD <- as.numeric(Data$PreSD)
        Data$PreN <- as.numeric(Data$PreN)
        Data$PostN <- as.numeric(Data$PostN)
        Data$R <- as.numeric(Data$R)
        Data$EffectSize <- as.numeric(Data$EffectSize)
        Data$ESproceadure <- as.numeric(Data$ESproceadure)
        Data$Sample.Variance <- as.numeric(Data$Sample.Variance)
    ### Split df
        Split <- Data %>%
            group_by(ESrequired)
        Split <- group_split(Split, .keep = TRUE)
        NoCalc = print(Split[[1]])
        Calc = print(Split[[2]])
    ### Use Coalesce to populate missing PostN values using the PreN
        Calc <- Calc %>%
          mutate(PostN = coalesce(PostN,PreN))
    ### Replace NAs with pre-defined value
        Calc$R[is.na(Calc$R)] <- .6
        Calc <- escalc(measure = "SMCR", ni = PreN, n2i = PostN,
                       m1i = PreMean, m2i = PostMean, ri = R,
                       sd1i = PreSD, slab = Key, var.names = c("EffectSize", "Sample.Variance"),
                       data = Calc)
    ### Merge DFs
        GeneralData <- full_join(Calc, NoCalc)
        remove(Calc, Data, NoCalc, Split)
    ### make negative values positive
        GeneralData$EffectSize <- abs(GeneralData$EffectSize)

    ## Change Lunnen2008 and Harte2016(OCD+PD) back to negative
        GeneralData <- GeneralData %>%
              mutate(EffectSize = case_when(
            Key == "Lunnen2008" ~ -0.03706072,
            Key == "Harte2016(OCD+PD)" ~ -0.57353435, TRUE ~ EffectSize))

  GeneralData$Citation <- paste(GeneralData$Citation, GeneralData$Year, sep = ", ")



  ## 4. Quality ####
  ### Import
        QualityData <- read_excel("Input/Extraction_Full.xlsx", sheet = "Quality", skip = 1)
  ### Isolate required columns
        QualityData <- QualityData %>% select(Key, QualityScore)
        QualityData$QualityScore <- as.numeric(QualityData$QualityScore)

DepressionData <- inner_join(DepressionData, QualityData)
AnxietyData <- inner_join(AnxietyData, QualityData)
GeneralData <- inner_join(GeneralData, QualityData)




# Full-Text Screening ####
  ## 1. Database Searching ####
      Databases <- read_excel("Input/ft-screening.xlsx", sheet = "Databases", range = cell_cols(1:9))
      Databases <- Databases %>% select(-c(Volume, Pages))
      Databases$Tab <- "Databases"
      Databases$Decision <- as.factor(Databases$Decision)
      Databases$ExclusionReason <- as.factor(Databases$ExclusionReason)
  ## 2. Reference Lists/Forward Citations ####
      ForwardBackward <- read_excel("Input/ft-screening.xlsx", sheet = "ForwardBackward", range = cell_cols(1:7))
      ForwardBackward$Tab <- "ForwardBackward"
      ForwardBackward$Decision <- as.factor(ForwardBackward$Decision)
      ForwardBackward$ExclusionReason <- as.factor(ForwardBackward$ExclusionReason)
  ## 3. Grey Literature ####
      Grey <- read_excel("Input/ft-screening.xlsx", sheet = "Grey", range = cell_cols(1:6))
      Grey$Tab <- "Grey"
      Grey$Decision <- as.factor(Grey$Decision)
      Grey$ExclusionReason <- as.factor(Grey$ExclusionReason)
  ## 4. Combine ####
    ### Merge different phase dataframes
      SLRcombined <- full_join(Databases, ForwardBackward)
      SLRcombined <- full_join(SLRcombined, Grey)
    ### Full Text Screening
      FullTextscreening <-  list(Databases, ForwardBackward, Grey)
      FullTextscreening <- setNames(object = FullTextscreening, nm=c("Databases", "ForwardBackward", "Grey"))
      remove(Databases, ForwardBackward, Grey)
# Study Data ####
  ## Import Data ####
      StudyData <- read_excel("Input/Extraction_Full.xlsx", sheet = "StudyData", skip = 1)
      StudyData <- StudyData %>% select(
          Key, Year, StudySampleN, For.Meta,
          Design, Implementation, HourGlass, Country, Continent, Service, Setting, Sector, PaymentProxy,
          ReferralType, Starters, Completers, Dropouts, DropoutRate, ITT, ProblemCategory,
          DiagnosticTool, SessionsAvg, DosageMetric, SamplesRepresented,
          Minority.perc, Employed.perc, Married.perc,TrainingExclusive,
          TreatmentPackage, DosageMetric, IncludesUnqualifieds, TrainingExclusive, TherapyCategory, ExtractionLevel,
          DemographicN, FemaleN, Female.perc, AgeMean)
  ## Convert required data structures ####
      StudyData$DemographicN <- as.numeric(StudyData$DemographicN)
      StudyData$Female.perc <- as.numeric(StudyData$Female.perc)
      StudyData$FemaleN <- as.numeric(StudyData$FemaleN)
      StudyData$DemographicN <- as.numeric(StudyData$DemographicN)
      StudyData$Starters <- as.numeric(StudyData$Starters)
      StudyData$Completers <- as.numeric(StudyData$Completers)
      StudyData$Dropouts <- as.numeric(StudyData$Dropouts)
      StudyData$DropoutRate <- as.numeric(StudyData$DropoutRate)
      StudyData$ITT <- as.factor(StudyData$ITT)
      StudyData$AgeMean <- as.numeric(StudyData$AgeMean)
      StudyData$Minority.perc <- as.numeric(StudyData$Minority.perc)
      StudyData$Employed.perc <- as.numeric(StudyData$Employed.perc)
      StudyData$Married.perc <- as.numeric(StudyData$Married.perc)
      StudyData$SessionsAvg <- as.numeric(StudyData$SessionsAvg)



  ## Recode any required values ####
      StudyData$Sector <- recode(StudyData$Sector, "Secondary/Tertiary/Specialist/IOP/BehaviouralHealth" = "Secondary")
      StudyData$Sector <- recode(StudyData$Sector, "Primary/Health/Counselling/Voluntary" = "Primary")
      StudyData$Sector <- recode(StudyData$Sector, "Inpatient/DayHosp/PartialHospital" = "Residential")
      StudyData$Sector <- recode(StudyData$Sector, "Uni Training Clinic/Uni Treatment Centre" = "Uni. Clinics")
      StudyData$TherapyCategory <- recode(StudyData$TherapyCategory, "BT/CT/CBT" = "CBT")
      StudyData$TherapyCategory <- recode(StudyData$TherapyCategory, "PDT" = "Dynamic")
      StudyData$TherapyCategory <- recode(StudyData$TherapyCategory, "Multiple/NotSpecific/Other" = "Other")
      StudyData$Continent <- recode(StudyData$Continent, "North America" = "N.America")

      StudyData <- inner_join(StudyData, QualityData)


# Export
write.csv(AnxietyData, "/Users/chris/Documents/thesis-meta-submission/psych-medicine/SuppMatt/AnxietyData.csv")
write.csv(DepressionData, "/Users/chris/Documents/thesis-meta-submission/psych-medicine/SuppMatt/DepressionData.csv")
write.csv(GeneralData, "/Users/chris/Documents/thesis-meta-submission/psych-medicine/SuppMatt/GeneralData.csv")
write.csv(StudyData, "/Users/chris/Documents/thesis-meta-submission/psych-medicine/SuppMatt/StudyData.csv")


