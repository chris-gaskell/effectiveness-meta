# Data Cleaning


## 3. General ####
### Import
Data <- read_excel("Input/Extraction_Full.xlsx", sheet = "General", skip = 1)
### Isolate required columns
Data <- Data %>% select(Key, Year, Gen.Measure.Used, Gen.Measure.Group, PreN,
                        PreMean, PreSD, PostN, PostMean, PostSD,
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




## Isolate Studies which used the OQ-45 for benchmarking purposes.
OQStudies <- GeneralData %>% filter(Gen.Measure.Group == "Outcome Questionnaire (OQ 30/45/45.2)")
OQStudies <- OQStudies %>% filter(Key != "Minami2008") %>%
            select(Key, PreN, PreMean, PreSD, PostN, PostMean, PostSD, R, EffectSize, Sample.Variance) %>%
            rename(group = Key, ni = PreN, n2i = PostN, m1i = PreMean, m2i = PostMean, ri = R,
                   sd1i = PreSD, sd2i = PostSD, yi = EffectSize, vi = Sample.Variance)






#OQStudies.rma <- metafor::rma(EffectSize, Sample.Variance, data = OQStudies, slab = Key)
#General.rma <- metafor::rma(EffectSize, Sample.Variance, data = GeneralData)



#write.csv(OQbenchmarks, "/Users/chris/Desktop/Empirical/R_Empirical/Input/OQbenchmarks.csv")
write.csv(OQStudies, "/Users/chris/Desktop/Empirical/R_Empirical/Input/OQbenchmarks2.csv")

GeneralStudyData <- read_excel("Input/Extraction_Full.xlsx", sheet = "StudyData", skip = 1)

GeneralStudyData <- GeneralStudyData %>% filter(
        Key == "Baldwin2009" |
        Key == "Bradshaw2009" |
        Key == "Galili-Weinstock2018" |
        Key == "Haugen2017" |
        Key == "Kaplinski2014" |
        Key == "Kolly2015" |
        Key == "Kramer2013" |
        Key == "Lunnen2008" |
        Key == "Minami2008" |
        Key == "Prout2016" |
        Key == "Ronnestad2019"|
          Key == "Wiseman2014"|
          Key == "Goldberg2016"|
          Key == "Roseborough2006"
)

write.csv(GeneralStudyData, "/Users/chris/Desktop/Empirical/R_Empirical/Input/OQbenchmarks3.csv")

