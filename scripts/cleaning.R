# Data Cleaning
  ## 1. Depression ####
    ### Import
  dat <- list(
          dep = read_excel("Input/extraction.xlsx", sheet = "Depression", skip = 1),
          anx = read_excel("Input/extraction.xlsx", sheet = "Anxiety", skip = 1),
          gen = read_excel("Input/extraction.xlsx", sheet = "General", skip = 1)
            )

  dat$dep <- dat$dep %>% rename(measure.used = Dep.Measure.Used,
                                measure.group = Dep.Measure.Group,
                                measure.type = Dep.Measure.Type)
  dat$anx <- dat$anx %>% rename(measure.used = Anx.Measure.Used,
                                measure.group = Anx.Measure.Group,
                                measure.type = Anx.Measure.Type)
  dat$gen <- dat$gen %>% rename(measure.used = Gen.Measure.Used,
                                measure.group = Gen.Measure.Group,
                                measure.type = Gen.Measure.Type)

  dat <- lapply(dat, function(x) x%>% select(
            "Key", "Year", "measure.used", "measure.type", "measure.group",
            "PreN", "PreMean", "PreSD", "PostN", "PostMean", "PostSD", "EffectSize",
            "R", "ESrequired", "ESproceadure", "Sample.Variance", "Key2"
              ))

  dat <- lapply(dat, function(x) x%>% filter(measure.used == "Yes"))

  dat <- lapply(dat, function(x) x%>% mutate(
                        PreMean = as.numeric(PreMean),
                        PostMean = as.numeric(PostMean),
                        PreSD = as.numeric(PreSD),
                        PreN = as.numeric(PreN),
                        PostN = as.numeric(PostN),
                        R = as.numeric(R),
                        EffectSize = as.numeric(EffectSize),
                        ESproceadure = as.numeric(ESproceadure),
                        Sample.Variance = as.numeric(Sample.Variance),
                        PostN = coalesce(PostN,PreN)
                        )) %>%
                        suppressWarnings()

  dat$dep$R[is.na(dat$dep$R)] <- .6
  dat$anx$R[is.na(dat$anx$R)] <- .6
  dat$gen$R[is.na(dat$gen$R)] <- .6


    dat <- lapply(dat, function(x) x%>% group_by(ESrequired) %>%
     group_split(dat, .keep = TRUE) )%>%
      suppressWarnings()

    names(dat$dep) <- c("es.reported", "calc.needed")
    names(dat$anx) <- c("es.reported", "calc.needed")
    names(dat$gen) <- c("es.reported", "calc.needed")


    dat$dep$calc.needed <- escalc("SMCR", ni = PreN, n2i = PostN,
                                  m1i = PreMean, m2i = PostMean, ri = R, sd1i = PreSD, slab = Key, var.names =
                                    c("EffectSize", "Sample.Variance"),data = dat$dep$calc.needed)
    dat$anx$calc.needed <- escalc("SMCR", ni = PreN, n2i = PostN,
                                  m1i = PreMean, m2i = PostMean, ri = R, sd1i = PreSD, slab = Key, var.names =
                                    c("EffectSize", "Sample.Variance"),data = dat$anx$calc.needed)
    dat$gen$calc.needed <- escalc("SMCR", ni = PreN, n2i = PostN,
                                  m1i = PreMean, m2i = PostMean, ri = R, sd1i = PreSD, slab = Key, var.names =
                                    c("EffectSize", "Sample.Variance"),data = dat$gen$calc.needed)

dat$dep <- full_join(dat$dep$es.reported, dat$dep$calc.needed)
dat$anx <- full_join(dat$anx$es.reported, dat$anx$calc.needed)
dat$gen <- full_join(dat$gen$es.reported, dat$gen$calc.needed)

dat$gen$EffectSize <- abs(dat$gen$EffectSize)
dat$gen <- dat$gen %>%
  mutate(EffectSize = case_when(
    Key == "Lunnen2008" ~ -0.03706072,
    Key == "Harte2016(OCD+PD)" ~ -0.57353435, TRUE ~ EffectSize))




  ## 4. Quality ####
  ### Import
        qual <- read_excel("Input/extraction.xlsx", sheet = "Quality", skip = 1)
        qual <- qual %>% select(Key, QualityScore, CriteriaCal, ConsecCalc, CompleyeCalc,
                                DemographCalc, InterventionCalc, OutcomeCalc, ClinicCalc, AnalysisCalc) %>%
                                mutate(across(c(QualityScore:AnalysisCalc), as.numeric)) %>% suppressWarnings()




slr <- list( # This list Is a big size because of the number of columns-investigate the impact of dropping redundant columns.
  phase.1 = read_excel("Input/ft-screening.xlsx", sheet = "Databases", range = cell_cols(1:9)),
  phase.2 = read_excel("Input/ft-screening.xlsx", sheet = "ForwardBackward", range = cell_cols(1:7)),
  phase.3 = read_excel("Input/ft-screening.xlsx", sheet = "Grey", range = cell_cols(1:6))
)

slr <- lapply(slr, function(x) x%>% mutate(Decision = as.factor(Decision),
                                           ExclusionReason = as.factor(ExclusionReason)))

slr$ms.requests <- read_excel("Input/ft-screening.xlsx", sheet = "ms-requests")
slr$phase.1 <- slr$phase.1 %>% select(-c(Volume, Pages)) %>% mutate(Tab = "phase.1")
slr$phase.2 <- slr$phase.2 %>% mutate(Tab = "phase.2")
slr$phase.3 <- slr$phase.3 %>% mutate(Tab = "phase.3")
slr$combined <- full_join(slr$phase.1, slr$phase.2)
slr$combined <- full_join(slr$combined, slr$phase.3)

slr$dat.req <- list(dat = read_excel("Input/data-requests.xlsx") %>% select(-c(Authors, Title, Journal, Vol, Pages)))
slr$dat.req[["requests.all"]] = slr$dat.req$dat %>% group_by(InfoRequested) %>% count()
slr$dat.req[["requests.made"]] = slr$dat.req$dat %>% filter(InfoRequested == "Yes") %>% count()
slr$dat.req[["heard.back"]] = slr$dat.req$dat %>% filter(InfoRequested == "Yes") %>% filter(Response == "Yes") %>% count()
slr$dat.req[["corr"]] = slr$dat.req$dat %>% filter(InfoRequested == "Yes") %>% filter(What == "Correlation") %>% count()
slr$dat.req[["other"]] = slr$dat.req$dat %>% filter(InfoRequested == "Yes") %>% filter(What == "EffectSize") %>% count()
slr$dat.req[["data.provided"]] = slr$dat.req$dat %>% filter(InfoRequested == "Yes") %>% filter(Response == "Yes") %>% filter(Provided == "Yes")%>% count()



# Study Data ####
  ## Import Data ####

dat.mod <- read_excel("Input/extraction.xlsx", sheet = "StudyData", skip = 1)
dat.mod <- dat.mod %>% select(
          Key, Year, StudySampleN, For.Meta,
          Design, Implementation, HourGlass, Country, Continent, Service, Setting, Sector, PaymentProxy,
          ReferralType, Starters, Completers, Dropouts, DropoutRate, ITT, ProblemCategory,
          DiagnosticTool, SessionsAvg, DosageMetric, SamplesRepresented,
          Minority.perc, Employed.perc, Married.perc,TrainingExclusive,
          TreatmentPackage, DosageMetric, IncludesUnqualifieds, TrainingExclusive, TherapyCategory, ExtractionLevel,
          DemographicN, FemaleN, Female.perc, AgeMean, Key2)
  ## Convert required data structures ####

dat.mod <-
      dat.mod %>% mutate(
                    DemographicN = as.numeric(DemographicN),
                    Female.perc = as.numeric(Female.perc),
                    FemaleN = as.numeric(FemaleN),
                    DemographicN = as.numeric(DemographicN),
                    Starters = as.numeric(Starters),
                    Completers = as.numeric(Completers),
                    Dropouts = as.numeric(Dropouts),
                    DropoutRate = as.numeric(DropoutRate),
                    ITT = as.factor(ITT),
                    AgeMean = as.numeric(AgeMean),
                    Minority.perc = as.numeric(Minority.perc),
                    Employed.perc = as.numeric(Employed.perc),
                    Married.perc = as.numeric(Married.perc),
                    SessionsAvg = as.numeric(SessionsAvg)
                          )

  ## Recode any required values ####
      dat.mod$Sector <- recode(dat.mod$Sector, "Secondary/Tertiary/Specialist/IOP/BehaviouralHealth" = "Secondary")
      dat.mod$Sector <- recode(dat.mod$Sector, "Primary/Health/Counselling/Voluntary" = "Primary")
      dat.mod$Sector <- recode(dat.mod$Sector, "Inpatient/DayHosp/PartialHospital" = "Residential")
      dat.mod$Sector <- recode(dat.mod$Sector, "Uni Training Clinic/Uni Treatment Centre" = "Uni. Clinics")
      dat.mod$TherapyCategory <- recode(dat.mod$TherapyCategory, "BT/CT/CBT" = "CBT")
      dat.mod$TherapyCategory <- recode(dat.mod$TherapyCategory, "PDT" = "Dynamic")
      dat.mod$TherapyCategory <- recode(dat.mod$TherapyCategory, "Multiple/NotSpecific/Other" = "Other")
      dat.mod$Continent <- recode(dat.mod$Continent, "North America" = "N.America")

      dat.mod <- inner_join(dat.mod, qual, by = "Key")

      dat.mod$Sector <- plyr::revalue(dat.mod$Sector, c("EAP/OH" = "Primary", "Private" = "Primary",
                                      "Veterans" = "Primary", "University Counselling" = "Primary",
                                      "Check with team" = "Other", "Mixed" = "Other")
                                      )
      SectorFreq <- freq(dat.mod$Sector)
      SectorFreq <- SectorFreq[,2]

      dat.mod$ITT2 <- dat.mod$ITT
      dat.mod$ITT <- plyr::revalue(dat.mod$ITT, c("Assume ITT" = "ITT", "Modified ITT" = "ITT"))
      dat.mod$TrainingExclusive <- plyr::revalue(dat.mod$TrainingExclusive, c("No" = "No/NA", "NA" = "No/NA"))
      dat.mod <- dat.mod %>% rename(Therapy = TherapyCategory)
