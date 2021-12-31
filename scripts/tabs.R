# 1. Effect size calculation procedure #####
tabs <- list(
  es.proc = data.frame(
    Steps = c("Step 1", "Step 2","Step 3", "Step 4", "Step 5"),
    Scenario = c("Manuscript reports all required information (N, M1, M2, SD1) for preferred d.",
                 "Manuscript reports all information apart from Pearson's r.",
                 "Manuscript does not report the mean or standard deviation but reports paired samples d.",
                 "Manuscript does not report mean, standard deviation, or paired samples d however reports alternative metrics
                                      (e.g. median, range, standard error, ANOVA, regression",
                 "All above steps attempted without success"),
    Response = c("Calculate preferred d.",
                 "E-mail corresponding authors to request r.",
                 "Use the reported d within the manuscript.",
                 "Estimate the meand and standard deviation by converting available metrics.",
                 "Study is not included in meta-analysis but is retained for narrative synthesis.")
              ))

# 2. Moderator descriptions #####
tabs$mod.desc = read_excel("Input/extraction.xlsx", sheet = "Coding", range = cell_cols(1:3))

# 3. SLR results #####
tabs$slr.res = as.data.frame(slr$combined %>% janitor::tabyl(Decision, Tab)) %>%
    adorn_totals(where = c("col", "row"))

# 4. Random effects meta results #####
tabs$rma.sum = data.frame(
      Variable  = c("Depression", "Anxiety","General"),
      k =         c(res$rma$dep$k, res$rma$anx$k, res$rma$gen$k),
      ES =        c(res$rma$dep$b, res$rma$anx$b, res$rma$gen$b),
      Lower =     c(res$rma$dep$ci.lb, res$rma$anx$ci.lb, res$rma$gen$ci.lb),
      Upper =     c(res$rma$dep$ci.ub, res$rma$anx$ci.ub, res$rma$gen$ci.ub),
      I2 =        c(res$rma$dep$I2, res$rma$anx$I2, res$rma$gen$I2),
      Q =         c(res$rma$dep$QE, res$rma$anx$QE, res$rma$gen$QE),
      p =         format.pval(c(res$rma$dep$pval, res$rma$anx$pval, res$rma$gen$pval), eps = .001)
                            )
tabs$rma.sum[,c(2:5,6:7)] <- round(tabs$rma.sum[,c(2:5,6:7)], 2)

# 5. Narrative review table #####
  ## categorical vars
tabs$narr <- list(setting = dat.mod %>% tabyl(Setting, Sector) %>%
                        mutate(mod = "Setting", .before = 1),
              continent = dat.mod %>% tabyl(Continent, Sector) %>%
                        mutate(mod = "Continent", .before = 1),
              completion = dat.mod %>% tabyl(ITT, Sector) %>%
                        mutate(mod = "Completion", .before = 1),
              therapy = dat.mod %>% tabyl(Therapy, Sector) %>%
                        mutate(mod = "Therapy", .before = 1),
              hourglass = dat.mod %>% tabyl(HourGlass, Sector) %>%
                        mutate(mod = "hourglass", .before = 1)
                  )
tabs$narr <-lapply(tabs$narr, function(x) x%>% rename(level = 2))
tabs$narr <- dplyr::bind_rows(tabs$narr) %>% filter(level != "Check") %>%
                  relocate("mod", "level", "Primary", "Residential", "Secondary",
                           "Uni. Clinics", "Other",) %>%
                  adorn_totals(where = c("col")) # Consider finding a way to include adorn percentages, percentage formats and ns,
  ## numerical vars
narr.num <- list(N = dat.mod %>% group_by(Sector) %>% tally(DemographicN) %>%
                        mutate(variable = "N"),
                 N2 = dat.mod %>% group_by(Sector) %>%
                   get_summary_stats(DemographicN, show = c('mean', 'median', 'iqr')) %>%
                   mutate(variable = "N"),
                female = dat.mod %>% group_by(Sector) %>% tally(FemaleN) %>%
                        mutate(variable = "Females"),
                age = dat.mod %>% group_by(Sector) %>% get_summary_stats(AgeMean,
                        show = c('n', 'mean', 'min', 'max')) %>% mutate(variable = "Age"),
                sessions = dat.mod %>% group_by(Sector) %>% filter(DosageMetric == "Sessions") %>%
                        get_summary_stats(SessionsAvg, show = c('n', 'mean', 'min', 'max', 'median', 'iqr')) %>%
                        mutate(variable = "Sessions")
                )
narr.num$N2 <-  narr.num$N2 %>% pivot_longer(cols = c(n, mean, median, iqr)) %>%
  pivot_wider(names_from = Sector) %>%
  mutate(name =  recode(name,  "n" ="samples"),
         Total = c(sum(!is.na(dat.mod$DemographicN)),
                   mean(dat.mod$DemographicN, na.rm = T),
                   median(dat.mod$DemographicN, na.rm = T),
                   IQR(dat.mod$DemographicN, na.rm = T))
  )
narr.num$age <- narr.num$age %>% pivot_longer(cols = c(n, mean, min, max)) %>%
                    pivot_wider(names_from = Sector) %>%
                    mutate(name =  recode(name,  "n" ="samples"),
                           Total = c(sum(!is.na(dat.mod$AgeMean)),
                                     mean(dat.mod$AgeMean, na.rm = T),
                                     min(dat.mod$AgeMean, na.rm = T),
                                     max(dat.mod$AgeMean, na.rm = T))
                           )
narr.num$sessions <- narr.num$sessions %>% pivot_longer(cols = c(n, mean, min, max, median, iqr)) %>% pivot_wider(names_from = Sector) %>%
                    mutate(name = recode(name,  "n" ="samples"),
                           Total = c(sum(!is.na(dat.mod %>% filter(DosageMetric == "Sessions") %>%
                                                  select(SessionsAvg))),
                                     as.numeric(dat.mod %>% filter(DosageMetric == "Sessions") %>%
                                                  select(SessionsAvg) %>% summarise(mean = mean(SessionsAvg, na.rm = T))),
                                     as.numeric(dat.mod %>% filter(DosageMetric == "Sessions") %>%
                                                  select(SessionsAvg) %>% summarise(mean = min(SessionsAvg, na.rm = T))),
                                     as.numeric(dat.mod %>% filter(DosageMetric == "Sessions") %>%
                                                  select(SessionsAvg) %>% summarise(mean = max(SessionsAvg, na.rm = T))),
                                     as.numeric(dat.mod %>% filter(DosageMetric == "Sessions") %>%
                                                  select(SessionsAvg) %>% summarise(mean = median(SessionsAvg, na.rm = T))),
                                     as.numeric(dat.mod %>% filter(DosageMetric == "Sessions") %>%
                                                  select(SessionsAvg) %>% summarise(mean = IQR(SessionsAvg, na.rm = T)))
                                     )
                            )

narr.num$N <- narr.num$N %>% pivot_longer(cols = c(n)) %>% pivot_wider(names_from = Sector) %>% mutate(Total = sum(dat.mod$DemographicN, na.rm = t))
narr.num$female <- narr.num$female %>% pivot_longer(cols = c(n)) %>% pivot_wider(names_from = Sector) %>% mutate(Total = sum(dat.mod$FemaleN, na.rm = t))

narr.num <- bind_rows(narr.num)
narr.num <- narr.num %>% rename(mod = variable, level = name) %>% relocate("mod", "level", "Uni. Clinics", "Primary",
                                                                           "Residential", "Secondary", "Other", "Total")
tabs$narr <- rbind(narr.num, tabs$narr)
remove(narr.num)

# 6. Prisma table #####
tabs$prisma <- list(
  found_other = slr$combined %>% filter(Tab == "phase.2" |Tab == "phase.3") %>% nrow(),
  no_dupes = slr$combined %>% filter(Tab == "phase.2" |Tab == "phase.3") %>% nrow()+8709,
  full_text = slr$combined %>% nrow(),
  screen_exclusions = (slr$combined %>% filter(Tab == "phase.2" |Tab == "phase.3") %>% nrow()+8709)-slr$combined %>% nrow(),
  full_text_exclusions = (slr$combined %>% nrow()-dat.mod %>% filter(StudySampleN == 1) %>% nrow()),
  qualitative = dat.mod %>% filter(StudySampleN == 1) %>% nrow(),
  quantitative = dat.mod %>% filter(StudySampleN == 1 & For.Meta == "Yes") %>% nrow()
)



