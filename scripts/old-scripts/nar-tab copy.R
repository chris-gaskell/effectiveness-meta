tabs$narr <- list(
    setting = dat.mod %>% tabyl(Setting, Sector) %>% mutate(mod = "Setting", .before = 1),
    continent = dat.mod %>% tabyl(Continent, Sector) %>% mutate(mod = "Continent", .before = 1),
    completion = dat.mod %>% tabyl(ITT, Sector) %>% mutate(mod = "Completion", .before = 1),
    therapy = dat.mod %>% tabyl(Therapy, Sector) %>% mutate(mod = "Therapy", .before = 1),
    hourglass = dat.mod %>% tabyl(HourGlass, Sector) %>% mutate(mod = "hourglass", .before = 1)
                  )
tabs$narr <-  lapply(tabs$narr, function(x) x%>% rename(level = 2))
tabs$narr <-  dplyr::bind_rows(tabs$narr) %>% filter(level != "Check") %>%
                  relocate("mod", "level", "Primary", "Residential", "Secondary",
                  "Uni. Clinics", "Other",) %>% adorn_totals(where = c("col"))
Test <- list(
  age = dat.mod %>% group_by(Sector) %>% get_summary_stats(AgeMean,
                show = c('n', 'mean', 'min', 'max')) %>% mutate(variable = "Age"),
  sessions = dat.mod %>% group_by(Sector) %>% filter(DosageMetric == "Sessions") %>%
                get_summary_stats(SessionsAvg, show = c('n', 'mean', 'min', 'max')) %>%
                mutate(variable = "Sessions"),
  N = dat.mod %>% group_by(Sector) %>% tally(DemographicN) %>% mutate(variable = "N"),
  female = dat.mod %>% group_by(Sector) %>% tally(FemaleN) %>% mutate(variable = "Females")
            )

Test$age <- Test$age %>% pivot_longer(cols = c(n, mean, min, max)) %>% pivot_wider(names_from = Sector)
Test$sessions <- Test$sessions %>% pivot_longer(cols = c(n, mean, min, max)) %>% pivot_wider(names_from = Sector)
Test$N <- Test$N %>% pivot_longer(cols = c(n)) %>% pivot_wider(names_from = Sector)
Test$female <- Test$female %>% pivot_longer(cols = c(n)) %>% pivot_wider(names_from = Sector)

Test <- bind_rows(Test)
Test <- Test %>% adorn_totals(where = "col") %>% rename(mod = variable, level = name)
Test <- rbind(Test, tabs$narr)
