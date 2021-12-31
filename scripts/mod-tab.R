### data #####
dat <- lapply(dat, function(x) x%>% arrange(Key))
dat.mod <- dat.mod %>% arrange(Key)
dat.comb <- lapply(dat, function(x) x%>% inner_join(dat.mod, by = "Key"))
dat.comb <- lapply(dat.comb, function(x) x%>% mutate(sample.n.group = ifelse(PreN %in% 0:24, "small",
                                                                 ifelse(PreN %in% 25:49, "medium","large"))))

### 1.1 RMA without aggregation  ####

### 1.2 RMA without aggregation for moderator vars ####
mod.res <- list( ## The random effect metal analysis using this package is different to the previous-possibly because of the SMD and not SMCR.
  prim = list(
    anx = metagen(EffectSize, Sample.Variance, data=dat.comb$anx, studlab=paste(Key), random = TRUE, prediction=TRUE, sm="SMD"),
    dep = metagen(EffectSize, Sample.Variance, data=dat.comb$dep, studlab=paste(Key), random = TRUE, prediction=TRUE, sm="SMD"),
    gen = metagen(EffectSize, Sample.Variance, data=dat.comb$gen, studlab=paste(Key), random = TRUE, prediction=TRUE, sm="SMD")
  ))



### 1.2. Aggregate samples for studies ####



# Subgroup (categorical) moderators

### 2. Moderators ####
   #### 1. Intention-To-Treat ####
mod.res$ITT <- list(out = lapply(mod.res$prim, function(x) x%>% update.meta(subgroup = ITT, random = TRUE, fixed = FALSE)))

mod.res$ITT$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
        anx = data.frame(mod.res$ITT$out$anx[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
        dep = data.frame(mod.res$ITT$out$dep[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
        gen = data.frame(mod.res$ITT$out$gen[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")])
                        )
mod.res$ITT$tab <- lapply(mod.res$ITT$tab, function(x) x %>%
                        mutate_if(is.numeric, round, 2) %>%
                        mutate(ci = paste(upper.random.w, lower.random.w, sep = "-"),
                               mod = "completion") %>%
                        select(-c(upper.random.w, lower.random.w)) %>%
                        rename(Level = bylevs, K = k.w, SMD = TE.random.w, Q = Q.w, I2 = I2.w))
mod.res$ITT$sum <- list(
      dep = paste("Random effects model for analysis (Q = ", round(mod.res$ITT$out$dep$Q.b.random, 2), ", p = ",
            format.pval(mod.res$ITT$out$dep$pval.Q.b.random, eps = .001, digits = 2),")", sep = ""),
      anx = paste("Random effects model for analysis (Q = ", round(mod.res$ITT$out$anx$Q.b.random, 2), ", p = ",
        round(mod.res$ITT$out$anx$pval.Q.b.random, 3),")", sep = ""),
      gen = paste("Random effects model for analysis (Q = ", round(mod.res$ITT$out$gen$Q.b.random, 2), ", p = ",
        round(mod.res$ITT$out$gen$pval.Q.b.random, 3),")", sep = "")
                        )
   #### 2. Sector ####
mod.res$sector <- list(out = lapply(mod.res$prim, function(x) x%>% update.meta(subgroup = Sector, random = TRUE, fixed = FALSE, exclude = Sector == "Other")))

mod.res$sector$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
  anx = data.frame(mod.res$sector$out$anx[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  dep = data.frame(mod.res$sector$out$dep[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  gen = data.frame(mod.res$sector$out$gen[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")])
                          )
mod.res$sector$tab <- lapply(mod.res$sector$tab, function(x) x %>%
                            mutate_if(is.numeric, round, 2) %>%
                              mutate(ci = paste(upper.random.w, lower.random.w, sep = "-"),
                                     mod = "sector") %>%
                            select(-c(upper.random.w, lower.random.w)) %>%
                            rename(Level = bylevs, K = k.w, SMD = TE.random.w, Q = Q.w, I2 = I2.w))
mod.res$sector$sum <- list(
  dep = paste("Random effects model for severity (Q = ", round(mod.res$sector$out$dep$Q.b.random, 2), ", p = ",
              format.pval(mod.res$sector$out$dep$pval.Q.b.random, eps = .001, digits = 3),")", sep = ""),
  anx = paste("Random effects model for severity (Q = ", round(mod.res$sector$out$anx$Q.b.random, 2), ", p = ",
              round(mod.res$sector$out$anx$pval.Q.b.random, 3),"*)", sep = ""),
  gen = paste("Random effects model for severity (Q = ", round(mod.res$sector$out$gen$Q.b.random, 2), ", p = ",
              round(mod.res$sector$out$gen$pval.Q.b.random, 3),"*)", sep = "")
                          )

   #### 3. Setting ####
mod.res$setting <- list(out = lapply(mod.res$prim, function(x) x%>% update.meta(subgroup = Setting, random = TRUE, fixed = FALSE, exclude = Setting == "Mixed")))

mod.res$setting$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
  anx = data.frame(mod.res$setting$out$anx[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  dep = data.frame(mod.res$setting$out$dep[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  gen = data.frame(mod.res$setting$out$gen[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")])
                          )
mod.res$setting$tab <- lapply(mod.res$setting$tab, function(x) x %>%
                            mutate_if(is.numeric, round, 2) %>%
                              mutate(ci = paste(upper.random.w, lower.random.w, sep = "-"),
                                     mod = "setting") %>%
                            select(-c(upper.random.w, lower.random.w)) %>%
                            rename(Level = bylevs, K = k.w, SMD = TE.random.w, Q = Q.w, I2 = I2.w))
mod.res$setting$sum <- list(
  dep =  paste("Random effects model for setting (Q = ", round(mod.res$setting$out$dep$Q.b.random, 2), ", p = ",
               round(mod.res$setting$out$dep$pval.Q.b.random, 3), ")", sep = ""),
  anx = paste("Random effects model for setting (Q = ", round(mod.res$setting$out$anx$Q.b.random, 2), ", p = ",
              round(mod.res$setting$out$anx$pval.Q.b.random, 3), "*)", sep = ""),
  gen = paste("Random effects model for setting (Q = ", round(mod.res$setting$out$gen$Q.b.random, 2), ", p = ",
              round(mod.res$setting$out$gen$pval.Q.b.random, 3), ")",sep = "")
                          )


   #### 4. Continent ####
mod.res$cont <- list(out = lapply(mod.res$prim, function(x) x%>% update.meta(subgroup = Continent, random = TRUE, fixed = FALSE)))

mod.res$cont$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
  anx = data.frame(mod.res$cont$out$anx[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  dep = data.frame(mod.res$cont$out$dep[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  gen = data.frame(mod.res$cont$out$gen[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")])
)
mod.res$cont$tab <- lapply(mod.res$cont$tab, function(x) x %>%
                                mutate_if(is.numeric, round, 2) %>%
                                mutate(ci = paste(upper.random.w, lower.random.w, sep = "-"),
                                       mod = "continent") %>%
                                select(-c(upper.random.w, lower.random.w)) %>%
                                rename(Level = bylevs, K = k.w, SMD = TE.random.w, Q = Q.w, I2 = I2.w))
mod.res$cont$sum <- list(
  dep =  paste("Random effects model for continent (Q = ", round(mod.res$cont$out$dep$Q.b.random, 2), ", p = ",
               round(mod.res$cont$out$dep$pval.Q.b.random, 3), "*)", sep = ""),
  anx = paste("Random effects model for continent (Q = ", round(mod.res$cont$out$anx$Q.b.random, 2), ", p = ",
              round(mod.res$cont$out$anx$pval.Q.b.random, 3), "*)", sep = ""),
  gen = paste("Random effects model for continent (Q = ", round(mod.res$cont$out$gen$Q.b.random, 2), ", p = ",
              round(mod.res$cont$out$gen$pval.Q.b.random, 3), "*)",sep = "")
)


   #### 5. Therapy Category ####
mod.res$therapy <- list(out = lapply(mod.res$prim, function(x) x%>% update.meta(subgroup = Therapy, random = TRUE, fixed = FALSE)))

mod.res$therapy$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
  anx = data.frame(mod.res$therapy$out$anx[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  dep = data.frame(mod.res$therapy$out$dep[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  gen = data.frame(mod.res$therapy$out$gen[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")])
)
mod.res$therapy$tab <- lapply(mod.res$therapy$tab, function(x) x %>%
                             mutate_if(is.numeric, round, 2) %>%
                             mutate(ci = paste(upper.random.w, lower.random.w, sep = "-"),
                                    mod = "therapy") %>%
                             select(-c(upper.random.w, lower.random.w)) %>%
                             rename(Level = bylevs, K = k.w, SMD = TE.random.w, Q = Q.w, I2 = I2.w))
mod.res$therapy$sum <- list(
  dep =  paste("Random effects model for therapy modality (Q = ", round(mod.res$therapy$out$dep$Q.b.random, 2), ", p = ",
               round(mod.res$therapy$out$dep$pval.Q.b.random, 3), ")", sep = ""),
  anx = paste("Random effects model for therapy modality  (Q = ", round(mod.res$therapy$out$anx$Q.b.random, 2), ", p = ",
              round(mod.res$therapy$out$anx$pval.Q.b.random, 3), "*)", sep = ""),
  gen = paste("Random effects model for therapy modality  (Q = ", round(mod.res$therapy$out$gen$Q.b.random, 2), ", p = ",
              round(mod.res$therapy$out$gen$pval.Q.b.random, 3), "*)",sep = "")
)


   #### 6. Exclusively Clinicians in Training ####
mod.res$train <- list(out = lapply(mod.res$prim, function(x) x%>% update.meta(subgroup = TrainingExclusive, random = TRUE, fixed = FALSE)))

mod.res$train$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
  anx = data.frame(mod.res$train$out$anx[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  dep = data.frame(mod.res$train$out$dep[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  gen = data.frame(mod.res$train$out$gen[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")])
)
mod.res$train$tab <- lapply(mod.res$train$tab, function(x) x %>%
                              mutate_if(is.numeric, round, 2) %>%
                              mutate(ci = paste(upper.random.w, lower.random.w, sep = "-"),
                                     mod = "training") %>%
                              select(-c(upper.random.w, lower.random.w)) %>%
                              rename(Level = bylevs, K = k.w, SMD = TE.random.w, Q = Q.w, I2 = I2.w))
mod.res$train$sum <- list(
  dep =  paste("Random effects model for experience (Q = ", round(mod.res$train$out$dep$Q.b.random, 2), ", p = ",
               round(mod.res$train$out$dep$pval.Q.b.random, 3), ")", sep = ""),
  anx = paste("Random effects model for experience (Q = ", round(mod.res$train$out$anx$Q.b.random, 2), ", p = ",
              round(mod.res$train$out$anx$pval.Q.b.random, 3), "*)", sep = ""),
  gen = paste("Random effects model for experience (Q = ", round(mod.res$train$out$gen$Q.b.random, 2), ", p = ",
              round(mod.res$train$out$gen$pval.Q.b.random, 3), "*)",sep = "")
)

   #### 7. Includes any Training Clinicians ####

   #### 8. Stages of the hour glass ####
mod.res$hour <- list(out = lapply(mod.res$prim, function(x) x%>% update.meta(subgroup = HourGlass, random = TRUE, fixed = FALSE)))

mod.res$hour$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
  anx = data.frame(mod.res$hour$out$anx[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  dep = data.frame(mod.res$hour$out$dep[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  gen = data.frame(mod.res$hour$out$gen[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")])
)
mod.res$hour$tab <- lapply(mod.res$hour$tab, function(x) x %>%
                              mutate_if(is.numeric, round, 2) %>%
                              mutate(ci = paste(upper.random.w, lower.random.w, sep = "-"),
                                     mod = "hourglass") %>%
                              select(-c(upper.random.w, lower.random.w)) %>%
                              rename(Level = bylevs, K = k.w, SMD = TE.random.w, Q = Q.w, I2 = I2.w))
mod.res$hour$sum <- list(
  dep =  paste("Random effects model for treatment development stage (Q = ", round(mod.res$hour$out$dep$Q.b.random, 2), ", p = ",
               round(mod.res$hour$out$dep$pval.Q.b.random, 3), ")", sep = ""),
  anx = paste("Random effects model for treatment development stage (Q = ", round(mod.res$hour$out$anx$Q.b.random, 2), ", p = ",
              round(mod.res$hour$out$anx$pval.Q.b.random, 3), ")", sep = ""),
  gen = paste("Random effects model for treatment development stage (Q = ", round(mod.res$hour$out$gen$Q.b.random, 2), ", p = ",
              round(mod.res$hour$out$gen$pval.Q.b.random, 3), ")",sep = "")
)

   #### 9. Outcome Measure Sensitivity ####

dat.comb$dep$measure.type <- recode(dat.comb$dep$measure.type,"BDI-II" = "BDI", "BDI-I" = "BDI")

mod.res$measures$out <- list(
  dep = update.meta(object = mod.res$prim$dep, subgroup = measure.type, random = TRUE, fixed = FALSE,
                    subset = measure.type == "BDI" | measure.type == "PHQ-9"),
  anx = update.meta(object = mod.res$prim$anx, subgroup = measure.type, random = TRUE, fixed = FALSE,
                    subset = measure.type == "BAI" | measure.type == "GAD-7"),
  gen = update.meta(object = mod.res$prim$gen, subgroup = measure.type, random = TRUE, fixed = FALSE,
                    subset = measure.type == "CORE-OM" | measure.type == "BSI-GSI" |
                      measure.type == "SCL (Global)" | measure.type == "OQ-45" | measure.type == "PCL")
)

mod.res$measures$tab <- list(
  anx = data.frame(mod.res$measures$out$anx[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  dep = data.frame(mod.res$measures$out$dep[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  gen = data.frame(mod.res$measures$out$gen[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")])
)
mod.res$measures$tab <- lapply(mod.res$measures$tab, function(x) x %>%
                                 mutate_if(is.numeric, round, 2) %>%
                                 mutate(ci = paste(upper.random.w, lower.random.w, sep = "-"),
                                        mod = "measure") %>%
                                 select(-c(upper.random.w, lower.random.w)) %>%
                                 rename(Level = bylevs, K = k.w, SMD = TE.random.w, Q = Q.w, I2 = I2.w))
mod.res$measures$sum <- list(
  dep =  paste("Random effects model for measurement tool (Q = ", round(mod.res$measures$out$dep$Q.b.random, 2), ", p = ",
               round(mod.res$measures$out$dep$pval.Q.b.random, 3), ")", sep = ""),
  anx = paste("Random effects model for measurement tool (Q = ", round(mod.res$measures$out$anx$Q.b.random, 2), ", p = ",
              round(mod.res$measures$out$anx$pval.Q.b.random, 3), "*)", sep = ""),
  gen = paste("Random effects model for measurement tool (Q = ", round(mod.res$measures$out$gen$Q.b.random, 2), ", p = ",
              round(mod.res$measures$out$gen$pval.Q.b.random, 3), "*)",sep = "")
)
   #### 10. sample size - categorical####

mod.res$sample.n.group <- list(out = lapply(mod.res$prim, function(x) x%>% update.meta(subgroup = sample.n.group, random = TRUE, fixed = FALSE)))

mod.res$sample.n.group$out <- list(
  dep = update.meta(object = mod.res$prim$dep, subgroup = sample.n.group, random = TRUE, fixed = FALSE),
  anx = update.meta(object = mod.res$prim$anx, subgroup = sample.n.group, random = TRUE, fixed = FALSE),
  gen = update.meta(object = mod.res$prim$gen, subgroup = sample.n.group, random = TRUE, fixed = FALSE)
)

mod.res$sample.n.group$tab <- list(
  anx = data.frame(mod.res$sample.n.group$out$anx[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  dep = data.frame(mod.res$sample.n.group$out$dep[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")]),
  gen = data.frame(mod.res$sample.n.group$out$gen[c("bylevs","k.w", "TE.random.w", "lower.random.w", "upper.random.w", "Q.w", "I2.w")])
)
mod.res$sample.n.group$tab <- lapply(mod.res$sample.n.group$tab, function(x) x %>%
                                 mutate_if(is.numeric, round, 2) %>%
                                 mutate(ci = paste(upper.random.w, lower.random.w, sep = "-"),
                                        mod = "sample size") %>%
                                 select(-c(upper.random.w, lower.random.w)) %>%
                                 rename(Level = bylevs, K = k.w, SMD = TE.random.w, Q = Q.w, I2 = I2.w))
mod.res$sample.n.group$sum <- list(
  dep =  paste("Random effects model for sample size (Q = ", round(mod.res$sample.n.group$out$dep$Q.b.random, 2), ", p = ",
               round(mod.res$sample.n.group$out$dep$pval.Q.b.random, 3), ")", sep = ""),
  anx = paste("Random effects model for sample size (Q = ", round(mod.res$sample.n.group$out$anx$Q.b.random, 2), ", p = ",
              round(mod.res$sample.n.group$out$anx$pval.Q.b.random, 3), "*)", sep = ""),
  gen = paste("Random effects model for sample size (Q = ", round(mod.res$sample.n.group$out$gen$Q.b.random, 2), ", p = ",
              round(mod.res$sample.n.group$out$gen$pval.Q.b.random, 3), "*)",sep = "")
)


        ### Sub-group Moderator Table ####

mod.res$sector$tab <- lapply(mod.res$sector$tab, function(x) x%>% filter(Level != "Other"))
mod.res$setting$tab <- lapply(mod.res$setting$tab, function(x) x%>% filter(Level != "Mixed"))

mod.res$sub <- list(
            dep = bind_rows(mod.res$sector$tab$dep, mod.res$ITT$tab$dep, mod.res$setting$tab$dep,
                            mod.res$cont$tab$dep, mod.res$therapy$tab$dep, mod.res$hour$tab$dep,
                            mod.res$train$tab$dep, mod.res$measures$tab$dep, mod.res$sample.n.group$tab$dep),
            anx = bind_rows(mod.res$sector$tab$anx, mod.res$ITT$tab$anx, mod.res$setting$tab$anx,
                            mod.res$cont$tab$anx, mod.res$therapy$tab$anx, mod.res$hour$tab$anx,
                            mod.res$train$tab$anx, mod.res$measures$tab$anx, mod.res$sample.n.group$tab$anx),
            gen = bind_rows(mod.res$sector$tab$gen, mod.res$ITT$tab$gen, mod.res$setting$tab$gen,
                            mod.res$cont$tab$gen, mod.res$therapy$tab$gen, mod.res$hour$tab$gen,
                            mod.res$train$tab$gen, mod.res$measures$tab$gen, mod.res$sample.n.group$tab$gen)
                  )
mod.res$sub$dep$I2 <- scales::percent(mod.res$sub$dep$I2)
mod.res$sub$anx$I2 <- scales::percent(mod.res$sub$anx$I2)
mod.res$sub$gen$I2 <- scales::percent(mod.res$sub$gen$I2)

mod.res$sub$dep$Level <- recode(mod.res$sub$dep$Level, "Primary" = "mild", "secondary" = "moderate",
       "Uni. Clinics" = "university",  "residential" = "severe", "ITT" = "include",
      "N.Ameria" = "America", "Stage-3" = "routine evaluations", "Stage-1" = "preliminary studies",
      "No/NA" = "qualified", "Yes" = "trainees", "Dynamic" = "psychodynamic",
      "CBT" = "cognitive-behavioural")
mod.res$sub$anx$Level <- recode(mod.res$sub$anx$Level, "Primary" = "mild", "secondary" = "moderate",
                                "Uni. Clinics" = "university",  "residential" = "severe", "ITT" = "include",
                                "N.Ameria" = "America", "Stage-3" = "routine evaluations", "Stage-1" = "preliminary studies",
                                "No/NA" = "qualified", "Yes" = "trainees", "Dynamic" = "psychodynamic",
                                "CBT" = "cognitive-behavioural")
mod.res$sub$gen$Level <- recode(mod.res$sub$gen$Level, "Primary" = "mild", "secondary" = "moderate",
                                "Uni. Clinics" = "university",  "residential" = "severe", "ITT" = "include",
                                "N.Ameria" = "America", "Stage-3" = "routine evaluations", "Stage-1" = "preliminary studies",
                                "No/NA" = "qualified", "Yes" = "trainees", "Dynamic" = "psychodynamic",
                                "CBT" = "cognitive-behavioural")

mod.res$sub$dep$mod <-  recode(mod.res$sub$dep$mod,
                   "sector" = "severity", "completion" = "analysis", "therapy" = "therapy modality",
                   "hourglass" = "Tretament stage", "training" = "experience", "measure" = "measurement tool")
mod.res$sub$anx$mod <-  recode(mod.res$sub$anx$mod,
                   "sector" = "severity", "completion" = "analysis", "therapy" = "therapy modality",
                   "hourglass" = "Tretament stage", "training" = "experience", "measure" = "measurement tool")
mod.res$sub$gen$mod <-  recode(mod.res$sub$gen$mod,
                   "sector" = "severity", "completion" = "analysis", "therapy" = "therapy modality",
                   "hourglass" = "Tretament stage", "training" = "experience", "measure" = "measurement tool")



### 3. Meta-regression ####

    ### 1. PreN ####
# mod.res$preN <- list(out = lapply(mod.res$prim, function(x) x%>% metareg(PreN)))
#
# mod.res$preN$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
#   anx = data.frame(mod.res$preN$out$anx[c("k", "QM", "QMp", "R2")], beta = mod.res$preN$out$anx$beta[2,],
#                    ci.lb = mod.res$preN$out$anx$ci.lb[2], ci.ub = mod.res$preN$out$anx$ci.ub[2],
#                    se = mod.res$preN$out$anx$se[2], pval = mod.res$preN$out$anx$pval[2]),
#   dep = data.frame(mod.res$preN$out$dep[c("k", "QM", "QMp", "R2")], beta = mod.res$preN$out$dep$beta[2,],
#                    ci.lb = mod.res$preN$out$dep$ci.lb[2], ci.ub = mod.res$preN$out$dep$ci.ub[2],
#                    se = mod.res$preN$out$dep$se[2], pval = mod.res$preN$out$dep$pval[2]),
#   gen = data.frame(mod.res$preN$out$gen[c("k", "QM", "QMp", "R2")], beta = mod.res$preN$out$gen$beta[2,],
#                    ci.lb = mod.res$preN$out$gen$ci.lb[2], ci.ub = mod.res$preN$out$gen$ci.ub[2],
#                    se = mod.res$preN$out$gen$se[2], pval = mod.res$preN$out$gen$pval[2])
#   )
#
# mod.res$preN$tab <-
#   lapply(mod.res$preN$tab, function(x) x %>%
#                              mutate_if(is.numeric, round, 2) %>% # How to round/format when you need two figures to the right of the decimal.
#                              mutate(ci = paste(ci.ub, ci.lb, sep = "-"),
#                                     mod = "Pre N") %>%
#                              select(-c(ci.ub, ci.lb)) %>%
#                              relocate(k, beta, ci, se, pval, QM, QMp, R2) %>%
#                              rename(b = beta, p = pval))

# moderator <- "Pre N"
# Range <- paste(min(DepressionData$PreN), max(DepressionData$PreN), sep = "-")

    ### 2, Year ####
mod.res$year <- list(out = lapply(mod.res$prim, function(x) x%>% metareg(Year.x)))

mod.res$year$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
  anx = data.frame(mod.res$year$out$anx[c("k", "QM", "QMp", "R2")], beta = mod.res$year$out$anx$beta[2,],
                   ci.lb = mod.res$year$out$anx$ci.lb[2], ci.ub = mod.res$year$out$anx$ci.ub[2],
                   se = mod.res$year$out$anx$se[2], pval = mod.res$year$out$anx$pval[2]),
  dep = data.frame(mod.res$year$out$dep[c("k", "QM", "QMp", "R2")], beta = mod.res$year$out$dep$beta[2,],
                   ci.lb = mod.res$year$out$dep$ci.lb[2], ci.ub = mod.res$year$out$dep$ci.ub[2],
                   se = mod.res$year$out$dep$se[2], pval = mod.res$year$out$dep$pval[2]),
  gen = data.frame(mod.res$year$out$gen[c("k", "QM", "QMp", "R2")], beta = mod.res$year$out$gen$beta[2,],
                   ci.lb = mod.res$year$out$gen$ci.lb[2], ci.ub = mod.res$year$out$gen$ci.ub[2],
                   se = mod.res$year$out$gen$se[2], pval = mod.res$year$out$gen$pval[2])
)

mod.res$year$tab <-
  lapply(mod.res$year$tab, function(x) x %>%
           mutate_if(is.numeric, round, 2) %>% # How to round/format when you need two figures to the right of the decimal.
           mutate(ci = paste(ci.ub, ci.lb, sep = "-"),
                  mod = "Year (of publication)") %>%
           select(-c(ci.ub, ci.lb)) %>%
           relocate(k, beta, ci, se, pval, QM, QMp, R2) %>%
           rename(b = beta, p = pval))

#     moderator <- "Year (of publication)"
#     Mean_Range <- "(1988 - 2020)"


    ### 3. Age ####

mod.res$age <- list(out = lapply(mod.res$prim, function(x) x%>% metareg(AgeMean)))

mod.res$age$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
  anx = data.frame(mod.res$age$out$anx[c("k", "QM", "QMp", "R2")], beta = mod.res$age$out$anx$beta[2,],
                   ci.lb = mod.res$age$out$anx$ci.lb[2], ci.ub = mod.res$age$out$anx$ci.ub[2],
                   se = mod.res$age$out$anx$se[2], pval = mod.res$age$out$anx$pval[2]),
  dep = data.frame(mod.res$age$out$dep[c("k", "QM", "QMp", "R2")], beta = mod.res$age$out$dep$beta[2,],
                   ci.lb = mod.res$age$out$dep$ci.lb[2], ci.ub = mod.res$age$out$dep$ci.ub[2],
                   se = mod.res$age$out$dep$se[2], pval = mod.res$age$out$dep$pval[2]),
  gen = data.frame(mod.res$age$out$gen[c("k", "QM", "QMp", "R2")], beta = mod.res$age$out$gen$beta[2,],
                   ci.lb = mod.res$age$out$gen$ci.lb[2], ci.ub = mod.res$age$out$gen$ci.ub[2],
                   se = mod.res$age$out$gen$se[2], pval = mod.res$age$out$gen$pval[2])
)


mod.res$age$tab <-
  lapply(mod.res$age$tab, function(x) x %>%
           mutate_if(is.numeric, round, 2) %>% # How to round/format when you need two figures to the right of the decimal.
           mutate(ci = paste(ci.ub, ci.lb, sep = "-"),
                  mod = "mean age") %>%
           select(-c(ci.ub, ci.lb)) %>%
           relocate(k, beta, ci, se, pval, QM, QMp, R2) %>%
           rename(b = beta, p = pval))

#      moderator <- "Mean age"
#     Mean_Range <- "(19-60 years; M = 36)"




    ### 4. Sessions ####

# For the session meta-reg we need to filter only the studies that reported sessions.

dat.comb.sessions <- lapply(dat.comb, function(x) x%>% filter(DosageMetric == "Sessions"))

dat.res.sessions <- list(
    anx = metagen(EffectSize, Sample.Variance, data=dat.comb.sessions$anx, studlab=paste(Key), random = TRUE, prediction=TRUE, sm="SMD"),
    dep = metagen(EffectSize, Sample.Variance, data=dat.comb.sessions$dep, studlab=paste(Key), random = TRUE, prediction=TRUE, sm="SMD"),
    gen = metagen(EffectSize, Sample.Variance, data=dat.comb.sessions$gen, studlab=paste(Key), random = TRUE, prediction=TRUE, sm="SMD")
                        )


mod.res$sessions <- list(out = lapply(dat.res.sessions, function(x) x%>% metareg(SessionsAvg)))

mod.res$sessions$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
  anx = data.frame(mod.res$sessions$out$anx[c("k", "QM", "QMp", "R2")], beta = mod.res$sessions$out$anx$beta[2,],
                   ci.lb = mod.res$sessions$out$anx$ci.lb[2], ci.ub = mod.res$sessions$out$anx$ci.ub[2],
                   se = mod.res$sessions$out$anx$se[2], pval = mod.res$sessions$out$anx$pval[2]),
  dep = data.frame(mod.res$sessions$out$dep[c("k", "QM", "QMp", "R2")], beta = mod.res$sessions$out$dep$beta[2,],
                   ci.lb = mod.res$sessions$out$dep$ci.lb[2], ci.ub = mod.res$sessions$out$dep$ci.ub[2],
                   se = mod.res$sessions$out$dep$se[2], pval = mod.res$sessions$out$dep$pval[2]),
  gen = data.frame(mod.res$sessions$out$gen[c("k", "QM", "QMp", "R2")], beta = mod.res$sessions$out$gen$beta[2,],
                   ci.lb = mod.res$sessions$out$gen$ci.lb[2], ci.ub = mod.res$sessions$out$gen$ci.ub[2],
                   se = mod.res$sessions$out$gen$se[2], pval = mod.res$sessions$out$gen$pval[2])
)


mod.res$sessions$tab <-
  lapply(mod.res$sessions$tab, function(x) x %>%
           mutate_if(is.numeric, round, 2) %>% # How to round/format when you need two figures to the right of the decimal.
           mutate(ci = paste(ci.ub, ci.lb, sep = "-"),
                  mod = "Sessions (mean)") %>%
           select(-c(ci.ub, ci.lb)) %>%
           relocate(k, beta, ci, se, pval, QM, QMp, R2) %>%
           rename(b = beta, p = pval))

#      moderator <- "Sessions (mean)"
#      Mean_Range <- "(1-46 sessions; M = 15)"



    ### 5. Minority ####

mod.res$minor <- list(out = lapply(mod.res$prim, function(x) x%>% metareg(Minority.perc)))

mod.res$minor$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
  anx = data.frame(mod.res$minor$out$anx[c("k", "QM", "QMp", "R2")], beta = mod.res$minor$out$anx$beta[2,],
                   ci.lb = mod.res$minor$out$anx$ci.lb[2], ci.ub = mod.res$minor$out$anx$ci.ub[2],
                   se = mod.res$minor$out$anx$se[2], pval = mod.res$minor$out$anx$pval[2]),
  dep = data.frame(mod.res$minor$out$dep[c("k", "QM", "QMp", "R2")], beta = mod.res$minor$out$dep$beta[2,],
                   ci.lb = mod.res$minor$out$dep$ci.lb[2], ci.ub = mod.res$minor$out$dep$ci.ub[2],
                   se = mod.res$minor$out$dep$se[2], pval = mod.res$minor$out$dep$pval[2]),
  gen = data.frame(mod.res$minor$out$gen[c("k", "QM", "QMp", "R2")], beta = mod.res$minor$out$gen$beta[2,],
                   ci.lb = mod.res$minor$out$gen$ci.lb[2], ci.ub = mod.res$minor$out$gen$ci.ub[2],
                   se = mod.res$minor$out$gen$se[2], pval = mod.res$minor$out$gen$pval[2])
)

mod.res$minor$tab <-
  lapply(mod.res$minor$tab, function(x) x %>%
           mutate_if(is.numeric, round, 2) %>% # How to round/format when you need two figures to the right of the decimal.
           mutate(ci = paste(ci.ub, ci.lb, sep = "-"),
                  mod = "ethnicity (% minority)") %>%
           select(-c(ci.ub, ci.lb)) %>%
           relocate(k, beta, ci, se, pval, QM, QMp, R2) %>%
           rename(b = beta, p = pval))
#      moderator <- "Ethnicity (% minority)"
#      Mean_Range <- "(0-66%; M = 23%)"


    ### 6. Gender  ####

mod.res$female <- list(out = lapply(mod.res$prim, function(x) x%>% metareg(Female.perc)))

mod.res$female$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
  anx = data.frame(mod.res$female$out$anx[c("k", "QM", "QMp", "R2")], beta = mod.res$female$out$anx$beta[2,],
                   ci.lb = mod.res$female$out$anx$ci.lb[2], ci.ub = mod.res$female$out$anx$ci.ub[2],
                   se = mod.res$female$out$anx$se[2], pval = mod.res$female$out$anx$pval[2]),
  dep = data.frame(mod.res$female$out$dep[c("k", "QM", "QMp", "R2")], beta = mod.res$female$out$dep$beta[2,],
                   ci.lb = mod.res$female$out$dep$ci.lb[2], ci.ub = mod.res$female$out$dep$ci.ub[2],
                   se = mod.res$female$out$dep$se[2], pval = mod.res$female$out$dep$pval[2]),
  gen = data.frame(mod.res$female$out$gen[c("k", "QM", "QMp", "R2")], beta = mod.res$female$out$gen$beta[2,],
                   ci.lb = mod.res$female$out$gen$ci.lb[2], ci.ub = mod.res$female$out$gen$ci.ub[2],
                   se = mod.res$female$out$gen$se[2], pval = mod.res$female$out$gen$pval[2])
)

mod.res$female$tab <-
  lapply(mod.res$female$tab, function(x) x %>%
           mutate_if(is.numeric, round, 2) %>% # How to round/format when you need two figures to the right of the decimal.
           mutate(ci = paste(ci.ub, ci.lb, sep = "-"),
                  mod = "gender (% female)") %>%
           select(-c(ci.ub, ci.lb)) %>%
           relocate(k, beta, ci, se, pval, QM, QMp, R2) %>%
           rename(b = beta, p = pval))

#      moderator <- "Gender (% female)"
#      Mean_Range <- "(0-100%; M = 67%)"

    ### 7. Employment ####

mod.res$employ <- list(out = lapply(mod.res$prim, function(x) x%>% metareg(Employed.perc)))

mod.res$employ$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
  anx = data.frame(mod.res$employ$out$anx[c("k", "QM", "QMp", "R2")], beta = mod.res$employ$out$anx$beta[2,],
                   ci.lb = mod.res$employ$out$anx$ci.lb[2], ci.ub = mod.res$employ$out$anx$ci.ub[2],
                   se = mod.res$employ$out$anx$se[2], pval = mod.res$employ$out$anx$pval[2]),
  dep = data.frame(mod.res$employ$out$dep[c("k", "QM", "QMp", "R2")], beta = mod.res$employ$out$dep$beta[2,],
                   ci.lb = mod.res$employ$out$dep$ci.lb[2], ci.ub = mod.res$employ$out$dep$ci.ub[2],
                   se = mod.res$employ$out$dep$se[2], pval = mod.res$employ$out$dep$pval[2]),
  gen = data.frame(mod.res$employ$out$gen[c("k", "QM", "QMp", "R2")], beta = mod.res$employ$out$gen$beta[2,],
                   ci.lb = mod.res$employ$out$gen$ci.lb[2], ci.ub = mod.res$employ$out$gen$ci.ub[2],
                   se = mod.res$employ$out$gen$se[2], pval = mod.res$employ$out$gen$pval[2])
)

mod.res$employ$tab <-
  lapply(mod.res$employ$tab, function(x) x %>%
           mutate_if(is.numeric, round, 2) %>% # How to round/format when you need two figures to the right of the decimal.
           mutate(ci = paste(ci.ub, ci.lb, sep = "-"),
                  mod = "employment (% in full-time") %>%
           select(-c(ci.ub, ci.lb)) %>%
           relocate(k, beta, ci, se, pval, QM, QMp, R2) %>%
           rename(b = beta, p = pval))

    ### 8. Marriage ####


mod.res$married <- list(out = lapply(mod.res$prim, function(x) x%>% metareg(Married.perc)))

mod.res$married$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
  anx = data.frame(mod.res$married$out$anx[c("k", "QM", "QMp", "R2")], beta = mod.res$married$out$anx$beta[2,],
                   ci.lb = mod.res$married$out$anx$ci.lb[2], ci.ub = mod.res$married$out$anx$ci.ub[2],
                   se = mod.res$married$out$anx$se[2], pval = mod.res$married$out$anx$pval[2]),
  dep = data.frame(mod.res$married$out$dep[c("k", "QM", "QMp", "R2")], beta = mod.res$married$out$dep$beta[2,],
                   ci.lb = mod.res$married$out$dep$ci.lb[2], ci.ub = mod.res$married$out$dep$ci.ub[2],
                   se = mod.res$married$out$dep$se[2], pval = mod.res$married$out$dep$pval[2]),
  gen = data.frame(mod.res$married$out$gen[c("k", "QM", "QMp", "R2")], beta = mod.res$married$out$gen$beta[2,],
                   ci.lb = mod.res$married$out$gen$ci.lb[2], ci.ub = mod.res$married$out$gen$ci.ub[2],
                   se = mod.res$married$out$gen$se[2], pval = mod.res$married$out$gen$pval[2])
)

mod.res$married$tab <-
  lapply(mod.res$married$tab, function(x) x %>%
           mutate_if(is.numeric, round, 2) %>% # How to round/format when you need two figures to the right of the decimal.
           mutate(ci = paste(ci.ub, ci.lb, sep = "-"),
                  mod = "marital status (% marrier)") %>%
           select(-c(ci.ub, ci.lb)) %>%
           relocate(k, beta, ci, se, pval, QM, QMp, R2) %>%
           rename(b = beta, p = pval))

    ### 9. Quality ####

mod.res$rob <- list(out = lapply(mod.res$prim, function(x) x%>% metareg(QualityScore)))

mod.res$rob$tab <- list(# Need to find out if there is a way to apply this code across lists of lists.
  anx = data.frame(mod.res$rob$out$anx[c("k", "QM", "QMp", "R2")], beta = mod.res$rob$out$anx$beta[2,],
                   ci.lb = mod.res$rob$out$anx$ci.lb[2], ci.ub = mod.res$rob$out$anx$ci.ub[2],
                   se = mod.res$rob$out$anx$se[2], pval = mod.res$rob$out$anx$pval[2]),
  dep = data.frame(mod.res$rob$out$dep[c("k", "QM", "QMp", "R2")], beta = mod.res$rob$out$dep$beta[2,],
                   ci.lb = mod.res$rob$out$dep$ci.lb[2], ci.ub = mod.res$rob$out$dep$ci.ub[2],
                   se = mod.res$rob$out$dep$se[2], pval = mod.res$rob$out$dep$pval[2]),
  gen = data.frame(mod.res$rob$out$gen[c("k", "QM", "QMp", "R2")], beta = mod.res$rob$out$gen$beta[2,],
                   ci.lb = mod.res$rob$out$gen$ci.lb[2], ci.ub = mod.res$rob$out$gen$ci.ub[2],
                   se = mod.res$rob$out$gen$se[2], pval = mod.res$rob$out$gen$pval[2])
)

  mod.res$rob$tab <-
  lapply(mod.res$rob$tab, function(x) x %>%
           mutate_if(is.numeric, round, 2) %>% # How to round/format when you need two figures to the right of the decimal.
           mutate(ci = paste(ci.ub, ci.lb, sep = "-"),
                  mod = "risk of bias (1-10)") %>%
           select(-c(ci.ub, ci.lb)) %>%
           relocate(k, beta, ci, se, pval, QM, QMp, R2) %>%
           rename(b = beta, p = pval))

#  moderator <- "Risk of Bias (1-10)"
#  Mean_Range <- "(1-8; M = 5.69)"


        ### Meta-regression Table ####
mod.res$reg <- list(
    dep = bind_rows(mod.res$year$tab$dep, mod.res$age$tab$dep,
                    mod.res$sessions$tab$dep, mod.res$minor$tab$dep, mod.res$female$tab$dep,
                    mod.res$employ$tab$dep,  mod.res$rob$tab$dep),
    anx = bind_rows(mod.res$year$tab$anx, mod.res$age$tab$anx,
                    mod.res$sessions$tab$anx, mod.res$minor$tab$anx, mod.res$female$tab$anx,
                    mod.res$employ$tab$anx,  mod.res$rob$tab$anx),
    gen = bind_rows(mod.res$year$tab$gen, mod.res$age$tab$gen,
                    mod.res$sessions$tab$gen, mod.res$minor$tab$gen, mod.res$female$tab$gen,
                    mod.res$employ$tab$gen,  mod.res$rob$tab$gen)
            )

test = mod.res$reg$dep %>% select(mod)
test = data.frame(test,
           detail = c(
             #paste(round(mean(dat.comb$dep$PreN, na.rm = T), 2), " (", min(dat.comb$dep$PreN, na.rm = T), "-", max(dat.comb$dep$PreN, na.rm = T), ")", sep = ""),
             paste(round(mean(dat.comb$dep$Year.x, na.rm = T), 2), " (", min(dat.comb$dep$Year.x, na.rm = T), "-", max(dat.comb$dep$Year.x, na.rm = T), ")", sep = ""),
             paste(round(mean(dat.comb$dep$AgeMean, na.rm = T), 2), " (", min(dat.comb$dep$AgeMean, na.rm = T), "-", max(dat.comb$dep$AgeMean, na.rm = T), ")", sep = ""),
             paste(round(mean(dat.comb.sessions$dep$SessionsAvg, na.rm = T), 2), " (", min(dat.comb.sessions$dep$SessionsAvg, na.rm = T), "-", max(dat.comb.sessions$dep$SessionsAvg, na.rm = T), ")", sep = ""),
             paste(round(mean(dat.comb$dep$Minority.perc, na.rm = T), 2), " (", min(dat.comb$dep$Minority.perc, na.rm = T), "-", max(dat.comb$dep$Minority.perc, na.rm = T), ")", sep = ""),
             paste(round(mean(dat.comb$dep$Female.perc, na.rm = T), 2), " (", min(dat.comb$dep$Female.perc, na.rm = T), "-", max(dat.comb$dep$Female.perc, na.rm = T), ")", sep = ""),
             paste(round(mean(dat.comb$dep$Employed.perc, na.rm = T), 2), " (", min(dat.comb$dep$Employed.perc, na.rm = T), "-", max(dat.comb$dep$Employed.perc, na.rm = T), ")", sep = ""),
             paste(round(mean(dat.comb$dep$QualityScore, na.rm = T), 2), " (", min(dat.comb$dep$QualityScore, na.rm = T), "-", max(dat.comb$dep$QualityScore, na.rm = T), ")", sep = "")
                      ))
mod.res$reg$dep <- inner_join(mod.res$reg$dep, test)
mod.res$reg$dep$domain <- "dep"

test = mod.res$reg$anx %>% select(mod)
test = data.frame(test,
                  detail = c(
                    #paste(round(mean(dat.comb$anx$PreN, na.rm = T), 2), " (", min(dat.comb$anx$PreN, na.rm = T), "-", max(dat.comb$anx$PreN, na.rm = T), ")", sep = ""),
                    paste(round(mean(dat.comb$anx$Year.x, na.rm = T), 2), " (", min(dat.comb$anx$Year.x, na.rm = T), "-", max(dat.comb$anx$Year.x, na.rm = T), ")", sep = ""),
                    paste(round(mean(dat.comb$anx$AgeMean, na.rm = T), 2), " (", min(dat.comb$anx$AgeMean, na.rm = T), "-", max(dat.comb$anx$AgeMean, na.rm = T), ")", sep = ""),
                    paste(round(mean(dat.comb.sessions$anx$SessionsAvg, na.rm = T), 2), " (", min(dat.comb.sessions$anx$SessionsAvg, na.rm = T), "-", max(dat.comb.sessions$anx$SessionsAvg, na.rm = T), ")", sep = ""),
                    paste(round(mean(dat.comb$anx$Minority.perc, na.rm = T), 2), " (", min(dat.comb$anx$Minority.perc, na.rm = T), "-", max(dat.comb$anx$Minority.perc, na.rm = T), ")", sep = ""),
                    paste(round(mean(dat.comb$anx$Female.perc, na.rm = T), 2), " (", min(dat.comb$anx$Female.perc, na.rm = T), "-", max(dat.comb$anx$Female.perc, na.rm = T), ")", sep = ""),
                    paste(round(mean(dat.comb$anx$Employed.perc, na.rm = T), 2), " (", min(dat.comb$anx$Employed.perc, na.rm = T), "-", max(dat.comb$anx$Employed.perc, na.rm = T), ")", sep = ""),
                    paste(round(mean(dat.comb$anx$QualityScore, na.rm = T), 2), " (", min(dat.comb$anx$QualityScore, na.rm = T), "-", max(dat.comb$anx$QualityScore, na.rm = T), ")", sep = "")
                  ))
mod.res$reg$anx <- inner_join(mod.res$reg$anx, test)
mod.res$reg$anx$domain <- "anx"

test = mod.res$reg$gen %>% select(mod)
test = data.frame(test,
                  detail = c(
                    #paste(round(mean(dat.comb$gen$PreN, na.rm = T), 2), " (", min(dat.comb$gen$PreN, na.rm = T), "-", max(dat.comb$gen$PreN, na.rm = T), ")", sep = ""),
                    paste(round(mean(dat.comb$gen$Year.x, na.rm = T), 2), " (", min(dat.comb$gen$Year.x, na.rm = T), "-", max(dat.comb$gen$Year.x, na.rm = T), ")", sep = ""),
                    paste(round(mean(dat.comb$gen$AgeMean, na.rm = T), 2), " (", min(dat.comb$gen$AgeMean, na.rm = T), "-", max(dat.comb$gen$AgeMean, na.rm = T), ")", sep = ""),
                    paste(round(mean(dat.comb.sessions$gen$SessionsAvg, na.rm = T), 2), " (", min(dat.comb.sessions$gen$SessionsAvg, na.rm = T), "-", max(dat.comb.sessions$gen$SessionsAvg, na.rm = T), ")", sep = ""),
                    paste(round(mean(dat.comb$gen$Minority.perc, na.rm = T), 2), " (", min(dat.comb$gen$Minority.perc, na.rm = T), "-", max(dat.comb$gen$Minority.perc, na.rm = T), ")", sep = ""),
                    paste(round(mean(dat.comb$gen$Female.perc, na.rm = T), 2), " (", min(dat.comb$gen$Female.perc, na.rm = T), "-", max(dat.comb$gen$Female.perc, na.rm = T), ")", sep = ""),
                    paste(round(mean(dat.comb$gen$Employed.perc, na.rm = T), 2), " (", min(dat.comb$gen$Employed.perc, na.rm = T), "-", max(dat.comb$gen$Employed.perc, na.rm = T), ")", sep = ""),
                    paste(round(mean(dat.comb$gen$QualityScore, na.rm = T), 2), " (", min(dat.comb$gen$QualityScore, na.rm = T), "-", max(dat.comb$gen$QualityScore, na.rm = T), ")", sep = "")
                  ))
mod.res$reg$gen <- inner_join(mod.res$reg$gen, test)
mod.res$reg$gen$domain <- "gen"

mod.res$reg <- list(reg = lapply(mod.res$reg, function(x) x%>% relocate(domain, mod, detail)))
tabs$mod.reg <- rbind(mod.res$reg$reg$dep, mod.res$reg$reg$anx)
tabs$mod.reg  <- rbind(tabs$mod.reg,  mod.res$reg$reg$gen)
tabs$mod.reg$domain <- recode(tabs$mod.reg$domain, "dep" = "depression", "anx" = "anxiety", "gen" = "general")

tabs$mod.reg <- tabs$mod.reg %>% select(-p)

remove(dat.comb.sessions, dat.res.sessions, test)


# Multiple Meta-regression



### 1. Completion analysis * Average sessions ####
# Dummy
mod.res$multivar$completion_session$dummy <- list(
              dep = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$dep,
                        method = "ML", mods = ~ ITT, subset = DosageMetric == "Sessions"),
              anx = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$anx,
                  method = "ML", mods = ~ ITT, subset = DosageMetric == "Sessions"),
              gen = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$gen,
                  method = "ML", mods = ~ ITT, subset = DosageMetric == "Sessions")
                                    )
# Full
mod.res$multivar$completion_session$full <- list(
              dep = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$dep,
                        method = "ML", mods = ~ ITT + SessionsAvg, subset = DosageMetric == "Sessions"),
              anx = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$anx,
                        method = "ML", mods = ~ ITT + SessionsAvg, subset = DosageMetric == "Sessions"),
              gen = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$gen,
                        method = "ML", mods = ~ ITT + SessionsAvg, subset = DosageMetric == "Sessions")
                                  )
# Interaction
mod.res$multivar$completion_session$interact <- list(
              dep = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$dep,
                        method = "ML", mods = ~ ITT*SessionsAvg, subset = DosageMetric == "Sessions"),
              anx = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$anx,
                        method = "ML", mods = ~ ITT*SessionsAvg, subset = DosageMetric == "Sessions"),
              gen = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$gen,
                        method = "ML", mods = ~ ITT*SessionsAvg, subset = DosageMetric == "Sessions")
                                    )

### 2. Continent * Average sessions ####
# Dummy
mod.res$multivar$continent_session$dummy <- list(
              dep = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$dep,
                        method = "ML", mods = ~ Continent, subset = DosageMetric == "Sessions"),
              anx = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$anx,
                        method = "ML", mods = ~ Continent, subset = DosageMetric == "Sessions"),
              gen = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$gen,
                        method = "ML", mods = ~ Continent, subset = DosageMetric == "Sessions")
                                    )
# Full
mod.res$multivar$continent_session$full <- list(
  dep = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$dep,
            method = "ML", mods = ~ Continent + SessionsAvg, subset = DosageMetric == "Sessions"),
  anx = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$anx,
            method = "ML", mods = ~ Continent + SessionsAvg, subset = DosageMetric == "Sessions"),
  gen = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$gen,
            method = "ML", mods = ~ Continent + SessionsAvg, subset = DosageMetric == "Sessions")
)
# Interaction
mod.res$multivar$continent_session$interact <- list(
  dep = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$dep,
            method = "ML", mods = ~ Continent*SessionsAvg, subset = DosageMetric == "Sessions"),
  anx = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$anx,
            method = "ML", mods = ~ Continent*SessionsAvg, subset = DosageMetric == "Sessions"),
  gen = rma(yi = EffectSize, sei = Sample.Variance, data = dat.comb$gen,
            method = "ML", mods = ~ Continent*SessionsAvg, subset = DosageMetric == "Sessions")
)


