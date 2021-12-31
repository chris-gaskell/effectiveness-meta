# Aggregate ES for multiple study samples
dat.agg <- list()
dat.agg$dep <- escalc(yi= EffectSize, vi= Sample.Variance, data = dat$dep)
dat.agg$dep <- aggregate(dat.agg$dep, cluster=Key2, struct="ID")
dat.agg$anx <- escalc(yi= EffectSize, vi= Sample.Variance, data = dat$anx)
dat.agg$anx <- aggregate(dat.agg$anx, cluster=Key2, struct="ID")
dat.agg$gen <- escalc(yi= EffectSize, vi= Sample.Variance, data = dat$gen)
dat.agg$gen <- aggregate(dat.agg$gen, cluster=Key2, struct="ID")

res <- list(
      rma = list(# Random Effects Meta-Analyses
          anx = rma(EffectSize, Sample.Variance, data = dat.agg$anx),
          dep = rma(EffectSize, Sample.Variance, data = dat.agg$dep),
          gen = rma(EffectSize, Sample.Variance, data = dat.agg$gen)
                  ),
      fix = list(# Fixed Effects Meta-Analyses
          anx = rma(EffectSize, Sample.Variance, method = "FE", data = dat.agg$anx),
          dep = rma(EffectSize, Sample.Variance, method = "FE", data = dat.agg$dep),
          gen = rma(EffectSize, Sample.Variance, method = "FE", data = dat.agg$gen)
      ))

res$bias <- list(
      rank = list(# Rank correlation tests
          anx = ranktest(res$rma$dep),
          dep = ranktest(res$rma$anx),
          gen = ranktest(res$rma$gen)
      ),
      reg = list(# Rank correlation tests
        anx = regtest(res$rma$anx, model="rma"),     # Should this be rma or should it be lm?????
        dep = regtest(res$rma$dep, model="rma"),
        gen = regtest(res$rma$gen, model="rma")
      ),
      fsn = list(# Fail safe N tests
        anx = fsn(yi = EffectSize, vi = Sample.Variance, dat = dat.agg$anx),
        dep = fsn(yi = EffectSize, vi = Sample.Variance, dat = dat.agg$dep),
        gen = fsn(yi = EffectSize, vi = Sample.Variance, dat = dat.agg$gen)
      ),
      nnt = list(# Number needed to treat
        anx = dmetar::NNT(res$rma$anx$b[1,1]),
        dep = dmetar::NNT(res$rma$dep$b[1,1]),
        gen = dmetar::NNT(res$rma$gen$b[1,1])
      ))

res$rma$anx$d.class <- CohenClass(res$rma$anx$b)
res$rma$anx$I2.class <- HeteroClass(res$rma$anx$b)
res$rma$dep$d.class <- CohenClass(res$rma$dep$b)
res$rma$dep$I2.class <- HeteroClass(res$rma$dep$b)
res$rma$gen$d.class <- CohenClass(res$rma$gen$b)
res$rma$gen$I2.class <- HeteroClass(res$rma$gen$b)

res$irr <- list(
  data = list(
    abst = read_excel("Input/irr/irr.xlsx", sheet = "abstract"),
    full = read_excel("Input/irr/irr.xlsx", sheet = "full-text"),
    outcomes = read_excel("Input/irr/irr.xlsx", sheet = "outcomes"),
    quality = read_excel("Input/irr/irr.xlsx", sheet = "quality")
  ))
res$irr$data$outcomes.es <- res$irr$data$outcomes[3:5] %>% filter(Data == "ES") %>% select(-Data)

res$irr$agree = list(
  abst = agree(res$irr$data$abst),
  full = agree(res$irr$data$full[c(2,4)]),
  outcomes = agree(res$irr$data$outcomes[4:5]),
  outcomes.es = agree(res$irr$data$outcomes.es),
  quality = agree(res$irr$data$quality)
    )
res$irr$k = list(abst = kappa2(res$irr$data$abst),
             full = kappa2(res$irr$data$full[c(2,4)]),
             outcomes = kappa2(res$irr$data$outcomes[4:5]),
             quality = kappa2(res$irr$data$quality),
             outcomes.es = kappa2(res$irr$data$outcomes.es),
             quality.ord = kappa2(res$irr$data$quality, weight = "equal")
      )


##  Quality by item
quality.agree <- read_excel("Input/irr/irr.xlsx", sheet = "quality.item")

res$irr$RoB <- list()
res$irr$RoB$agree <- list(
      all = agree(quality.agree[3:4]),
      criteria = agree(filter(quality.agree, item == "criteria")[3:4]),
      consecutive = agree(filter(quality.agree, item == "consecutive")[3:4]),
      complete = agree(filter(quality.agree, item == "complete")[3:4]),
      demographics = agree(filter(quality.agree, item == "demographics")[3:4]),
      intervention.info = agree(filter(quality.agree, item == "intervention.info")[3:4]),
      outcome.data = agree(filter(quality.agree, item == "outcome.data")[3:4]),
      service.data = agree(filter(quality.agree, item == "service.data")[3:4]),
      analysis = agree(filter(quality.agree, item == "analysis")[3:4])
              )
res$irr$RoB$kappa <- list(
  all = kappa2(quality.agree[3:4]),
  criteria = kappa2(filter(quality.agree, item == "criteria")[3:4]),
  consecutive = kappa2(filter(quality.agree, item == "consecutive")[3:4]),
  complete = kappa2(filter(quality.agree, item == "complete")[3:4]),
  demographics = kappa2(filter(quality.agree, item == "demographics")[3:4]),
  intervention.info = kappa2(filter(quality.agree, item == "intervention.info")[3:4]),
  outcome.data = kappa2(filter(quality.agree, item == "outcome.data")[3:4]),
  service.data = kappa2(filter(quality.agree, item == "service.data")[3:4]),
  analysis = kappa2(filter(quality.agree, item == "analysis")[3:4])
)



