# Primary Random Effects Meta-Analyses
## 1. Primary RMA Models ####
    rma.anx <- rma(EffectSize, Sample.Variance, data = AnxietyData)
    rma.dep <- rma(EffectSize, Sample.Variance, data = DepressionData)
    rma.gen <- rma(EffectSize, Sample.Variance, data = GeneralData)
## 1.2 Fixed effect Models ####
fix.anx <- rma(EffectSize, Sample.Variance, method = "FE", data = AnxietyData)
fix.dep <- rma(EffectSize, Sample.Variance, method = "FE", data = DepressionData)
fix.gen <- rma(EffectSize, Sample.Variance, method = "FE", data = GeneralData)

      # Merge into one List
        Primary.MetaAnalysis <-  list(rma.anx, rma.dep, rma.gen, fix.anx, fix.dep, fix.gen)
        Primary.MetaAnalysis <- setNames(object = Primary.MetaAnalysis, nm=c("Anxiety", "Depression", "General", "Anxiety Fixed", "Depression Fixed", "General Fixed"))
        remove(rma.anx, rma.dep, rma.gen, fix.anx, fix.dep, fix.gen)
## 2. Rank Correlation Tests ####
    rank.anx <- ranktest(Primary.MetaAnalysis$Anxiety)
    rank.dep <- ranktest(Primary.MetaAnalysis$Depression)
    rank.gen <- ranktest(Primary.MetaAnalysis$General)
      # Merge into one List
        Primary.RankTests <-  list(rank.anx, rank.dep, rank.gen)
        Primary.RankTests <- setNames(object = Primary.RankTests, nm=c("Anxiety", "Depression", "General"))
        remove(rank.anx, rank.dep, rank.gen)
## 3. Regression Tests  ####
    reg.anx <- regtest(Primary.MetaAnalysis$Anxiety, model="rma")
    reg.dep <- regtest(Primary.MetaAnalysis$Depression, model="rma")
    reg.gen <- regtest(Primary.MetaAnalysis$General, model="lm")
      # Merge into one List
        Primary.RegressionTests <-  list(reg.anx, reg.dep, reg.gen)
        Primary.RegressionTests <- setNames(object = Primary.RegressionTests, nm=c("Anxiety", "Depression", "General"))
        remove(reg.anx, reg.dep, reg.gen)
## 4. Fail-Safe N ####
    fsn.anx <- fsn(yi = EffectSize, vi = Sample.Variance, dat = AnxietyData)
    fsn.dep <- fsn(yi = EffectSize, vi = Sample.Variance, dat = DepressionData)
    fsn.gen <- fsn(yi = EffectSize, vi = Sample.Variance, dat = GeneralData)
      # Merge into one List
        Primary.FSN <-  list(fsn.anx, fsn.dep, fsn.gen)
        Primary.FSN <- setNames(object = Primary.FSN, nm=c("Anxiety", "Depression", "General"))
        remove(fsn.anx, fsn.dep, fsn.gen)

## 5. NNT ####
        nnt.anx <- dmetar::NNT(Primary.MetaAnalysis$Anxiety$b[1,1])
        nnt.dep <- dmetar::NNT(Primary.MetaAnalysis$Depression$b[1,1])
        nnt.gen <- dmetar::NNT(Primary.MetaAnalysis$General$b[1,1])
        # Merge into one List
        Primary.nnt <-  list(nnt.anx, nnt.dep, nnt.gen)
        Primary.nnt <- setNames(object = Primary.nnt, nm=c("Anxiety", "Depression", "General"))
        remove(nnt.anx, nnt.dep, nnt.gen)



