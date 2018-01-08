 
rules = c(50, 75, 90) # policy thresholds
for (i in 1:3) {
  print(paste0('Rule: Admit top', rules[i]))
  blind_pctile = ntile(blind_fit.tst, 100)
  aware_pctile = ntile(aware_fit.tst, 100)
  
  blind_admit = ifelse(blind_pctile >= rules[i], 1, 0)
  aware_admit = ifelse(aware_pctile >= rules[i], 1, 0)
  
  # aware_rule = (race_covar.tst$black == 1 & aware_pctile > blind_pctile & aware_pctile >= rules[i]) |
  #   (race_covar.tst$black == 0 & aware_pctile > blind_pctile & blind_pctile >= rules[i]) |
  #   (race_covar.tst$black == 1 & aware_pctile < blind_pctile & blind_pctile >= rules[i]) |
  #   (race_covar.tst$black == 0 & aware_pctile < blind_pctile & aware_pctile >= rules[i])

  # aware_admit2 = ifelse(aware_rule, 1, 0)
  
  # print(paste0("Differences? ", sum(aware_admit1 != aware_admit2)))
  
  print(paste0('Similarity: ', sum(blind_admit == aware_admit)/tst_sz))
  
  blind_admit.acc = mean(y.tst[blind_admit == 1])
  aware_admit.acc = mean(y.tst[aware_admit == 1])
  print(paste0('Blind admit accuracy: ', blind_admit.acc))
  print(paste0('Aware admit accuracy: ', aware_admit.acc))
  
  blind_admit.blk = mean(race_covar.tst$black[blind_admit == 1])
  aware_admit.blk = mean(race_covar.tst$black[aware_admit == 1])
  print(paste0('Blind black %', blind_admit.blk))
  print(paste0('Aware black %', aware_admit.blk))
  print('---------------------')
}




 
