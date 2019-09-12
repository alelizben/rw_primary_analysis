setwd("~/Documents/ptbi/ga_prediction")

b = read.csv(file= "boys_intergrowth.csv", header = F)
g = read.csv(file = "girls_intergrowth.csv" , header = F)
#names(b) = c("weeks", "count", "3pct", "10pct", "50pct", "90pct", "97pct")
#names(g) = c("weeks", "count", "3pct", "10pct", "50pct", "90pct", "97pct")

girls_normal= apply(g[1:9, 3:7], 2, as.numeric)
boys_normal = apply(b[1:9, 3:7], 2, as.numeric)
girls_lancet = (g[10:19, 3:7])
boys_lancet = b[10:19, 3:7]

new_girls_lancet = apply(girls_lancet, 2, function(x) as.numeric(gsub("·", ".", x)))
new_boys_lancet = apply(boys_lancet, 2, function(x) as.numeric(gsub("·", ".", x)))

girls = rbind(girls_normal, new_girls_lancet)
boys = rbind(boys_normal, new_boys_lancet)
#alt shift 9
girls_fr = data.frame( girls)
boys_fr = data.frame( boys)

girls_full = data.frame(as.numeric(g$V2), girls)
boys_full = data.frame(as.numeric(b$V2), boys)

names(girls_full) = c("count", paste0("pct", c(3, 10, 50, 90, 97)))

names(boys_full) = c("count", paste0("pct", c(3, 10, 50, 90, 97)))

#girls_fr[girls_fr < 2.5] <- NA
#boys_fr[boys_fr < 2.5] <- NA

girls_full

approx_marg_prob = function(wk_row, thresh_wt) {

  pct = c(0.03, 0.1, 0.5, 0.9, 0.97) 
  index_pct = max(which(wk_row < thresh_wt))
  if (index_pct < 0) {
    
    est_prob = 0
    
  } else if (index_pct < length(pct)) {
    
    diff_pct = pct[index_pct + 1] - pct[index_pct]
    #test[index_pct + 1] - test[index_pct]
    prop_of_wt_diff = (thresh_wt - wk_row[index_pct]) / (wk_row[index_pct + 1] - wk_row[index_pct])
    est_prob = (prop_of_wt_diff * diff_pct) + pct[index_pct]
    
    } else {
      
    est_prob = 1
    
    }
  return(est_prob)
  
}
#P(weight < x | y wks) * P(y wks) = P(weight < x and y wks)

#g_prob_wt_wk = c(rep(1, 9), 0.95, .87, 0.6, 0.4, .4, .10, .07, 0,0,0)

## marginal probability of weeks
g_prob_wks = girls_full$count / sum(girls_full$count)

## conditional probability given wks
g_prob_wt_wk = apply(girls_fr, 1, function(x) approx_marg_prob(x, 2.5))
#probability < 37wks| <2.5kg = p(<2.5kg & < 37wks) / p(<2.5kg)
sum((g_prob_wks * g_prob_wt_wk)[1:which(g$V1 == 36)]) / sum(g_prob_wks * g_prob_wt_wk)

##marginal probabilty of weeks
b_prob_wks = boys_full$count / sum(boys_full$count)
b_prob_wks

#b_prob_wt_wk = c(rep(1, 9), 0.88, 0.70, .55, .45, .45, 0.08, 0,0,0,0)
#conditional< 2.5 given weeeks
b_prob_wt_wk = apply(boys_fr, 1, function(x) approx_marg_prob(x, 2.5))
#40th pctl is 2.435
sum((b_prob_wks * b_prob_wt_wk)[1:which(b$V1 == 36)]) / sum(b_prob_wks * b_prob_wt_wk)

sum((b_prob_wks * b_prob_wt_wk)[1:which(b$V1 == 36)]) / sum(b_prob_wks[1:which(b$V1 == 36)])
#marginal prob < 2500

#According to their data, babies born less than 2500g have a high likelihood of being preterm
#P(ga < 37 | weight < 2500g)
#P(ga < 37 & weight < 2500) / p(weight < 2500)


#g_prob_wt_wk = c(rep(1, 9), 0.95, .87, 0.6, 0.4, .4, .10, .07, 0,0,0)
#probabilty born less than < 2.5g are <37 wks: denominator is probability being less than 2.5kg
sum((g_prob_wks * g_prob_wt_wk)[1:which(g$V1 == 36)])/sum(g_prob_wks * g_prob_wt_wk)
#now try proability preterm baby is less than < 2.5g
sum((g_prob_wks * g_prob_wt_wk)[1:which(g$V1 == 36)])/sum(g_prob_wks[1:which(g$V1 == 36)])
#marginal prob < 2500


##condition upoon > 3k, what is P >= 37wks
##condition upon >=37wks, what is P > 3k
g_prob_wt_wk_3kg = 1 - apply(girls_fr, 1, function(x) approx_marg_prob(x, 3))
b_prob_wt_wk_3kg = 1 - apply(boys_fr, 1, function(x) approx_marg_prob(x, 3))

## conditional upon >3k, what is P >= 37wks
sum((g_prob_wks * g_prob_wt_wk_3kg)[g$V1 >= 37])/sum(g_prob_wks * g_prob_wt_wk_3kg)
sum((b_prob_wks * b_prob_wt_wk_3kg)[b$V1 >= 37])/sum(b_prob_wks * b_prob_wt_wk_3kg)

## conditional upon >= 37wks, what is P > 3k
sum((g_prob_wks * g_prob_wt_wk_3kg)[g$V1 >= 37])/sum(g_prob_wks[g$V1 >= 37])
sum((b_prob_wks * b_prob_wt_wk_3kg)[b$V1 >= 37])/sum(b_prob_wks[b$V1 >= 37])

