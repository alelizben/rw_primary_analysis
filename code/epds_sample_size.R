library(here)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
#setwd('~/Documents/ptbi/power calc/')
dat = read.csv(here("data", "epds_Data.csv"))
ctrl_n = dat$Number.of.Observations.Postpartum[dat$Arm == "Control" | dat$Arm == "Control "]
ctrl_n = ctrl_n[ctrl_n > 11]
length(ctrl_n)


interv_n = dat$Number.of.Observations.Postpartum[dat$Arm == "Group" | dat$Arm == "Group "]
interv_n = interv_n[interv_n > 11]
length(interv_n)
all = c(ctrl_n, interv_n)
all = all[all < 130 & all > 16]
all

current.rates = c(ctrl_n, interv_n)

## more depressed; selection  bias; depressed women
## sentence that goes with confidence interval 

## matched for proportion M_Hayes using harmonic mean (7.14)


##unmatched M_H 7.11
power.var.cl.size = function(p0, p1, m.H, rho, nclust){
  t = pnorm( sqrt( (nclust-1) * (m.H * (p0 - p1) ^ 2) / ((p0 * (1 - p0) + p1 * (1 - p1)) * (1 + (m.H - 1) * rho)) ) - qnorm(.975))
  
  return(t)
}

get_km = function(km_range, pct, p0) {
  #rates to inrease volume  by
  rates = c(0, 3, 5, 7)
  power = matrix(NA, length(rates), length(km_range))
  volume = rep(NA, length(rates))
  
  for (k in 1:length(km_range)) {
    
    rho = km_range[k]
    
    for(i in 1:length(rates)){
      rate.increase = rates[i]
  
      pct.rd = pct
      
      ss.calc = function(rate.increase) {
   
          ss = current.rates * ((1 + rate.increase))
          #ss = nnd.rates * (c(13, 13, 8, 8) + (1 + rate.increase) * c(5, 5, max(0, add.months - 1), max(0, add.months - 1)))
   
          vect = ss
      
         #ctrl = round(mean(ss[1:14]))
         ctrl = round(mean(ss[1:(length(ss)/2)]))
         interv = round(mean(ss[(length(ss)/2 + 1):length(ss)]))
         rate = sprintf("avg ctrl %s, avg interv %s", ctrl, interv)
        

        #return(c(rep(ss[1], 2), rep(ss[2], 2), rep(ss[3], 8), rep(ss[4], 8)))
        return(list(vect, rate))
      }
      
      ## changed Nov6 
      #p0   = .25
      #p0 = .125
      p0 = p0
      
      ss = ss.calc(rate.increase)
      p1  = p0 * (1 - pct.rd)
      m.H = 1 / mean(1 / ss[[1]])

      power[i, k] = power.var.cl.size(p0, p1, m.H, rho, length(ss[[1]]))
      volume[i] = ss[[2]]
      
      ## rate increase, add months, km
    }
    
  }
  
  #row_names = paste0(seq(0, 50, 10), "pct_fac_volume_incr")
  #f = split(volume, seq(nrow(volume)))

  #row_names = lapply(f, function(j) sprintf("ug %s, ken %s", j[1], j[2]))
  
  #rownames(power) = paste('N/fac = ',  round(volume, 2))
  rownames(power) = volume
  
  output = data.frame(power)
  names(output) = km_range
  
  #class(j)
  #colnames(j) = col_names
  return(output)
  
} 

km_range = seq(0, 0.3, 0.06)


plot_power = function(x1, pct_red, p0, nclust) {
  
  #row_names = paste(seq(0, 50, 10), "%_volume_incr", sep = "")
  row_names1 = row.names(x1)

  
  df1 = melt(x1)

  
  ##ignore the warning here
  df1$rowid = row_names1
  df1$variable = as.numeric(levels(df1$variable))[df1$variable]
  
  head(df1)

  # scale_x_continuous(name="km", limits=c(0, 1))
  
  pl1 = ggplot(df1, aes(variable, value, group=factor(rowid))) + 
    geom_line(aes(color=factor(rowid)), linetype = 5) + 
    labs(x = "", color = NULL) + labs(y = "power") +
    #labs(title = sprintf("effect size = %s%%, %s mos, Uganda ends early", pct_red*100, mos)) + 
    labs(title = sprintf("effect size = %s%%, Rwanda", pct_red * 100)) + 
    scale_y_continuous(breaks = seq(0.0, 1, 0.1), limits = c(0, 1)) + 
    scale_x_continuous( "icc", breaks = km_range, limits = c(0, max(km_range))) + theme(axis.text=element_text(size= 13),
                                                                                        legend.position = "bottom", legend.text=element_text(size = 12))
  
  #plot(pl1)

  #plot(pl2)
  #mylegend<-g_legend(pl2)
  
  #jpeg(sprintf('power_plot_%s_%s.jpg', pct_red*100, mos), width = 1442, height = 600)
  jpeg(here("", sprintf('power_nclust%s_p0%s_effectsize%s_%s.jpg', nclust, p0, pct_red*100, Sys.Date())), width = 900, height = 630)
  
  grid.arrange(pl1)    
  
  # grid.arrange(arrangeGrob(pl1, grob(NULL),
  #                                pl2 + theme(legend.position="none"),
  #                                nrow=1, ncol = 3, widths = c(9, 0.5, 9)),
  #                    mylegend, nrow = 2, heights = c(10, 1))
  # 
  # grid.arrange(arrangeGrob(pl1, grob(NULL),
  #                          pl2 + theme(legend.position="none"),
  #                          nrow=1, ncol = 3, widths = c(9, 0.5, 9)),
  #               nrow = 2, heights = c(10, 1))
  
  
  #plot(pl)
  dev.off()
  
}


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

n_arm = 15
current.rates = c(sample(all, n_arm), sample(all, n_arm))
#current.rates = c(ctrl_n, interv_n)

for (p in c(0.4, 0.5, 0.6)) {
    
    X1 = get_km(km_range, p, .15)

    plot_power(X1, p, 0.15, length(current.rates))
    
} 


