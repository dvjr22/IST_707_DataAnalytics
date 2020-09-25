# Diego Valdes
# IST 707
# Apr 14, 2019
# Functions


# =============================================================================================================
# get the legend from a ggplot
#
gplot = pepPlot
getLegend = function(gplot){
  
  tmp = ggplot_gtable(ggplot_build(gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")

  return(tmp$grobs[[leg]])
}
