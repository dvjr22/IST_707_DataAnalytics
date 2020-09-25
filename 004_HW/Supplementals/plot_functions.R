# Diego Valdes
# IST 707
# Apr 14, 2019
# Functions


# =============================================================================================================
# get the legend from a ggplot
# ensure plot has legend attached. Do not run - theme(legend.position = "none"), as there will be no legend
# to be returned. Remove the legend afterward
#
gplot = pepPlot
getLegend = function(gplot){
  
  tmp = ggplot_gtable(ggplot_build(gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")

  return(tmp$grobs[[leg]])
}
