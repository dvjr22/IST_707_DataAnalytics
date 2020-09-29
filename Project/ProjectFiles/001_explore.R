# =============================================================================================================
# RESET WORKSPACE
# =============================================================================================================

rm(list=ls()) # clear work space
dev.off(dev.list()["RStudioGD"]) # clear plots

# =============================================================================================================
# LIBRARIES
# =============================================================================================================

library(dplyr)
loadLibraries()


originalWD = getwd() # get original wd for saving to .r file
setwd("C:/Users/dvjr2/Google Drive/Documents/Syracuse/IST_707/Project")

# =============================================================================================================
# IMPORT AND CLEAN DATA
# =============================================================================================================


states_og = read.csv('states_all.csv', stringsAsFactors = FALSE)
str(states_og)

DF.STATES = states_og
naInData(DF.STATES)
DF.STATES = DF.STATES[,-1]
DF.STATES = toFactor(DF.STATES,c(1,2))
str(DF.STATES)

DF.STATES$STATE = sub('_', " ", DF.STATES$STATE) # get rid of _ in state names

# filter to keep only 50 states
stateFilter = toupper(state.name)
DF.STATES = DF.STATES[DF.STATES$STATE %in% stateFilter,]
str(DF.STATES)
naInData(DF.STATES)

######################################################################################################

StateRev = DF.STATES[,c(1,4)]
StateRev = StateRev[!is.na(StateRev[,2]),]

StateRevSum = aggregate(x=StateRev$TOTAL_REVENUE,
                 by=list(StateRev$STATE),
                 FUN=mean)

plot_revenue = ggplot(StateRevSum, aes(Group.1, x)) + geom_bar(stat = "identity") +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 6, hjust = 1)) + ggtitle("Total revenue")


StateEnroled = DF.STATES[,c(1,3)]
StateEnroled = StateEnroled[!is.na(StateEnroled[,2]),]
StateEnroledMean = aggregate(x=StateEnroled$ENROLL,
                             by=list(StateEnroled$STATE),
                             mean)
plot_enrolled = ggplot(StateEnroledMean, aes(Group.1, x)) + geom_bar(stat = "identity") +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 6, hjust = 1)) + ggtitle("Enrolled Students")



perStudent = StateRevSum[,2]/StateEnroledMean[,2]
States = StateEnroledMean$Group.1
PerStuDF = data.frame(States, perStudent)
ggplot(PerStuDF, aes(States, perStudent)) + geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 6, hjust = 1))

State4Score = DF.STATES[,c(1,21,23)]
naInData(State4Score)

State4Score = State4Score[!is.na(State4Score[,2]),]
State4Score = State4Score[!is.na(State4Score[,3]),]

State4ScoreMean = aggregate(x=cbind(State4Score$AVG_MATH_4_SCORE, State4Score$AVG_READING_4_SCORE),
                            by=list(State4Score$STATE),
                            mean)
State4ScoreMean$perStudent = perStudent
plot_math_scores = ggplot(State4ScoreMean, aes(Group.1, V1)) + geom_bar(stat = "identity", aes(fill=perStudent)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 6, hjust = 1)) + ggtitle("Math Scores")
plot_read_scores = ggplot(State4ScoreMean, aes(Group.1, V2)) + geom_bar(stat = "identity", aes(fill=perStudent)) +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 6, hjust = 1)) + ggtitle("Reading Scores")


