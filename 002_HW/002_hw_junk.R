# This is a junk file
# any code I tried that didn't work as expected or not neccessary lives here
# keep it just in case I need to come back and use it

plot(schools$School, schools$Middling)

ggplot(schools[schools$School == 'A', ], aes(x=Middling)) + geom_histogram(binwidth = .5)


ggplot(bySchools, aes(x=bySchools[1,])) +
  geom_histogram(fill="white")

bySchools
Totals = rowSums(bySchools[,2:7])
bySchools/Totals


ggplot(schools, aes(Middling)) + geom_density(kernel = 'gaussian')
ggplot(schools, aes(Middling)) + geom_bar(fill = 'blue')
ggplot(schools, aes(Behind)) + geom_bar(fill = 'blue')
ggplot(schools, aes(Behind)) + geom_dotplot()


Aschool = schools[schools$School == 'A', ]
AschoolTotals = colSums(Aschool[,3:8 ])
AschoolTotals/sum(AschoolTotals)
signif(AschoolTotals/sum(AschoolTotals)*100, digits = 3)

print(AschoolTotals/sum(AschoolTotals))

schoolPercent = function(df, school_code) {
  sc = df[df[ ,1] == school_code, ]
  return(rbind(Total = colSums(sc[,3:8 ]), Percent = signif(colSums(sc[,3:8 ])/sum(colSums(sc[,3:8 ]))*100, digits = 3)))
}

A = schoolPercent(schools, 'A')
B = schoolPercent(schools, 'B')
C = schoolPercent(schools, 'C')
D = schoolPercent(schools, 'D')
E = schoolPercent(schools, 'E')

A
B
C
D
E

plot(A[1, 1:6])
hist(A[1, 1:6])


schoolPercentV2 = function(df, col_num, school_code) {
  sc = df[df[ ,col_num] == school_code, ]
  return(rbind(Total = colSums(sc[,3:8 ]), Percent = signif(colSums(sc[,3:8 ])/sum(colSums(sc[,3:8 ]))*100, digits = 3)))
}

A
cast(schools, Section ~ School )


grid.arrange(histPlot, histPlot.v2, ncol=2)

a = ggplot(schools, aes(School, Completed)) +
  geom_col()
#geom_col(aes(fill = Section))

b = ggplot(schools, aes(School, VeryBehind)) +
  geom_col()
#geom_col(aes(fill = Section))

grid.arrange(a,b, ncol=2)

# nope
Aschool = schools[schools$School == 'A', ]
Dschool = schools[schools$School == 'D', ]

scale(Aschool$Middling)
scale(Dschool$Middling)

ScaledSchool = NewSchoolsSec[,-8]

for (i in 3:length(ScaledSchool)) {
  ScaledSchool[,i] = scale(ScaledSchool[,i])
}




for (i in 1:length(TheListCounts)) {
  section.hist = rbind(section.hist, TheListCounts[[i]])
}

section.hist.per = data.frame()
for (i in 1:length(TheListPer)) {
  section.hist.per = rbind(section.hist.per, TheListPer[[i]])
}

?t.test
x = t.test(A, B) # two sided t test, if p is low, h0 must go


x = t.test(A, B, alternative = 'greater') # two sided t test, if p is low, h0 must go
x$p.value < 0.05

x = t.test(A, B, alternative = 'less') # two sided t test, if p is low, h0 must go
x$p.value < 0.05

ggplot(bySchools,aes(School, Middling)) + geom_bar(stat = 'identity')
ggplot(bySchools,aes(School, Middling)) + geom_line()

AB = twoSample(1,0, hypoTest[1,2], hypoTest[1,3], hypoTest[2,2], hypoTest[2,3])
AB$p.value < 0.05 # reject null - school is making a difference

BC = twoSample(1,0, hypoTest[2,2], hypoTest[2,3],hypoTest[3,2], hypoTest[3,3])
BC$p.value < 0.05 # reject null - - school is making a difference

CD = twoSample(1,0, hypoTest[3,2], hypoTest[3,3],hypoTest[4,2], hypoTest[4,3])
CD$p.value < 0.05 # cannot recject null - school may not be making a difference

DE = twoSample(1,0, hypoTest[4,2], hypoTest[4,3],hypoTest[5,2], hypoTest[5,3])
DE$p.value < 0.05 # cannot recject null - school may not be making a difference

AE = twoSample(1,0, hypoTest[5,2], hypoTest[5,3],hypoTest[1,2], hypoTest[1,3])
AE$p.value < 0.05 # cannot recject null - school may not be making a difference

DU = 1*hypoTest[i,2] + hypoTest[i,3]
A = hypoTest[i,3]
DPO =  (hypoTest[i,3]/(1*hypoTest[i,2] + hypoTest[i,3]) ) *1000000
SQL = DPO*1000000
SQL
3*933
677/933
.72*1000000

one = rep(1,255)
two = rep(1, 209)
three = rep(.75, 677)
four = rep(.75, 237)
one = c(one, three)
two = c(two, four)
x2 = t.test(one, two, alternative = 'less')
one = c(rep(1,hypoTest$AtLvl),rep(0, hypoTest$AtRisk) )
