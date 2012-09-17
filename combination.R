# all combination of table factors
wb <- read.csv("./workbook.csv")

edu <- wb$Education[1:8]
loan <- wb$Loan.Amount.Monthly.Income
res <- wb$Residential.Status[1:3]
mar <- wb$Marital.Status[1:7]
pos <- wb$Position.Employment.Status[1:11]
age <- wb$Age[1:4]
dep <- wb$Number.Dependants[1:4]
cibil <- wb$CIBIL.Score[1:4]
car <- wb$Car.Registration[1:2]

combines <- expand.grid(edu, loan, res, mar, pos, age, dep, cibil, car)

colnames(combines) <- names(wb)

combines <- data.frame(lapply(combines, as.character), stringsAsFactors=FALSE)

# generate scores
# create a lookup table
score_edu <- c('School up to 4 years'=-8, 'School 5-9 years'=-6, 'SSC/HSC'=-4, 'College but not graduate'=0,  Graduate=8, 'Postgraduate general/Postgraduate professional'=10, 'Student at School'=0, Other=-6)
score_loan <- c(AA=-10, AB=2, AC=6, AD=10, AE=14, AF=16, BA=-16, BB=-2, BC=2, BD=6, BE=10, BF=14, CA=-28, CB=-4, CC=-2, CD=2, CE=6, CF=10, DA=-24, DB=-6, DC=-4, DD=-2, DE=2, DF=6, EA=-30, EB=-8, EC=-6, ED=-4, EE=-2, EF=2)
score_res <- c('Self/spouse owned'=5, 'Living with parents'=0, Rent=-2)
score_mar <- c(Divorced=0, 'Living together'=0, Married=6, Separated=0, Single=-4, Widowed=0, Other=0)
score_pos <- c('Part-time'=-2, 'Home-maker'=0, Student=0, 'Full-time Clerical/Salesmen'=0, 'Full-time Supervisory'=2, 'Full-time Junior Officer'=4, 'Full-time Senior Officer'=6, 'Full-time Other'=-6, 'Self-employed 0 employee'=-4, 'Self-employed 1-9 employees'=0, 'Self-employed 10+ employees'=4)
score_age <- c('(18, 24]'=-4, '(24, 35]'=0, '(35, 44]'=2, '44+'=0)
score_dep <- c('0'=0, '(0, 1]'=-1, '(1, 4]'=-2, '4+'=-3)
score_cibil <- c('Does not exist '=0, '< 400'=-10, '(400, 800]'=5, '800+'=10)
score_car <- c(No=0, Yes=5)

edu_score <- score_edu[combines$Education]
names(edu_score) <- NULL # remove header

loan_score <- score_loan[combines$Loan.Amount.Monthly.Income]
names(loan_score) <- NULL

res_score <- score_res[combines$Residential.Status]
names(res_score) <- NULL

mar_score <- score_mar[combines$Marital.Status]
names(mar_score) <- NULL

pos_score <- score_pos[combines$Position.Employment.Status]
names(pos_score) <- NULL

age_score <- score_age[combines$Age]
names(age_score) <- NULL

dep_score <- score_dep[combines$Number.Dependants]
names(dep_score) <- NULL

cibil_score <- score_cibil[combines$CIBIL.Score]
names(cibil_score) <- NULL

car_score <- score_car[combines$Car.Registration]
names(car_score) <- NULL

Score <- edu_score + loan_score + res_score + mar_score + pos_score + age_score + dep_score + cibil_score + car_score
hist(Score, xlab = "Score")

library(ggplot2)
Score <- data.frame(Score=Score)
ggplot(Score, aes(x=Score)) + 
    geom_histogram(binwidth=.5, colour="black", fill="white") +  
    geom_vline(aes(xintercept=median(Score, na.rm=T)), color="red", linetype="dashed", size=1) + 
    geom_vline(aes(xintercept=mean(Score, na.rm=T)), color="green", linetype="dashed", size=1)

write.csv(Score,"./score.csv", row.names=FALSE)

combines$Score <- Score

# generate a data file ~ 800M size
write.csv(combines,"./combines_workbook.csv", row.names=FALSE)
