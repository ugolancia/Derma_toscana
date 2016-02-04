

setwd("~/Desktop/Github/Derma_toscana/2016_02_03_Revisione/Code")
library(xlsx)

psor<- read.xlsx("DB_derma_eng.xlsx", 1, stringsAsFactors = FALSE)
names(psor)[18]<- "arthritis"
names(psor)[4]<- "age"

library(ggplot2)
g<- ggplot(psor, aes(x=arthritis, y=age, fill = arthritis)) + geom_boxplot() + ggtitle("Figure 1. Age in groups without and with arthritis")
g

dev.copy(jpeg, "fig_1.jpg", quality=300, width = 529, height=305)
dev.off()


#let's try another way to create an high resolution graph

png("fig_1_bis.jpg", width = 6, height = 6, units = 'in', res = 600)
g<- ggplot(psor, aes(x=arthritis, y=age, fill = arthritis)) + geom_boxplot() + ggtitle("Figure 1. Age in groups without and with arthritis")
g
dev.off()

# first figure boxplot età ~ diagnosis
##boxplot(psor$età~psor$arthritis, col = "red")


#figure 2

##names(psor)[13]<- "nails"
##ggplot(psor, aes(nails, arthritis)) + geom_bar()

mosaicplot(table(psor[, 13], psor[, 18]), main = "Figure 2. Presence of arthritis (horizontal) 
and of ungueal psoriasis (vertical)", col = c("red", "green"))  

dev.copy(jpeg, "figure 2.jpeg", quality=300, width = 529, height=305)
dev.off()


#figure 3
mosaicplot(table(psor[, 16], psor[, 18]), main = "Figure 3. Arthritis diagnosis (horizontal) and
    psoriasis on more than 50 % of body (vertical)", col = c("red", "green"))
dev.copy(jpeg, "figure 3.jpeg", quality=300, width = 529, height=305)
dev.off()


dim(psor)fo
names(psor)
str(psor)

#let's recode si and no in SI and NO

## no more necessary  psor[,18]<- as.character(psor[,18])
names(psor)[18]<- "artrite"
psor$artrite[psor$artrite== "si"]<- "SI"
psor$artrite[psor$artrite== "no"]<- "NO"
table(psor[,18])

##NO  SI 
##102  32 

#create a subset with artrite si

psorart<- subset(psor, psor$arthritis == "YES")

table(psorart[, 7])

psorart$psoriasi.a.placche[psorart$psoriasi.a.placche == "si"]<- "SI"
table(psorart[, 8])
NO si SI 
1  2 29

psorartno<- subset(psor, psor$arthritis == "NOT")

dia_art<- psorart[, 19]
dia_psor<- psorart[, 6]
time_lapse<- dia_art-dia_psor
time<- time_lapse[!is.na(time_lapse)]


time<- as.data.frame(time)
ggplot(time, aes(x = factor(0), y = time)) + geom_boxplot() + xlab("")+ scale_x_discrete(breaks = NULL) + coord_flip() + ggtitle("Figure 4. Time between diagnosis of psoriasis and of arthritis (years)")

dev.copy(jpeg, "figure 4.jpeg", quality=300, width = 529, height=305)
dev.off()






par(bg = "gray90")
boxplot(time, col= "red")
length(time)
summary(time)
diff_date<- psorart[, c(6, 19)]
diff_date_comp<- diff_date$data.diagnosi.1-diff_date$data.diagnosi
x<- cbind(diff_date, diff_date_comp)

identical(x[, 3], time_lapse)
TRUE


colSums(is.na(psor))
#after col 26 huge amount of missing data

psor<- psor[, 1:26]
names(psor)

#arthritis symptoms
colSums(is.na(psor))
names(psor)[20]<- "dolore lombo-crurale" 
names(psor)[21]<- "dolore/gonfiore al mattino"
names(psor)[22]<- "dolore/gonfiore articolazioni"
names(psor)[23]<- "dito molto gonfio"
names(psor)[24]<- "dolore al calcagno"

table(psor$artrite, psor[, 20], useNA="ifany")

psor[, 20][psor[, 20]== "si"]<- "SI"
psor[, 20][psor[, 20]== "no"]<- "NO"
table(psor$artrite, psor[, 20], useNA="ifany")

psorart[, 20][psorart[, 20]== "si"]<- "SI"
psorart[, 20][psorart[, 20]== "no"]<- "NO"
table(psorart[, 20], useNA="ifany")
table(psorart[, 20], useNA="ifany")/32

psor[, 21][psor[, 21]== "si"]<- "SI"
psor[, 21][psor[, 21]== "no"]<- "NO"
table(psor$artrite, psor[, 21], useNA="ifany")

psorart[, 21][psorart[, 21]== "si"]<- "SI"
psorart[, 21][psorart[, 21]== "no"]<- "NO"
table(psorart[, 21], useNA="ifany")/32

psorart[, 22:24][psorart[, 22:24]== "si"]<- "SI"
psorart[, 22:24][psorart[, 22:24]== "no"]<- "NO"
table(psorart[, 22], useNA="ifany")/32
table(psorart[, 22], useNA="ifany")

table(psorart[, 23], useNA="ifany")/32
table(psorart[, 23], useNA="ifany")

table(psorart[, 24], useNA="ifany")/32
table(psorart[, 24], useNA="ifany")

psorartno<- subset(psor, psor$artrite == "NO")
psorartno[, 20:24][psorartno[, 20:24]== "si"]<- "SI"
psorartno[, 20:24][psorartno[, 20:24]== "no"]<- "NO"
table(psorartno[, 20], useNA="ifany")/102
table(psorartno[, 20], useNA="ifany")
table(psorartno[, 21], useNA="ifany")/102
table(psorartno[, 21], useNA="ifany")
table(psorartno[, 22], useNA="ifany")/102
table(psorartno[, 22], useNA="ifany")
table(psorartno[, 23], useNA="ifany")/102
table(psorartno[, 23], useNA="ifany")
table(psorartno[, 24], useNA="ifany")/102
table(psorartno[, 24], useNA="ifany")

table(psorartno[, 23:24], useNA="ifany")/102
table(psorartno[, 24], useNA="ifany")


#let's look at the age in the two group
summary(psorart[, 4])
sd(psorart[, 4], na.rm=TRUE)
summary(psorartno[, 4])
sd(psorartno[, 4], na.rm=TRUE)

#let's do a subset leaving out columns from 27 that are most missing data

psor<- psor[ , 1:26]
names(psor)
summary(psor$PASI)
#obviously there is a mistake because 90 is not possible as a score

psor$PASI[psor$PASI>50] <- NA

psor[, 18]

psor$arthritis[psor$arthritis == "YES"]<- 1
psor$arthritis[psor$arthritis == "NOT"]<- 0
psor$arthritis<- as.numeric(psor$arthritis)
fit<- glm(arthritis~PASI, data = psor, family = binomial())
plot(fit)
boxplot(psor$PASI~psor$arthritis)
table(psor$PASI, psor$arthritis)