#if you run this I have no idea if it is deleting the correct columns, etc. Messy stuff here. 

col = read.csv("colleges.csv")
df = ipeds = read.csv("ipeds.csv")

ipeds$unitid

col$ipeds_id

df = df[-c(8, 22)]

df_save = df

df_final = df

#col = col[,-c(6)] #did this for notes, county, city, date.

library(dplyr)

#rename column by name
df <- df %>% rename_at('IC2021.Institutional.control.or.affiliation', ~'control')
df <- df %>% rename_at('HD2021.FIPS.state.code', ~'FIPS.state.code')

df <- df %>% rename_at('unitid', ~'ipeds_id')


df <- df %>% rename_at('IC2021.Religious.affiliation', ~'religious.affiliation')
df <- df %>% rename_at('DRVIC2021.Tuition.and.fees..2020.21', ~'tuition')

df <- df %>% rename_at('DRVEF122021.Total.12.month.unduplicated.headcount', ~'total.headcount')
df <- df %>% rename_at('DRVEF122021.Undergraduate.12.month.unduplicated.headcount', ~'undergrad.headcount')

df <- df %>% rename_at('DRVEF122021.Percent.of.12.month.unduplicated.headcount.that.are.American.Indian.or.Alaska.Native', ~'percent.american.native')
df <- df %>% rename_at('DRVEF122021.Percent.of.12.month.unduplicated.headcount.that.are.Asian', ~'percent.asian')
df <- df %>% rename_at('DRVEF122021.Percent.of.12.month.unduplicated.headcount.that.are.Black.or.African.American', ~'percent.black')

df <- df %>% rename_at('DRVEF122021.Percent.of.12.month.unduplicated.headcount.that.are.Hispanic.Latino', ~'percent.hispanic.latino')

df <- df %>% rename_at('DRVEF122021.Percent.of.12.month.unduplicated.headcount.that.are.Native.Hawaiian.or.Other.Pacific.Islander', ~'percent.pacific.islander')

df <- df %>% rename_at('DRVEF122021.Percent.of.12.month.unduplicated.headcount.that.are.White', ~'percent.white')

df <- df %>% rename_at('DRVEF122021.Percent.of.12.month.unduplicated.headcount.that.are.two.or.more.races', ~'percent.two.more.races')

df <- df %>% rename_at('DRVEF122021.Percent.of.12.month.unduplicated.headcount.that.are.race.ethnicity.unknown', ~'percent.NA.race')

df <- df %>% rename_at('DRVEF122021.Percent.of.12.month.unduplicated.headcount.that.are.Nonresident.Alien', ~'percent.nonres.alien')

df <- df %>% rename_at('DRVEF122021.Percent.of.12.month.unduplicated.headcount.that.are.women', ~'percent.women')

df <- df %>% rename_at('SFA2021.Average.amount.of.grant.and.scholarship.aid.awarded..2020.21', ~'avg.grant.money')

df <- df %>% rename_at('DRVGR2021.Graduation.rate..total.cohort', ~'grad.rate')
df <- df %>% rename_at('SFA2021.Percent.of.full.time.first.time.undergraduates.awarded.any.financial.aid', ~'percent.fin.aid')

df <- df %>% rename_at('SFA2021.Percent.of.full.time.first.time.undergraduates.awarded.student.loans', ~'percent.student.loan')

df <- df %>% rename_at('IC2021.Occupational', ~'occupational.degree')

df <- df %>% rename_at('IC2021.Academic', ~'academic.degree')

df <- df %>% rename_at('IC2021.Adult.basic.remedial.or.high.school.equivalent', ~'hs.equivelant.degree')


df <- df %>% rename_at('IC2021.Percent.of.undergraduates..who.are.formally.registered.as.students.with.disabilities..when.percentage.is.more.than.3.percent', ~'percent.disability')
df <- df %>% rename_at('IC2021.NCAA.NAIA.conference.number.football', ~'NCAA.football')
df <- df %>% rename_at('IC2021.Institution.provide.on.campus.housing', ~'on.campus.housing')
df <- df %>% rename_at('IC2021.Total.dormitory.capacity', ~'dorm.capacity')

df <- df %>% rename_at('IC2021.Typical.room.charge.for.academic.year', ~'dorm.room.price')

# todo: 
# delete from ipeds: state code, year, 21-22 fees
# delete from cols: 

deleted_inst = md[md$control == "",]

md = dataframe_AB = merge(df, col, by="ipeds_id")

# remove duplicated state, institution name
md = md[,-c(3, 32,33)]

md_save = md

#remove institutions with no ipeds data (a lot of NAs)
deleted_inst = md[md$control == "",]
md = md[-c(586, 622, 958, 986, 1488, 1781, 1799, 1845),]


md$religious = "Yes"
md[md$religious.affiliation == "Not applicable",]$religious <- "No"

md$catholic.religious = "Non-Catholic"
md[md$religious.affiliation == "Not applicable",]$catholic.religious <- "No"
md[md$religious.affiliation == "Roman Catholic",]$catholic.religious <- "Yes"

md[md$control == "Private not-for-profit (no religious affiliation)",]$control <- "Private not-for-profit"
md[md$control == "Private not-for-profit (religious affiliation)",]$control <- "Private not-for-profit"

for_lm = md

for_lm$religious = as.factor(for_lm$religious)
for_lm$control = as.factor(for_lm$control)
for_lm$catholic.religious = as.factor(for_lm$catholic.religious)

lm1 = lm(cases ~ religious, for_lm)
summary(lm1)

lm2 = lm(cases ~ control + catholic.religious + tuition + undergrad.headcount, for_lm)
summary(lm2)

write.csv(md,'merged_cases.csv')