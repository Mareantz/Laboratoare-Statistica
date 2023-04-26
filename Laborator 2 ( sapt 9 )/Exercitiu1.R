tablou=read.csv("life_expect.csv")
male=tablou[['male']]
female=tablou[['female']]
intervalf=seq(min(female),max(female),length=8)
intervalm=seq(min(male),max(male),length=8)
hist(female,breaks=intervalf,right=F,freq=T)
hist(male,breaks=intervalm,right=F,freq=T)