# Data
load("dataset_2012.RData")

df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "relative.incomeproxy" ,
"victim", "corrupt", "spatial.distance" ,
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"))                       
df<-na.exclude(df)


Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)
colnames(Y)<-c("ActMeetMun", "ActContMun", "ActContAut", "ActContGvt",
		"ActSolveProb", "ActMeetImp", "ActMeetPty", "ActVoted",
		"ActShare", "ActSign", "ActProtest", "ActBlock")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1

df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))


df$relative.incomeproxy.centered<-(df$relative.incomeproxy-mean(df$relative.incomeproxy))/(2*sd(df$relative.incomeproxy))


df$spatial.distance.centered<-(df$spatial.distance-mean(df$spatial.distance))/(2*sd(df$spatial.distance))

X<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$relative.incomeproxy.centered, 
	df$victim, df$corrupt, 
	df$spatial.distance.centered,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$social.spending-mean(df$social.spending))/(2*sd(df$social.spending))
)



Matrix.Cov.Beta<-solve(diag(ncol(X))+(6/(pi^2))*crossprod(X))



library(Rcpp)
library(RcppArmadillo)
library(RcppGSL)


Sys.setenv("OMP_NUM_THREADS"="1")
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")



sessionInfo()

sourceCpp("LATAMcontinuous.cpp")

cconv<-rep(0,nrow(Y))
cunconv<-rep(0,nrow(Y))


Results<-mcmc_probit (Y=Y, 
I=as.matrix(Matrix.Country),
tablecountry=table(Country),
Cinit_conv=cconv,  	
Cinit_unconv=cunconv,
alphastart=matrix(0,ncol(Y),ncol(Matrix.Country)),
alphastart_conv= matrix(0,ncol(Y),ncol(Matrix.Country)),
alphastart_unconv=matrix(0,ncol(Y),ncol(Matrix.Country)),
mcmc =400000,
burn=125000,
thin=10,
thin_latent=500,
chains=3
 )



save(Results, file="Results_ContinuousModel")
