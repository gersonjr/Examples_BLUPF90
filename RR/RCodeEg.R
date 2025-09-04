### Add Legendre Polynomials ---- 
time = c(min(FIBWECMWeek2$WeekLact):max(FIBWECMWeek2$WeekLact))
x = tstand(time)
x

#Define the order for the polynomial 
norder = 4
#K matrix (design matrix of the poly effects) ##calculating the values based on the pol equations
K = matrix(0, ncol=(1+norder), nrow=length(x))
K
K[,1] = rep(1, length(x)) # intercept will be one due to x0=x*0+1
K
for(i in 2:ncol(K)){ ### standard time for each x according to regressions equations (linear, quadratic, cubic) example x = x ; x2= x*x ; x3 = x2*x ....
  K[,i]=x^(i-1)
}
K ### K matrix 

legcoef <- legendre.polynomials(n=(norder), normalized=TRUE) ### getting the legendre polynomials using a function from the orthopolynom package
legcoef

coef = matrix(0, ncol=(1+norder), nrow=1+norder)
for (i in 1:nrow(coef)){
  coef[1:i,i]=unlist(polynomial.coefficients(legcoef)[i])
}
coef ### matrix with polynomials coeficients 

#Polynomials values (covariates) for each time (period/level) considering the equations according to the order ( see leg3coef)
dim(K)
dim(coef)
pol.values = K%*%coef #each time from -1 to 1
pol.values
dim(pol.values)

# Add Legendre polynomial terms to data
colnames(pol.values) <- paste0("leg", 0:norder)
df <- data.frame(cbind(sort(unique(FIBWECMWeek2$WeekLact)), pol.values))
names(df)[1] = "WeekLact"
head(df)

# Join with data
FIBWECMWeek3 <- left_join(FIBWECMWeek2, df, by = 'WeekLact')
glimpse(FIBWECMWeek3)

rm(df, legcoef, pol.values, coef, K, norder)
