library(rethinking)

data(Howell1)

df_adultos <- Howell1[Howell1$age >= 18, ]

plot(df_adultos$height ~ df_adultos$weight)
xbar <- mean(df_adultos$weight)

fit_altura <- quap( 
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b * (weight - xbar) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dnorm(0, 10),
    sigma ~ dunif( 0 , 50 )
  ), 
  data = df_adultos
)

precis(fit_altura)

plot( height ~ weight , data=df_adultos , col=rangi2 )
post <- extract.samples( fit_altura )
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(x - xbar) , add=TRUE )