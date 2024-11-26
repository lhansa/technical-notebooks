#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: libs-and-data
library(rethinking)
library(ggplot2)
ggplot2::theme_set(ggplot2::theme_bw())

data(WaffleDivorce)
df_divorce <- WaffleDivorce
head(df_divorce)
#
#
#
#
#
#
#
#| label: pre-procesado
df_divorce$A <- scale(df_divorce$MedianAgeMarriage)
df_divorce$D <- scale(df_divorce$Divorce)
df_divorce$M <- scale(df_divorce$Marriage)

ggplot(df_divorce) + 
  geom_histogram(aes(D), binwidth = 0.3)
#
#
#
#| label: modelo-edad
fit_edad <- rethinking::quap(
    alist(
        D ~ dnorm(mu, sigma), 
        mu <- a + bA * A, 
        a ~ dnorm(0, 0.2),
        bA ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), 
    data = df_divorce
)

precis(fit_edad)
#
#
#
#
#
#
#
#| label: modelo-casados
fit_casados <- quap(
    alist(
        D ~ dnorm(mu, sigma), 
        mu <- a + bM * M,
        bM ~ dnorm(0, 0.5), 
        a ~ dnorm(0, 0.2),
        sigma ~ dexp(1)
    ), 
    data = df_divorce
)

precis(fit_casados)
#
#
#
#
#
#
#
#| label: modelo-ambos
fit_ambas <- quap(
    alist(
        D ~ dnorm(mu, sigma),
        mu  <- a + bA * A + bM * M,
        bA ~ dnorm(0, 0.5),
        bM ~ dnorm(0, 0.5), 
        a ~ dnorm(0, 0.2),
        sigma ~ dexp(1)
    ), 
    data = df_divorce
)

plot(coeftab(fit_edad, fit_casados, fit_ambas), par = c("bA", "bM"))
#
#
#
#
#
#
#
#
#| label: modelo-edad-casados
fit_edad_casados <- quap(
    alist(
        M ~ dnorm(mu, sigma), 
        mu <- a + BA * A, 
        bA ~ dnorm(0, 0.5), 
        a ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), 
    data = df_divorce
)

precis()
#
#
#
#
#
#
