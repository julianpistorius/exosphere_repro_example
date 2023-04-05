# Example of rjags use from following tutorial: 
# http://rstudio-pubs-static.s3.amazonaws.com/15236_9bc0cd0966924b139c5162d7d61a2436.html

library(rjags)

sigma     <- c(15,10,16,11, 9,11,10,18)
schoolobs <- c(28,8, -3, 7,-1, 1,18,12)

model.sat.text<-"
  model {
    for(i in 1:N) {
    schoolmean[i] ~ dnorm(mu,itau)
    thes[i] <- 1/pow(sigma[i],2)
    schoolobs[i] ~ dnorm(schoolmean[i],thes[i])
    }
 
  mu ~ dnorm(0,alpha)
  alpha <- .01
  itau   ~ dgamma(1e-3,pow(15,2)*1e-3)
  tau <- pow(1/itau,1/2)
}
"
model.sat.spec<-textConnection(model.sat.text)

sat.jags <- jags.model(model.sat.spec,
                       data=list('sigma'=sigma,
                                 'schoolobs'=schoolobs,
                                 'N'=length(schoolobs)
                       ),
                       n.adapt = 1000)

samps.jags <- jags.samples(sat.jags,
                           c('mu','tau'),
                           n.iter=10000,
                           thin=10
)

samps.coda <- coda.samples(sat.jags,
                           c('mu','tau', 'schoolmean'),
                           n.iter=10000,
                           thin=10
)

plot(samps.coda[[1]][,c("mu","tau")])
