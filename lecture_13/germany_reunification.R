# OK. Much better example of how to do a synthetic control
# Author: Bruno Ponne
# Link with lots of explanantion here: https://www.r-bloggers.com/2023/07/when-numbers-meet-stories-an-introduction-to-the-synthetic-control-method-in-r/
# Clear memory and load our packages.
rm(list=ls())
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2025/COMPSS_201_2025/lecture_13")

library(pacman)
p_load(Synth,SCtools,tidyverse,skimr,foreign)

# We are replicating the one of the OGs
d <- read.dta("repgermany.dta")

dataprep_out <-
  dataprep(
    foo = d,
    predictors    = c("gdp","trade","infrate"),
    dependent     = "gdp",
    unit.variable = "index",
    time.variable = "year",
    special.predictors = list(
      list("industry" ,1981:1990, c("mean")),
      list("schooling",c(1980,1985), c("mean")),
      list("invest80" ,1980, c("mean"))
    ),
    treatment.identifier = 7,
    controls.identifier = unique(d$index)[-7],
    time.predictors.prior = 1981:1990,
    time.optimize.ssr = 1960:1989,
    unit.names.variable = "country",
    time.plot = 1960:2003
  )
synth_out <- synth(dataprep_out)

path.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          tr.intake = 1990,
          Ylab = "Per capita GDP",
          Xlab = "Year",
          Legend = c("West Germany", "Synthetic West Germany"),
          Main = "West Germany vs Synthetic West Germany")

gaps.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          tr.intake = 1990,
          Ylab = "Effect",
          Xlab = "Year",
          Main = " Gap between per capita GDP in West Germany and its synthetic version")


# Little placebo test (You can mess with this. I started from scratch to keep things clean, but 
# this is overkill. Key is the dataprep block and dropping Germany from the donor pool.)
rm(list=ls())
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2025/COMPSS_201_2025/lecture_13")

library(pacman)
p_load(Synth,SCtools,tidyverse,skimr,foreign)

# We are replicating the one of the OGs
d <- read.dta("repgermany.dta")
all_units <- unique(d$index)
# Dropping Germany
d2 <- d[d$index != 7, ]
dataprep_out2 <-
  dataprep(
    foo = d,
    predictors    = c("gdp","trade","infrate"),
    dependent     = "gdp",
    unit.variable = "index",
    time.variable = "year",
    special.predictors = list(
      list("industry" ,1981:1990, c("mean")),
      list("schooling",c(1980,1985), c("mean")),
      list("invest80" ,1980, c("mean"))
    ),
    treatment.identifier  = 19,
    controls.identifier   = setdiff(all_units, 19),  # <- this is the crucial change
    time.predictors.prior = 1981:1990,
    time.optimize.ssr     = 1960:1989,
    unit.names.variable   = "country",
    time.plot             = 1960:2003
  )

synth_out2 <- synth(dataprep_out2)


path.plot(synth.res = synth_out2,
          dataprep.res = dataprep_out2,
          tr.intake = 1990,
          Ylab = "Per capita GDP",
          Xlab = "Year",
          Legend = c("Spain", "Synthetic Spain"),
          Main = "Placebo (Spain)")

gaps.plot(synth.res = synth_out2,
          dataprep.res = dataprep_out2,
          tr.intake = 1990,
          Ylab = "Effect",
          Xlab = "Year",
          Main = " Gap between per capita GDP in Spain and its synthetic version")

# Now doing some fancy plots. This takes a while to run as we are looping over countries. 
rm(list=ls())
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2025/COMPSS_201_2025/lecture_13")

library(pacman)
p_load(Synth,SCtools,tidyverse,skimr,foreign)

# We are replicating the one of the OGs
d <- read.dta("repgermany.dta")
# I am annoyed by the index variable. I am going to create one that starts at 1 and increases by
# one for each country. Pedantic. I know. 
d$old_index <-d$index
d$index <- match(d$old_index, unique(d$old_index))
all_units <- unique(d$index)
store <- matrix(NA,length(1960:2003),17)
colnames(store) <- unique(d$country)


#store <- matrix(NA,length(1955:1997),17)
#colnames(store) <- unique(basque$regionname)[-1]

# run placebo test
for(iter in 1:17)
{  
dataprep.out <-  dataprep( foo = d,
predictors    = c("gdp","trade","infrate"),
dependent     = "gdp",
unit.variable = "index",
time.variable = "year",
special.predictors = list(
  list("industry" ,1981:1990, c("mean")),
  list("schooling",c(1980,1985), c("mean")),
  list("invest80" ,1980, c("mean"))),
treatment.identifier = iter,    
controls.identifier   = setdiff(all_units, iter),  # <- this is the crucial change
time.predictors.prior = 1981:1990,
time.optimize.ssr = 1960:1989,
unit.names.variable = "country",
time.plot = 1960:2003
)
   # run synth
  synth.out <- synth(
    data.prep.obj = dataprep.out,
    method = "BFGS"
  )
  
  # store gaps
  store[,iter] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

# Now we are going to make the Spagetti Figure. 
data <- store
rownames(data) <- 1960:2003

# Set bounds in gaps data
gap.start     <- 1
gap.end       <- nrow(data)
years         <- 1960:2003
gap.end.pre  <- which(rownames(data)=="1989")

#  MSPE Pre-Treatment
mse        <-             apply(data[ gap.start:gap.end.pre,]^2,2,mean)
germany.mse <- as.numeric(mse[7])
# Exclude states with 5 times higher MSPE than Germany
data <- data[,mse<5*germany.mse]
Cex.set <- .75

# Plot
plot(years,data[gap.start:gap.end,which(colnames(data)=="West Germany")],
     ylim=c(-4000,3000),
     xlab="year",
     xlim=c(1960,2003),ylab="Gap in real GDPpc",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")

# Add lines for control states
for (i in 1:ncol(data)) { lines(years,data[gap.start:gap.end,i],col="gray") }

## Add Germany Line
lines(years,data[gap.start:gap.end,which(colnames(data)=="West Germany")],lwd=2,col="black")

# Add grid
abline(v=1989,lty="dotted",lwd=2)
abline(h=0,lty="dashed",lwd=2)
legend("bottomright",legend=c("West Germany","control regions"),
       lty=c(1,1),col=c("black","gray"),lwd=c(2,1),cex=.8)
arrows(1989,-1.5,1989.5,-1.5,col="black",length=.1)
text(1989.5,-1.5,"TReunification",cex=Cex.set)
abline(v=1960)
abline(v=2003)
abline(h=-2)
abline(h=2)

