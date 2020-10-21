library(grt)
source("gcjc.R")
source("plot.gcjc.R")

##### define logLik for grg #####
# you need this for the codes below
# to work in some machine
logLik.grg <- function(object, ...)
{
  val <- object$logLik
  class(val) <- "logLik"
  val
}
##################################

cnam <- c('Subj', 'Trial','Block', 'Perf', 'Cat', 'Resp','Stim')


dat <- read.table("ERP_DATA_ALL.txt", col.names=cnam)
#dat$Resp <- factor(dat$Resp)
#dat$Cat <- factor(dat$Cat)
bysbj_mean <- with(dat, tapply(Perf, Subj, mean))
CJ <- read.table("CJ.txt", col.names=c("freq","ori"))
CJ$Stim <- 1:nrow(CJ)
dat <- merge(dat, CJ)


perBlock <- aggregate(Perf ~ Subj + Block, data=dat, mean)
perBlockW <- reshape(perBlock, idvar=c("Subj"),timevar="Block", direction="wide")

blocks <- split(dat, dat$Block)
bysbj <- split(blocks[[6]], blocks[[6]]$Subj)



foo <- glc(Resp ~ freq + ori, category=bysbj[[1]]$Cat, data=bysbj[[1]])

blocks <- levels(factor(dat$Block))
sbj <- levels(factor(dat$Subj))
models <- c("Orientation", "Frequency", "Information Integration", "Conjunctive Rule", "Fixed Random", "General Random")
res <- matrix(nrow=length(sbj) * length(blocks),ncol=(length(models) * 2 + 2))
res <- cbind(expand.grid(sbj,blocks), res)
colnames(res)[1:2] <- c("Subj","Block")
pdf(file="test_individual_plots.pdf", width=12, height=9)
op <- par(mfrow = c(3, 4), pty = "m")

for(i in sbj)
{
  for(j in blocks)
  {
    data <- subset(dat, dat$Block == j & dat$Subj == i)
    fit.ori <- glc(Resp ~ ori, category=data$Cat, data=data)
    fit.freq <- glc(Resp ~ freq, category=data$Cat, data=data)
    fit.2d <- glc(Resp ~ freq + ori, category=data$Cat, data=data)
    fit.cj <- gcjc(Resp ~ freq + ori, category=data$Cat, data=data, config=1, zlim=7)
    
    fit.frg <- grg(data$Resp, fixed=T)
    fit.grg <- grg(data$Resp, fixed=F)
    AICs <- AIC(fit.ori, fit.freq, fit.2d, fit.cj, fit.frg, fit.grg)
    BICs <- AIC(fit.ori, fit.freq, fit.2d, fit.cj, fit.frg, fit.grg, k=log(80))
    winner <- which.min(AICs$AIC)
    winner2 <- which.min(BICs$AIC)
    res[res$Subj==i & res$Block == j,3:ncol(res)] <- c(round(AICs$AIC,2), models[winner], round(BICs$AIC,2), models[winner2])
  ##### now plot'em ###RACHEL CHANGED 'winner' to 'winner2' to reflect BIC instead of AIC
    col <- rep("black",4)
    col[winner2] <- "red"  
    bg <- c("white","gray")[factor(data$Resp)]
    pch <- c(21,24)[factor(data$Cat)]
    title <- paste(i, ": Block", j, ": ", models[winner2])
    plot(ori ~ freq, data=data, xlab="Frequency", ylab="Orientation", bg=bg, pch=pch, xlim=c(200,400), ylim=c(0, 200), main=title)
    abline(h=coef(fit.ori), col = col[1])
    abline(v=coef(fit.freq), col = col[2])
    abline(coef(fit.2d), col = col[3])
    cj.coef <- coef(fit.cj)
    abline(h=cj.coef[["ori"]], col = col[4], lty = 3)
    abline(v=cj.coef[["freq"]], col = col[4], lty = 3)
  }
}

dev.off()


res.nam <- c("ori","freq","2d","cj","frg","grg","winner")
colnames(res)[3:16] <- c(paste(res.nam, "AIC", sep="."), paste(res.nam, "BIC", sep="."))




#### Just type in 'res' once the file has been run to receive the summary file with the fit values###