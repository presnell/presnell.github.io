### R code from vignette source 'icda-notes.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: icda-notes.Rnw:24-26
###################################################
bnw <- FALSE               # Change to TRUE for black and white slides
# bnw <- TRUE


###################################################
### code chunk number 2: icda-notes.Rnw:40-58
###################################################
## options(width=55, continue=" ", digits=5, show.signif.stars=FALSE, 
##         device.ask.default=FALSE, contrasts=c("contr.SAS","contr.poly"))
options(width=55, continue="  ", digits=5, show.signif.stars=FALSE, 
        device.ask.default=FALSE)
homepage <- "file:///home/presnell/public_html/Courses/sta4504-2012sp/"
options(SweaveHooks=list(
          fig=function() {
            par(mar=c(4,4,1,1)+0.1, mgp=c(2.5,0.75,0), oma=c(0,0,0,0)+0.1,
                cex=1.2, cex.axis=1.2, cex.lab=1.2, cex.main=1.2, cex.sub=1.2,
                fg = fgcol, col.axis = fgcol, col.lab = fgcol,
                col.main = fgcol, col.sub = fgcol)
            pdf.options(fg=fgcol)
          },
          cache=function() {setCacheDir("./SweaveCache")}
          )
        )
srcdir <- paste(homepage, "R/Src/", sep="")
datadir <- paste(homepage, "Data/", sep="")


###################################################
### code chunk number 3: icda-notes.Rnw:61-76
###################################################
source("beamer_colors.R")
if (bnw) {
  fgcol <- "black"
  mycolors <- rainbow(7)
} else {
  fgcol <- beamer[["albatrossText"]]
  mycolors <-
    unlist(beamer[c("albatrossText",
                    "albatrossHeaderText",
                    "albatrossAlertBlockHeaderText",
                    "albatrossExampleBlockHeaderText",
                    "albatrossAddedWhite",
                    "albatrossAddedGreen",
                    "albatrossAddedYellow")])
}


###################################################
### code chunk number 4: icda-notes.Rnw:176-205
###################################################
## Something for breaking up long output into "paragraphs".
paragraphs <- function(x) {
  text <- capture.output(x)
  blanklines <- (1:length(text))[text == ""]
  if (blanklines[1] != 1) blanklines <- c(-1,blanklines)
  parstarts <- blanklines + 1
  parends <- c(blanklines[-1] - 1, length(text))
  npar <- length(parstarts)
  res <- list()
  for (i in 1:npar) res <- c(res, list(text[parstarts[i]:parends[i]]))
  class(res) <- c("paragraphs", res[["class"]])
  res
}
as.paragraphs <- paragraphs
assign("[.paragraphs",
       function(x, ..., drop = TRUE) {
         cl <- oldClass(x)
         class(x) <- NULL
         val <- NextMethod("[")
         class(val) <- cl
         val
       })
print.paragraphs <- function(x, ...) {
  for (i in 1:(length(x))) {
    cat(x[[i]], sep="\n")
    cat("\n")
  }
  invisible(x)
}


###################################################
### code chunk number 5: icda-notes.Rnw:209-213
###################################################
## Something to print the sign of a number as a plus or minus.
signpm <- function(x) if(x < 0) "-" else "+"
roundabs <- function(x, digits) round(abs(x), digits)
signednum <- function(x, digits) sprintf(paste("%+.", digits, "f", sep=""), x)


###################################################
### code chunk number 6: icda-notes.Rnw:222-275
###################################################
## coef.ltx <- function(x, cmdname="coef", renew=(cmdname=="coef"), ...) {
##   fx <- formatC(x, ...)
##   cat(paste(if (renew) "\\renewcommand" else "\\newcommand",
##             "{\\", cmdname, "}[1]{%\n",
##             paste("  \\ifthenelse{#1=", 1:length(fx), "}{", fx, "}{}%\n",
##                   sep="", collapse=""),
##             "}\n", sep=""))
## }
coef.ltx <- function(x, cmdname="coef", renew=(cmdname=="coef"), ...) {
  xsgn <- ifelse(x < 0, "-", "")  # normal display of x
  nsgn <- ifelse(x > 0, "-", "")  # normal display of negative of x
  ssgn <- ifelse(x < 0, "-", "+") # always show sign
  osgn <- ifelse(x > 0, "-", "+") # always show opposite sign
  xabs <- formatC(abs(x), ...)
  cat(paste(if (renew) "\\renewcommand" else "\\newcommand",
            "{\\", cmdname, "}[2][]{%\n",
            paste("  \\ifthenelse{#2=", 1:length(x),
                  "}{\\ifthenelse{\\equal{#1}{a}}{",
                  "}{\\ifthenelse{\\equal{#1}{n}}{", nsgn,
                  "}{\\ifthenelse{\\equal{#1}{s}}{", ssgn,
                  "}{\\ifthenelse{\\equal{#1}{o}}{", osgn,
                  "}{", xsgn, "}}}}", xabs, "}{}%\n",
                  sep="", collapse=""),
            "}\n", sep=""))
}
ecoef.ltx <- function(x, cmdname="ecoef", renew=(cmdname=="ecoef"), ...) {
  cat(paste(if (renew) "\\renewcommand" else "\\newcommand",
            "{\\", cmdname, "}[2][]{%\n",
            paste("  \\ifthenelse{#2=", 1:length(x),
                  "}{\\ifthenelse{\\equal{#1}{n}}{",
                  formatC(exp(-x), ...), "}{",
                  formatC(exp(x), ...), "}}{}%\n",
                  sep="", collapse=""),
            "}\n", sep=""))
}
lp.ltx <- 
  function(x, digits, cmdname="linpred", renew=(cmdname=="linpred"), ...) {
    xlen <- length(x)
    if (length(digits) < xlen) digits <- rep(digits, length.out=xlen)
    fx <- formatC(x[1], digits=digits[1], ...)
    for (i in 2:xlen) {
      fx <-
        c(fx, 
          paste(signpm(x[i]), formatC(abs(x[i]), digits=digits[i], ...),
                sep=""))
    }
    cat(paste(if (renew) "\\renewcommand" else "\\newcommand",
              "{\\", cmdname, "}[",
              xlen, "]{\n",
              paste("  \\ifthenelse{\\equal{#", 1:length(fx), "}{}}{}{",
              fx, " {#", 1:length(fx), "}}\n", sep="", collapse=""),
              "}", sep=""))
  }


###################################################
### code chunk number 7: icda-notes.Rnw:333-338
###################################################
library(icda)
library(Hmisc)
### Fixing two bugs in Hmisc's latex.default function.
source("latex.s")
library(xtable)


###################################################
### code chunk number 8: icda-notes.Rnw:479-483
###################################################
dbinom(0, 3, .6)
dbinom(1, 3, .6)
dbinom(0:3, 3, .6)
cbind(0:3, dbinom(0:3, 3, .6))


###################################################
### code chunk number 9: icda-notes.Rnw:490-492
###################################################
plot(0:3, dbinom(0:3, 3, .6), type = "h",
     xlab = "y", ylab = "P(y)")


###################################################
### code chunk number 10: icda-notes.Rnw:525-531
###################################################
opar <- par(mfrow=c(1,2))
plot(0:8, dbinom(0:8,8,.2), type="h", xlab="y", ylab="p(y)",
main=expression(paste("Binomial(n=8, ",pi,"=.2)")))
plot(0:15, dbinom(0:15,25,.2), type="h", xlab="y", ylab="p(y)",
main=expression(paste("Binomial(n=25, ",pi,"=.2)")))
par(opar)


###################################################
### code chunk number 11: icda-notes.Rnw:566-567 (eval = FALSE)
###################################################
## curve(dbinom(1,3,x), xlim = c(0,1))


###################################################
### code chunk number 12: icda-notes.Rnw:570-573
###################################################
curve(dbinom(1,3,x), xlim = c(0,1),
xlab=expression(paste("Binomial parameter: ",pi)),
ylab=expression(paste("Likelihood:  ", l(pi))))


###################################################
### code chunk number 13: icda-notes.Rnw:596-601
###################################################
curve(dbinom(1,3,x), xlim = c(0,1), xaxt="n",
      xlab=expression(paste("Binomial parameter: ",pi)),
      ylab=expression(paste("Likelihood: ", l(pi))))
axis(1, at=c(0,1/3,1), labels=c("0","1/3","1"))
segments(1/3, 0, 1/3, dbinom(1, 3, 1/3), lty=2)


###################################################
### code chunk number 14: icda-notes.Rnw:615-619
###################################################
curve(dbinom(0,3,x), xlim = c(0,1),
      xlab=expression(paste("Binomial parameter: ",pi)),
      ylab=expression(paste("Likelihood: ", l(pi))))
segments(0, 0, 0, dbinom(0, 3, 0), lty=2)


###################################################
### code chunk number 15: icda-notes.Rnw:632-637
###################################################
curve(dbinom(2,3,x), xlim = c(0,1), xaxt="n",
      xlab=expression(paste("Binomial parameter: ",pi)),
      ylab=expression(paste("Likelihood: ", l(pi))))
axis(1, at=c(0,2/3,1), labels=c("0","2/3","1"))
segments(2/3, 0, 2/3, dbinom(2, 3, 2/3), lty=2)


###################################################
### code chunk number 16: icda-notes.Rnw:648-652
###################################################
curve(dbinom(3,3,x), xlim = c(0,1),
      xlab=expression(paste("Binomial parameter: ",pi)),
      ylab=expression(paste("Likelihood: ", l(pi))))
segments(1, 0, 1, dbinom(3, 3, 1), lty=2)


###################################################
### code chunk number 17: icda-notes.Rnw:885-886
###################################################
prop.test(0,20)


###################################################
### code chunk number 18: icda-notes.Rnw:895-896
###################################################
prop.test(0, 20, correct=FALSE)


###################################################
### code chunk number 19: icda-notes.Rnw:907-908
###################################################
binom.test(0,20)


###################################################
### code chunk number 20: phsdata
###################################################
phs <- matrix(c(189, 10845, 104, 10933), ncol = 2, byrow = TRUE)
dimnames(phs) <- 
  list(Group = c("Placebo","Aspirin"), MI = c("Y", "N"))


###################################################
### code chunk number 21: phstab
###################################################
latex(phs, file="", first.hline.double=FALSE,
      rowlabel=names(dimnames(phs))[1], 
      ## rgroup=names(dimnames(phs))[1], n.rgroup=dim(phs)[1],
      cgroup=names(dimnames(phs))[2], n.cgroup=dim(phs)[2],
      cgroupTexCmd="normalfont"
      )


###################################################
### code chunk number 22: phstabwtot
###################################################
latex(cbind(phs, Total = margin.table(phs, 1)),
      file="", first.hline.double=FALSE,
      rowlabel=names(dimnames(phs))[1], 
      cgroup=c(names(dimnames(phs))[2],""),
      n.cgroup=c(dim(phs)[2],1),
      cgroupTexCmd="normalfont"
      )


###################################################
### code chunk number 23: icda-notes.Rnw:964-971
###################################################
latex(format.df(cbind(prop.table(phs, 1), Total = c(1,1)), cdec=c(3,3,0)),
      file="", first.hline.double=FALSE,
      rowlabel=names(dimnames(phs))[1], 
      cgroup=c(names(dimnames(phs))[2],""),
      n.cgroup=c(dim(phs)[2],1),
      cgroupTexCmd="normalfont"
      )


###################################################
### code chunk number 24: phscalc
###################################################
phs.test <- prop.test(phs, correct=FALSE)
p <- phs.test[["estimate"]]
n <- margin.table(phs, 1)
diffp <- p[1] - p[2]
SE.diffp <- sqrt(sum(p*(1-p)/n))
CI.diffp <- phs.test$conf.int
rr <- p[1]/p[2]
SE.logrr <- sqrt(sum((1-p)/(n*p)))
CI.rr <- exp(log(rr) + c(-1,1) * qnorm(.975) * SE.logrr)
odds <- p/(1-p)
or <- odds[1]/odds[2]
lor <- log(or)
SE.lor <- sqrt(sum(1/phs))
CI.lor <- lor + c(-1,1)*qnorm(.975)*SE.lor
CI.or <- exp(CI.lor)


###################################################
### code chunk number 25: icda-notes.Rnw:1187-1194
###################################################
## Rounding for printing purposes
p <- round(p, 3)
diffp <- round(diffp, 3)
CI.diffp <- round(CI.diffp, 3)
rr <- round(rr, 2)
CI.rr <- round(CI.rr, 2)
or <- round(or, 2)


###################################################
### code chunk number 26: icda-notes.Rnw:1340-1341
###################################################
latex(cbind(phs, Total = margin.table(phs, 1)),
      file="", first.hline.double=FALSE,
      rowlabel=names(dimnames(phs))[1], 
      cgroup=c(names(dimnames(phs))[2],""),
      n.cgroup=c(dim(phs)[2],1),
      cgroupTexCmd="normalfont"
      )


###################################################
### code chunk number 27: icda-notes.Rnw:1344-1349
###################################################
## Recalculating and rounding for printing purposes
phs.test <- prop.test(phs, correct=FALSE)
p <- phs.test[["estimate"]]
n <- margin.table(phs, 1)
diffp <- p[1] - p[2]
SE.diffp <- sqrt(sum(p*(1-p)/n))
CI.diffp <- phs.test$conf.int
rr <- p[1]/p[2]
SE.logrr <- sqrt(sum((1-p)/(n*p)))
CI.rr <- exp(log(rr) + c(-1,1) * qnorm(.975) * SE.logrr)
odds <- p/(1-p)
or <- odds[1]/odds[2]
lor <- log(or)
SE.lor <- sqrt(sum(1/phs))
CI.lor <- lor + c(-1,1)*qnorm(.975)*SE.lor
CI.or <- exp(CI.lor)
p <- round(p, 4)
odds <- round(odds, 4)
or <- round(or, 2)


###################################################
### code chunk number 28: icda-notes.Rnw:1419-1427
###################################################
tphs <- t(phs)
latex(tphs, file="", table.env=FALSE, center="none",
      first.hline.double=FALSE,
      rowlabel=names(dimnames(tphs))[1], 
      cgroup=names(dimnames(tphs))[2],
      n.cgroup=dim(tphs)[2],
      cgroupTexCmd="normalfont"
      )


###################################################
### code chunk number 29: dollhill
###################################################
dh <- matrix(c(688,650,21,59), ncol = 2, byrow = TRUE)
dimnames(dh) <- list(Smoked = c("Yes","No"), Cancer = c("Yes","No"))
n <- margin.table(dh, 2)


###################################################
### code chunk number 30: dhtab
###################################################
latex(rbind(dh, Total=margin.table(dh, 2)),
      file="", first.hline.double=FALSE,
      rowlabel=names(dimnames(dh))[1],
      # rgroup=NULL, n.rgroup=c(2,1), rgroupTexCmd="",
      cgroup=names(dimnames(dh))[2],
      n.cgroup=dim(dh)[2],
      cgroupTexCmd="normalfont"
      )


###################################################
### code chunk number 31: icda-notes.Rnw:1678-1686
###################################################
data(jobsatisfaction)
sattab <- xtabs(Freq ~ Income + JobSat, data=jobsatisfaction)
names(dimnames(sattab))[2] <- "Job Satisfaction"
dimnames(sattab)[["Income"]] <- c("<5K","5K--15K","15K--25K",">25K")
dimnames(sattab)[["Job Satisfaction"]] <- c("Dissat","Little","Moderate","Very")
sattabwtot <- cbind(sattab, Total=margin.table(sattab,1))
sattabwtot <- rbind(sattabwtot, Total=margin.table(sattabwtot,2))
names(dimnames(sattabwtot)) <- names(dimnames(sattab))


###################################################
### code chunk number 32: texsattabwtot
###################################################
latex(sattabwtot,
      file="", booktabs=TRUE,
      rowlabel=names(dimnames(sattabwtot))[1], 
      ## rgroup=names(dimnames(sattabwtot))[1],
      n.rgroup=c(4,1),
      cgroup=c(names(dimnames(sattabwtot))[2],""),
      n.cgroup=c(4,1),
      cgroupTexCmd="normalfont"
      )


###################################################
### code chunk number 33: sattabcalc
###################################################
sattab.test <- chisq.test(sattab)
sattab.lrstat <- 
with(sattab.test,
2*sum(ifelse(observed == 0, 0, observed*log(observed/expected))))
n <- sum(sattab)
n.inc <- margin.table(sattab, 1)
n.sat <- margin.table(sattab, 2)
p.inc <- n.inc / n
p.sat <- n.sat / n
sat.r.adj <- residuals(sattab.test)/sqrt(((1-p.inc) %o% (1-p.sat)))
mu44 <- sattab.test[["expected"]][4,4]


###################################################
### code chunk number 34: icda-notes.Rnw:1930-1934
###################################################
latex(round(sat.r.adj, 2), 
      file="", first.hline.double=FALSE,
      rowlabel=names(dimnames(sat.r.adj))[1],
      cgroupTexCmd="normalfont")


###################################################
### code chunk number 35: icda-notes.Rnw:1949-1955
###################################################
pag.tab <- matrix(c(762, 484, 327, 239, 468, 477), nrow=2)
dimnames(pag.tab) <- 
  list(Gender=c("Female","Male"),
       Party=c("Democrat","Independent","Republican"))
pag.tab <- as.table(pag.tab)
pag.tab


###################################################
### code chunk number 36: icda-notes.Rnw:1965-1967
###################################################
pag.df <- as.data.frame(pag.tab)
pag.df


###################################################
### code chunk number 37: icda-notes.Rnw:1977-1982
###################################################
pag.df <- 
  expand.grid(Gender=c("Female","Male"),
             Party=c("Democrat","Independent","Republican"))
pag.df
pag.df$Freq <- c(762, 484, 327, 239, 468, 477)


###################################################
### code chunk number 38: icda-notes.Rnw:1990-1992
###################################################
pag.df
xtabs(Freq ~ Gender + Party, data=pag.df)


###################################################
### code chunk number 39: icda-notes.Rnw:2008-2009 (eval = FALSE)
###################################################
## pag.df <- read.table("Data/pag.txt", header=TRUE)


###################################################
### code chunk number 40: icda-notes.Rnw:2019-2020 (eval = FALSE)
###################################################
## pag.df <- read.csv("Data/pag.csv")


###################################################
### code chunk number 41: icda-notes.Rnw:2032-2035
###################################################
pag.tab
margin.table(pag.tab, 1)
margin.table(pag.tab, 2)


###################################################
### code chunk number 42: icda-notes.Rnw:2042-2043
###################################################
addmargins(pag.tab)


###################################################
### code chunk number 43: icda-notes.Rnw:2054-2056
###################################################
prop.table(pag.tab)
round(prop.table(pag.tab), 3)


###################################################
### code chunk number 44: icda-notes.Rnw:2066-2067
###################################################
prop.table(pag.tab, 1)


###################################################
### code chunk number 45: icda-notes.Rnw:2071-2072
###################################################
prop.table(pag.tab, 2)


###################################################
### code chunk number 46: icda-notes.Rnw:2081-2082
###################################################
chisq.test(pag.tab)


###################################################
### code chunk number 47: icda-notes.Rnw:2091-2096
###################################################
pag.chisq <- chisq.test(pag.tab)
names(pag.chisq)
pag.chisq$statistic
pag.chisq$parameter
pag.chisq$p.value


###################################################
### code chunk number 48: icda-notes.Rnw:2105-2108
###################################################
pag.chisq$observed
pag.chisq$expected
with(pag.chisq, sum((observed - expected)^2/expected))


###################################################
### code chunk number 49: icda-notes.Rnw:2119-2120
###################################################
pag.chisq$residuals


###################################################
### code chunk number 50: icda-notes.Rnw:2125-2126
###################################################
pag.chisq$stdres


###################################################
### code chunk number 51: texsattab
###################################################
latex(sattab,
      file="", booktabs=TRUE,
      rowlabel=names(dimnames(sattab))[1], 
      cgroup=names(dimnames(sattab))[2],
      n.cgroup=4,
      cgroupTexCmd="normalfont"
      )


###################################################
### code chunk number 52: icda-notes.Rnw:2198-2205
###################################################
latex(round(prop.table(sattab, 1),3),
      file="",  booktabs=TRUE, # first.hline.double=FALSE,
      rowlabel=names(dimnames(sattab))[1], 
      cgroup=names(dimnames(sattab))[2],
      n.cgroup=4,
      cgroupTexCmd="normalfont"
      )


###################################################
### code chunk number 53: sattabp
###################################################
satparttab <-
  rbind(sattab,
        apply(sattab[1:2,], 2, sum),
        apply(sattab[3:4,], 2, sum))
dimnames(satparttab) <- 
  list(Income=c("<5","5--15","15--25",">25","<15",">15"),
       c("VD","LS","MS","VS"))
myfun <- 
  function(tab){
    with(chisq.test(tab),
         c(statistic,
           2*sum(ifelse(observed == 0, 0, observed*log(observed/expected))),
           parameter))
  }
low.stats <- myfun(satparttab[1:2,])
high.stats <- myfun(satparttab[3:4,2:4])
high.stats["df"] <- 3
lovhi.stats <- myfun(satparttab[5:6,])
statcols <- rbind(low.stats,NA,high.stats,NA,lovhi.stats,NA)
dimnames(statcols) <- 
  list(NULL, c("$X^2$","$G^2$","df"))
satparttab <- cbind(satparttab, statcols)
satparttab <-
  rbind(satparttab, 
        c(rep(NA,4), apply(statcols, 2, sum, na.rm=TRUE)))
satparttab <- round(satparttab, 2)


###################################################
### code chunk number 54: icda-notes.Rnw:2245-2255
###################################################
latex(satparttab, # table.env=FALSE, center="none",
      file="", first.hline.double=FALSE,
      rowlabel="Income",
      n.rgroup=c(2,2,2,1),
      rgroup=c("Low","High","Low vs High","Sum"),
      rgroupTexCmd="normalfont",
      n.cgroup=c(4,3),
      cgroup=c("JobSat",""),
      cgroupTexCmd="normalfont",
      dcolumn=TRUE)


###################################################
### code chunk number 55: icda-notes.Rnw:2389-2390 (eval = FALSE)
###################################################
## cbind(0:4, dhyper(0:4, 4, 4, 4))


###################################################
### code chunk number 56: icda-notes.Rnw:2395-2398
###################################################
x <- cbind(0:4, round(dhyper(0:4, 4, 4, 4), 3))
colnames(x) <- c("$n_{11}$", "$P(n_{11})$")
latex(x, file="", first.hline.double=FALSE)


###################################################
### code chunk number 57: icda-notes.Rnw:2459-2466
###################################################
TeaTasting <-
  matrix(c(3, 1, 1, 3),
         nrow = 2,
         dimnames = list(Truth = c("Milk", "Tea"),
           Guess = c("Milk", "Tea")))
TeaTasting <- as.table(TeaTasting)
fisher.test(TeaTasting, alternative = "greater")


###################################################
### code chunk number 58: icda-notes.Rnw:2486-2487
###################################################
fisher.test(TeaTasting, alternative = "two.sided")


###################################################
### code chunk number 59: icda-notes.Rnw:2511-2513
###################################################
sattab
fisher.test(sattab)


###################################################
### code chunk number 60: deathpenalty.1
###################################################
data(deathpenalty)
deathpenalty
deathpenalty <-
  transform(deathpenalty,
            DeathPenalty = relevel(DeathPenalty, "Yes"),
            Defendant = relevel(Defendant, "White"),
            Victim = relevel(Victim, "White"))


###################################################
### code chunk number 61: deathpenalty.2
###################################################
dp <- xtabs(Freq ~ Victim + Defendant + DeathPenalty, 
            data=deathpenalty)
dp


###################################################
### code chunk number 62: deathpenalty.3
###################################################
dpflat <- ftable(DeathPenalty ~ Victim + Defendant, 
                 data=dp)
dpflat


###################################################
### code chunk number 63: icda-notes.Rnw:2569-2570
###################################################
round(100*prop.table(dpflat,1), 1)


###################################################
### code chunk number 64: icda-notes.Rnw:2579-2586
###################################################
latex(dp[1,,], 
      file="", table.env=FALSE, center="none",
      first.hline.double=FALSE,
      rowlabel="Def",
      n.cgroup=2,
      cgroup="DeathPen",
      cgroupTexCmd="normalfont")


###################################################
### code chunk number 65: icda-notes.Rnw:2589-2596
###################################################
latex(dp[2,,], 
      file="", table.env=FALSE, center="none",
      first.hline.double=FALSE,
      rowlabel="Def",
      n.cgroup=2,
      cgroup="DeathPen",
      cgroupTexCmd="normalfont")


###################################################
### code chunk number 66: icda-notes.Rnw:2634-2643
###################################################
dpmargin <- xtabs(Freq ~ Defendant + DeathPenalty,
                  data=deathpenalty)
latex(dpmargin,
      file="", # table.env=FALSE, center="none",
      first.hline.double=FALSE,
      rowlabel="Def",
      n.cgroup=2,
      cgroup="DeathPen",
      cgroupTexCmd="normalfont")


###################################################
### code chunk number 67: malformation.1
###################################################
data(malformation)
malformation


###################################################
### code chunk number 68: malformation.2
###################################################
malform.tab <- xtabs(Freq ~ Alcohol + Malformation, 
                     data=malformation)
malform.tab
round(100*prop.table(malform.tab, 1), 2)


###################################################
### code chunk number 69: malformation.3
###################################################
library(reshape2)
malformwide <- dcast(malformation,
                   Alcohol ~ Malformation, 
                   value.var="Freq")
malformwide


###################################################
### code chunk number 70: malformation.4
###################################################
malform.lin <- 
  glm(cbind(Present,Absent) ~ Alcohol, 
      family=binomial(link=make.link("identity")),
      data=malformwide)
malformwide <- 
  transform(malformwide, Total = Present + Absent)
malform.lin.alt <- 
  glm(Present/Total ~ Alcohol, weights=Total,
      family=binomial(link=make.link("identity")),
      data=malformwide)
coef(malform.lin)


###################################################
### code chunk number 71: malformation.5 (eval = FALSE)
###################################################
## summary(malform.lin)


###################################################
### code chunk number 72: icda-notes.Rnw:2914-2915
###################################################
summary(malform.lin)


###################################################
### code chunk number 73: icda-notes.Rnw:2923-2924
###################################################
malcoef.lin <- summary(malform.lin)$coefficients


###################################################
### code chunk number 74: malform.6
###################################################
malform.logit <- glm(cbind(Present,Absent) ~ Alcohol, 
                     family=binomial, data=malformwide)


###################################################
### code chunk number 75: malform.7 (eval = FALSE)
###################################################
## summary(malform.logit)


###################################################
### code chunk number 76: icda-notes.Rnw:3016-3017
###################################################
summary(malform.logit)


###################################################
### code chunk number 77: malform.9
###################################################
malcoef.logit <- summary(malform.logit)$coefficients
maldel <- malformwide
maldel[5,c("Present","Total")] <- c(0,37)
maldel.logit <- 
  glm(Present/Total ~ Alcohol, weights=Total,
      family=binomial(link="logit"),
      data=maldel)
maldel.pval <- summary(maldel.logit)$coefficients[2,4]


###################################################
### code chunk number 78: icda-notes.Rnw:3107-3111 (eval = FALSE)
###################################################
## plot(0:10, dpois(0:10,2.25), type="h", 
##      xlab="y", ylab="p(y)", main="Poisson(mu=2.25)")
## plot(0:18, dpois(0:18,7.3), type="h", 
##      xlab="y", ylab="p(y)", main="Poisson(mu=7.3)")


###################################################
### code chunk number 79: icda-notes.Rnw:3115-3121
###################################################
opar <- par(mfrow=c(1,2))
plot(0:10, dpois(0:10,2.25), type="h", xlab="y", ylab="p(y)",
     main=expression(paste("Poisson(", mu, "=2.25)")))
plot(0:18, dpois(0:18,7.3), type="h", xlab="y", ylab="p(y)",
     main=expression(paste("Poisson(", mu, "=7.3)")))
par(opar)


###################################################
### code chunk number 80: icda-notes.Rnw:3150-3160
###################################################
A <- c(8,7,6,6,3,4,7,2,3,4)
B <- c(9,9,8,14,8,13,11,5,7,6)
trt <- factor(rep(c("A","B"), each=10))
wafers <- data.frame(trt=trt, defects=c(A,B))
wafers.lin <- glm(defects ~ trt, 
                 family=poisson(link="identity"),
                 data=wafers)
wafers.loglin <- glm(defects ~ trt, 
                     family=poisson(link="log"),
                     data=wafers)


###################################################
### code chunk number 81: wafers.2 (eval = FALSE)
###################################################
## summary(wafers.lin)


###################################################
### code chunk number 82: wafers.3 (eval = FALSE)
###################################################
## summary(wafers.loglin)


###################################################
### code chunk number 83: icda-notes.Rnw:3180-3181
###################################################
summary(wafers.lin)


###################################################
### code chunk number 84: icda-notes.Rnw:3188-3189
###################################################
summary(wafers.loglin)


###################################################
### code chunk number 85: wafers.4
###################################################
wafercoef.lin <- summary(wafers.lin)$coefficients
wafercoef.loglin <- summary(wafers.loglin)$coefficients


###################################################
### code chunk number 86: icda-notes.Rnw:3288-3299
###################################################
wafcoef <- summary(wafers.loglin)$coefficients
wafL1 <- 
  with(wafers.loglin,
       sum(y*log(fitted.values) - fitted.values - lfactorial(y)))
wafL0 <-
  with(update(wafers.loglin, . ~ 1),
       sum(y*log(fitted.values) - fitted.values - lfactorial(y)))
wafdev <- wafers.loglin$deviance
wafdf <- wafers.loglin$df.residual
wafdev0 <- wafers.loglin$null.deviance
wafdf0 <- wafers.loglin$df.null


###################################################
### code chunk number 87: icda-notes.Rnw:3366-3367
###################################################
drop1(wafers.loglin, test="Chisq") 


###################################################
### code chunk number 88: icda-notes.Rnw:3379-3380
###################################################
anova(wafers.loglin, test="Chisq")


###################################################
### code chunk number 89: wafers.4
###################################################
wafCI.LR <- confint(wafers.loglin)
wafCI.Wald <- confint.default(wafers.loglin)


###################################################
### code chunk number 90: icda-notes.Rnw:3451-3455
###################################################
wafCI.LR <- confint(wafers.loglin)
wafCI.Wald <- confint.default(wafers.loglin)
wafCI.LR
exp(wafCI.LR)
wafCI.Wald


###################################################
### code chunk number 91: icda-notes.Rnw:3550-3552
###################################################
plot(0:53, dpois(0:53,25), type="h", xlab="y", ylab="p(y)",
     main=expression(paste("Poisson(", mu, "=25)")))


###################################################
### code chunk number 92: icda-notes.Rnw:3588-3593
###################################################
homicide <- 
  data.frame(nvics=rep(0:6, 2),
             race=rep(c("Black","White"), each=7),
             Freq=c(119,16,12,7,3,2,0,1070,60,14,4,0,0,1))
xtabs(Freq ~ race + nvics, data=homicide)


###################################################
### code chunk number 93: homicide.xtra
###################################################
n <- with(homicide, tapply(Freq, race, sum))
ybar <- by(homicide, homicide$race,
           function(x) weighted.mean(x$nvics, x$Freq))
homicide$ybar <- rep(ybar, each=7)
s2 <-
  by(homicide, homicide$race, 
     function(x) weighted.mean((x$nvics - x$ybar)^2, x$Freq))
cbind(n, ybar,s2)


###################################################
### code chunk number 94: icda-notes.Rnw:3630-3631
###################################################
n <- with(homicide, tapply(Freq, race, sum))
ybar <- by(homicide, homicide$race,
           function(x) weighted.mean(x$nvics, x$Freq))
homicide$ybar <- rep(ybar, each=7)
s2 <-
  by(homicide, homicide$race, 
     function(x) weighted.mean((x$nvics - x$ybar)^2, x$Freq))
cbind(n, ybar,s2)


###################################################
### code chunk number 95: icda-notes.Rnw:3641-3650
###################################################
## homicide <- 
##   transform(homicide, race = relevel(race, "White"))
options(contrasts=c("contr.SAS","contr.poly"))
hom.poi <-
  glm(nvics ~ race, data=homicide, weights=Freq,
      family=poisson)
library(MASS)
hom.nb <-
  glm.nb(nvics ~ race, data=homicide, weights=Freq)


###################################################
### code chunk number 96: hom.poi (eval = FALSE)
###################################################
## summary(hom.poi)


###################################################
### code chunk number 97: hom.nb (eval = FALSE)
###################################################
## summary(hom.nb)


###################################################
### code chunk number 98: icda-notes.Rnw:3670-3671
###################################################
summary(hom.poi)


###################################################
### code chunk number 99: icda-notes.Rnw:3678-3680
###################################################
tmp <- paragraphs(summary(hom.nb))
tmp[1:6]


###################################################
### code chunk number 100: icda-notes.Rnw:3687-3688
###################################################
tmp[-(1:6)]


###################################################
### code chunk number 101: icda-notes.Rnw:3695-3699
###################################################
homcoef.poi <- summary(hom.poi)$coefficients
homcoef.nb <- summary(hom.nb)$coefficients
homWCI.poi <- confint.default(hom.poi)
homWCI.nb <- confint.default(hom.nb)


###################################################
### code chunk number 102: icda-notes.Rnw:3754-3758
###################################################
confint.default(hom.poi)
exp(confint.default(hom.poi))
## confint.default(hom.nb)
exp(confint.default(hom.nb))


###################################################
### code chunk number 103: icda-notes.Rnw:3765-3769
###################################################
confint(hom.poi)
exp(confint(hom.poi))
## confint(hom.nb)
exp(confint(hom.nb))


###################################################
### code chunk number 104: icda-notes.Rnw:3776-3778
###################################################
homLRCI.poi <- confint(hom.poi)
homLRCI.nb <- confint(hom.nb)


###################################################
### code chunk number 105: icda-notes.Rnw:3839-3845
###################################################
homicide2 <- homicide[rep(1:14, homicide$Freq),]
homicide2$Freq <- NULL
homicide2$ybar <- NULL
head(homicide2)
hom.poi2 <- 
  glm(nvics ~ race, data=homicide2, family=poisson)


###################################################
### code chunk number 106: hom.poi.ungrp (eval = FALSE)
###################################################
## summary(hom.poi2)


###################################################
### code chunk number 107: icda-notes.Rnw:3858-3859
###################################################
summary(hom.poi2)


###################################################
### code chunk number 108: icda-notes.Rnw:3914-3918
###################################################
data(traincollisions)
trains.loglin <-
  glm(TrRd ~ I(Year-1975), offset = log(KM), 
      family=poisson, data=traincollisions)


###################################################
### code chunk number 109: trains.summary
###################################################
summary(trains.loglin)


###################################################
### code chunk number 110: icda-notes.Rnw:3932-3933
###################################################
summary(trains.loglin)


###################################################
### code chunk number 111: icda-notes.Rnw:3938-3940
###################################################
trcoef <- coef(trains.loglin)
etrcoef <- exp(trcoef)


###################################################
### code chunk number 112: icda-notes.Rnw:3982-3985 (eval = FALSE)
###################################################
## trains.nb <-
##   glm.nb(TrRd ~ I(Year-1975) + offset(log(KM)),
##          data=traincollisions)


###################################################
### code chunk number 113: trainplot (eval = FALSE)
###################################################
## attach(traincollisions)
## plot(Year, 1000*TrRd/KM, ylim=c(0,1000*max(TrRd/KM)),
##      ylab="Collisions per Billion Train-Kilometers")
## curve(1000*exp(-4.21 - 0.0329*(x-1975)), add=TRUE)
## detach(traincollisions)


###################################################
### code chunk number 114: icda-notes.Rnw:4006-4007
###################################################
attach(traincollisions)
plot(Year, 1000*TrRd/KM, ylim=c(0,1000*max(TrRd/KM)),
     ylab="Collisions per Billion Train-Kilometers")
curve(1000*exp(-4.21 - 0.0329*(x-1975)), add=TRUE)
detach(traincollisions)


###################################################
### code chunk number 115: icda-notes.Rnw:4095-4098
###################################################
data(horseshoecrabs)
head(horseshoecrabs, 5)
nrow(horseshoecrabs)


###################################################
### code chunk number 116: icda-notes.Rnw:4105-4108
###################################################
summary(horseshoecrabs)
crabs.fit1 <- glm((Satellites > 0) ~ Weight,
                   family=binomial, data=horseshoecrabs)


###################################################
### code chunk number 117: crabs.summary
###################################################
summary(crabs.fit1)


###################################################
### code chunk number 118: icda-notes.Rnw:4122-4123
###################################################
summary(crabs.fit1)


###################################################
### code chunk number 119: icda-notes.Rnw:4130-4159
###################################################
xbar <- mean(horseshoecrabs$Weight)
eta.xbar <- predict(crabs.fit1, data.frame(Weight=xbar), type="link")
pi.xbar <- predict(crabs.fit1, data.frame(Weight=xbar), type="response")
crabs.coef <- summary(crabs.fit1)$coefficients
crabs.WaldCI <- confint.default(crabs.fit1)
crabs.LRCI <- confint(crabs.fit1)
ld50 <- -crabs.coef[1,1]/crabs.coef[2,1]
ld50.p1 <- ld50 + 1
pi.ld50.p1 <- predict(crabs.fit1,
                   data.frame(Weight=ld50.p1),
                   type="response")
ld50.p.1 <- ld50 + 0.1
pi.ld50.p.1 <- predict(crabs.fit1,
                   data.frame(Weight=ld50.p.1),
                   type="response")
xmax <- max(horseshoecrabs$Weight)
pi.xmax <- predict(crabs.fit1,
                   data.frame(Weight=xmax),
                   type="response")
crabs.pred.link <- predict(crabs.fit1, type="link", se.fit=TRUE)
crabs.CI.link <- with(crabs.pred.link, fit - qnorm(0.975)*se.fit)
crabs.CI.link <- cbind(crabs.CI.link,
                       with(crabs.pred.link, fit + qnorm(0.975)*se.fit))
crabs.CI.resp <- plogis(crabs.CI.link)

crabs.pred.resp <- predict(crabs.fit1, type="response", se.fit=TRUE)
crabs.CI.resp2 <- with(crabs.pred.resp, fit - qnorm(0.975)*se.fit)
crabs.CI.resp2 <- cbind(crabs.CI.resp2,
                        with(crabs.pred.resp, fit + qnorm(0.975)*se.fit))


###################################################
### code chunk number 120: icda-notes.Rnw:4211-4220
###################################################
xbar <- mean(horseshoecrabs$Weight)
predict(crabs.fit1, data.frame(Weight=xbar), type="link")
predict(crabs.fit1, data.frame(Weight=xbar),
        type="response")
ab <- coef(crabs.fit1); ld50 <- -ab[1]/ab[2]
names(ld50) <- NULL; ld50
predict(crabs.fit1, 
        data.frame(Weight = ld50 + c(0, 0.1, 1)),
        type="response")


###################################################
### code chunk number 121: crabplot (eval = FALSE)
###################################################
## logit <- make.link("logit")
## ab <- coef(crabs.fit1)
## attach(horseshoecrabs)
## plot(Weight, (Satellites > 0), xlim=c(0,6), ylim=c(0,1),
##      xlab="Weight", ylab="Has Satellites")
## curve(logit$linkinv(ab[1] + ab[2]*x), add=TRUE)
## detach(horseshoecrabs)


###################################################
### code chunk number 122: icda-notes.Rnw:4285-4286
###################################################
logit <- make.link("logit")
ab <- coef(crabs.fit1)
attach(horseshoecrabs)
plot(Weight, (Satellites > 0), xlim=c(0,6), ylim=c(0,1),
     xlab="Weight", ylab="Has Satellites")
curve(logit$linkinv(ab[1] + ab[2]*x), add=TRUE)
detach(horseshoecrabs)


###################################################
### code chunk number 123: icda-notes.Rnw:4360-4362
###################################################
sd(horseshoecrabs$Weight)
IQR(horseshoecrabs$Weight)/2


###################################################
### code chunk number 124: icda-notes.Rnw:4561-4562
###################################################
crabs.CI.xbar <- predCI(crabs.fit1, newdata=data.frame(Weight=xbar))


###################################################
### code chunk number 125: icda-notes.Rnw:4586-4592
###################################################
confint(crabs.fit1)
exp(confint(crabs.fit1)[2,])
crabs.predCI <- predCI(crabs.fit1)
crabs.predCI[1,]
xbar <- mean(horseshoecrabs$Weight)
predCI(crabs.fit1, newdata=data.frame(Weight=xbar))


###################################################
### code chunk number 126: icda-notes.Rnw:4639-4641
###################################################
drop1(crabs.fit1, test="Chisq")
# anova(crabs.fit1, test="Chisq")


###################################################
### code chunk number 127: icda-notes.Rnw:4765-4766 (eval = FALSE)
###################################################
## options(contrasts=c("contr.SAS","contr.poly"))


###################################################
### code chunk number 128: icda-notes.Rnw:4771-4772
###################################################
options(contrasts=c("contr.treatment","contr.poly"))


###################################################
### code chunk number 129: icda-notes.Rnw:4781-4787
###################################################
horseshoecrabs <- 
  transform(horseshoecrabs, C = as.factor(Color))
levels(horseshoecrabs$C)
crabs.fit2 <-
  glm((Satellites > 0) ~ C + Weight, family=binomial, 
      data=horseshoecrabs)


###################################################
### code chunk number 130: icda-notes.Rnw:4790-4791 (eval = FALSE)
###################################################
## summary(crabs.fit2)


###################################################
### code chunk number 131: icda-notes.Rnw:4794-4796
###################################################
tmp <- paragraphs(summary(crabs.fit2))
tmp[1:2]


###################################################
### code chunk number 132: icda-notes.Rnw:4801-4802
###################################################
tmp[3:(length(tmp)-1)]


###################################################
### code chunk number 133: icda-notes.Rnw:4807-4812
###################################################
crabs.f1.coef <- summary(crabs.fit2)$coefficients
crabs.f1.pred.light.xbar <-
  predict(crabs.fit2,newdata=data.frame(C="1",Weight=xbar),type="response")
crabs.f1.pred.med.xbar <-
  predict(crabs.fit2,newdata=data.frame(C="2",Weight=xbar),type="response")


###################################################
### code chunk number 134: icda-notes.Rnw:4830-4834
###################################################
lp.ltx(crabs.f1.coef[,"Estimate"], "lpcrab", digits=2, format="f")
lp.ltx(c(sum(crabs.f1.coef[c("(Intercept)","C2"),"Estimate"]),
         crabs.f1.coef["Weight","Estimate"]),
       "lpcrabmed", digits=2, format="f")


###################################################
### code chunk number 135: icda-notes.Rnw:4968-4986
###################################################
## mycolors <- rainbow(4)
cf <- coef(crabs.fit2)
expit <- make.link("logit")$linkinv
with(horseshoecrabs, 
     plot(jitter(Weight,5), (Satellites > 0)+(2.5-Color)/40,
          col=mycolors[Color], xlim=c(0.5,5.5),
          xlab="Weight", ylab="Satellites (Yes=1, No=0)"))
curve(expit(cf["(Intercept)"] + cf["Weight"]*x),
      col=mycolors[1], add=TRUE)
curve(expit(cf["(Intercept)"] + cf["C2"] + cf["Weight"]*x), 
      col=mycolors[2], add=TRUE)
curve(expit(cf["(Intercept)"] + cf["C3"] + cf["Weight"]*x), 
      col=mycolors[3], add=TRUE)
curve(expit(cf["(Intercept)"] + cf["C4"] + cf["Weight"]*x), 
      col=mycolors[4], add=TRUE)
leg.txt <- c("med-light","medium","med-dark","dark")
legend("bottomright", leg.txt, pch=19,
       col=mycolors[1:4], text.col=mycolors[1:4])


###################################################
### code chunk number 136: icda-notes.Rnw:5025-5027
###################################################
anova(crabs.fit1, crabs.fit2, test="Chisq")
drop1(crabs.fit2, test="Chisq")


###################################################
### code chunk number 137: icda-notes.Rnw:5065-5070
###################################################
crabs.fit3 <-
  glm((Satellites > 0) ~ I(Color == 4) + Weight,
      family=binomial, data=horseshoecrabs)
summary(crabs.fit3)
anova(crabs.fit3, crabs.fit2, test="Chisq")


###################################################
### code chunk number 138: icda-notes.Rnw:5135-5139
###################################################
crabs.fit4 <-
  update(crabs.fit2, . ~ C*Weight)
deviance(crabs.fit4)
anova(crabs.fit2, crabs.fit4, test="Chisq")


###################################################
### code chunk number 139: icda-notes.Rnw:5146-5147
###################################################
drop1(crabs.fit4, test="Chisq")


###################################################
### code chunk number 140: icda-notes.Rnw:5173-5176
###################################################
crabs.fit5 <-
  glm((Satellites > 0) ~ Weight + Color,
      family=binomial, data=horseshoecrabs)


###################################################
### code chunk number 141: icda-notes.Rnw:5179-5180 (eval = FALSE)
###################################################
## summary(crabs.fit5)


###################################################
### code chunk number 142: icda-notes.Rnw:5188-5189
###################################################
summary(crabs.fit5)


###################################################
### code chunk number 143: icda-notes.Rnw:5197-5202
###################################################
crabs.f5.coef <- summary(crabs.fit5)$coefficients
coef.ltx(crabs.f5.coef[,"Estimate"], "cfcrabord", digits=2, format="f")
coef.ltx(crabs.f5.coef[,"Std. Error"], "secrabord", digits=2, format="f")
coef.ltx(exp(crabs.f5.coef[,"Estimate"]), "expcfcrabord", digits=2, format="f")
lp.ltx(crabs.f5.coef[,"Estimate"], "lpcrabord", digits=2, format="f")


###################################################
### code chunk number 144: icda-notes.Rnw:5258-5259
###################################################
anova(crabs.fit5, crabs.fit2, test="Chisq")


###################################################
### code chunk number 145: icda-notes.Rnw:5269-5270
###################################################
dpflat


###################################################
### code chunk number 146: icda-notes.Rnw:5283-5287
###################################################
deathpenalty
library(reshape2)
dp <- melt(deathpenalty)
dpwide <- dcast(dp, ... ~ DeathPenalty)


###################################################
### code chunk number 147: icda-notes.Rnw:5294-5298
###################################################
dpwide
dp.fit1 <-
  glm(cbind(Yes,No) ~ Defendant + Victim, family=binomial,
      data=dpwide)


###################################################
### code chunk number 148: icda-notes.Rnw:5302-5303 (eval = FALSE)
###################################################
## summary(dp.fit1)


###################################################
### code chunk number 149: icda-notes.Rnw:5310-5312
###################################################
summary(dp.fit1)
## drop1(dp.fit1, test="Chisq")


###################################################
### code chunk number 150: icda-notes.Rnw:5392-5396
###################################################
drop1(dp.fit1, test="Chisq")
dp.fit2 <- update(dp.fit1, . ~ Victim)
deviance(dp.fit2)
df.residual(dp.fit2)


###################################################
### code chunk number 151: icda-notes.Rnw:5895-5903
###################################################
horseshoecrabs <- 
  transform(horseshoecrabs, 
            C = as.factor(Color),
            S = as.factor(Spine))
options(contrasts=c("contr.SAS","contr.poly"))
crabs.fitall <-
  glm((Satellites > 0) ~ C + S + Weight + Width,
      family=binomial, data=horseshoecrabs)


###################################################
### code chunk number 152: icda-notes.Rnw:5908-5909 (eval = FALSE)
###################################################
## summary(crabs.fitall)


###################################################
### code chunk number 153: icda-notes.Rnw:5916-5918
###################################################
tmp <- paragraphs(summary(crabs.fitall))
tmp[1:3]


###################################################
### code chunk number 154: icda-notes.Rnw:5923-5924
###################################################
tmp[4:(length(tmp)-1)]


###################################################
### code chunk number 155: icda-notes.Rnw:5962-5963
###################################################
drop1(crabs.fitall, test="Chisq")


###################################################
### code chunk number 156: icda-notes.Rnw:5984-5986
###################################################
attach(horseshoecrabs)
cor(Weight, Width)


###################################################
### code chunk number 157: crab.plot.width.weight (eval = FALSE)
###################################################
## plot(Width, Weight)


###################################################
### code chunk number 158: icda-notes.Rnw:5993-5994
###################################################
detach(horseshoecrabs)


###################################################
### code chunk number 159: icda-notes.Rnw:6002-6005
###################################################
attach(horseshoecrabs)
plot(Width, Weight)
detach(horseshoecrabs)


###################################################
### code chunk number 160: icda-notes.Rnw:6029-6034
###################################################
crabs.fit1 <-
  glm((Satellites > 0) ~ C*S*Width,
      family=binomial, data=horseshoecrabs)
crabs.fit2 <- update(crabs.fit1, . ~ C + S + Width)
anova(crabs.fit2, crabs.fit1, test="Chisq")


###################################################
### code chunk number 161: icda-notes.Rnw:6081-6082
###################################################
with(horseshoecrabs, table(C,S))


###################################################
### code chunk number 162: icda-notes.Rnw:6097-6105 (eval = FALSE)
###################################################
## drop1(crabs.fit1, test="Chisq")
## crabs.fit1a <-
##   update(crabs.fit1, . ~ . - C:S:Width)
## drop1(crabs.fit1a, test="Chisq")
## crabs.fit1b <- update(crabs.fit1a, . ~ . - S:Width)
## drop1(crabs.fit1b, test="Chisq")
## crabs.fit1c <- update(crabs.fit1b, . ~ . - C:Width)
## drop1(crabs.fit1c, test="Chisq")


###################################################
### code chunk number 163: icda-notes.Rnw:6125-6126
###################################################
drop1(crabs.fit2, test="Chisq")


###################################################
### code chunk number 164: icda-notes.Rnw:6133-6138
###################################################
## crabs.fit3 <- update(crabs.fit2, . ~ . - S)
crabs.fit3 <- update(crabs.fit2, . ~ C + Width)
deviance(crabs.fit3)
deviance(crabs.fit2)
anova(crabs.fit3, crabs.fit2, test="Chisq")


###################################################
### code chunk number 165: icda-notes.Rnw:6143-6144
###################################################
drop1(crabs.fit3, test="Chisq")


###################################################
### code chunk number 166: icda-notes.Rnw:6149-6150 (eval = FALSE)
###################################################
## summary(crabs.fit3)


###################################################
### code chunk number 167: icda-notes.Rnw:6157-6159
###################################################
tmp <- paragraphs(summary(crabs.fit3))
tmp[1:4]


###################################################
### code chunk number 168: icda-notes.Rnw:6164-6165
###################################################
tmp[5:(length(tmp)-1)]


###################################################
### code chunk number 169: icda-notes.Rnw:6170-6173
###################################################
crabs.f3.coef <- summary(crabs.fit3)$coefficients
lp.ltx(crabs.f3.coef[,"Estimate"], "lpcrabcolwd", 
       digits=c(rep(1,4), 2), format="f")


###################################################
### code chunk number 170: crabsdkwd
###################################################
crabs.fit4 <- update(crabs.fit3, . ~ I(C == "4") + Width)


###################################################
### code chunk number 171: icda-notes.Rnw:6180-6182
###################################################
crabs.f4.coef <- summary(crabs.fit4)$coefficients
lp.ltx(crabs.f4.coef[,"Estimate"], "lpcrabdkwd", digits=c(1, 1, 2), format="f")


###################################################
### code chunk number 172: icda-notes.Rnw:6213-6215
###################################################
crabs.fit4 <- update(crabs.fit3, . ~ I(C == "4") + Width)
anova(crabs.fit4, crabs.fit3, test="Chisq")


###################################################
### code chunk number 173: icda-notes.Rnw:6220-6221 (eval = FALSE)
###################################################
## summary(crabs.fit4)


###################################################
### code chunk number 174: icda-notes.Rnw:6226-6228
###################################################
tmp <- paragraphs(summary(crabs.fit4))
tmp[1:4]


###################################################
### code chunk number 175: icda-notes.Rnw:6233-6238
###################################################
crabs.f4.coef <- summary(crabs.fit4)$coefficients
coef.ltx(crabs.f4.coef[,"Estimate"], "cfcrabdkwd", digits=3, format="f")
coef.ltx(crabs.f4.coef[,"Std. Error"], "secrabdkwd", digits=3, format="g")
coef.ltx(exp(crabs.f4.coef[,"Estimate"]), "expcfcrabdkwd", digits=1,
         format="f")


###################################################
### code chunk number 176: icda-notes.Rnw:6241-6245
###################################################
cidk <- confint.default(crabs.fit4)[2,]
ecidk <- exp(cidk)
ciwd <- confint.default(crabs.fit4)[3,]
eciwd <- exp(ciwd)


###################################################
### code chunk number 177: icda-notes.Rnw:6353-6360
###################################################
crabs.color <- glm((Satellites > 0) ~ C, family=binomial,
                    data=horseshoecrabs)
crabs.width <- update(crabs.color, . ~ Width)
crabs.color.width <- update(crabs.color, . ~ C + Width)
crabs.dark.width <-
   update(crabs.color, . ~ I(C == "4") + Width)
y <- as.numeric(horseshoecrabs$Satellites > 0)


###################################################
### code chunk number 178: icda-notes.Rnw:6365-6373
###################################################
pihat <- predict(crabs.color, type="response")
cor(y,pihat)
pihat <- predict(crabs.width, type="response")
cor(y,pihat)
pihat <- predict(crabs.color.width, type="response")
cor(y,pihat)
pihat <- predict(crabs.dark.width, type="response")
cor(y,pihat)


###################################################
### code chunk number 179: icda-notes.Rnw:6413-6418
###################################################
pihat <- predict(crabs.color.width, type="response")
yhat <- as.numeric(pihat > 0.50)
y <- as.numeric(horseshoecrabs$Satellites > 0)
table(y, yhat)
addmargins(table(y, yhat), 2)


###################################################
### code chunk number 180: icda-notes.Rnw:6463-6469 (eval = FALSE)
###################################################
## pihat <- vector(length=173)
## for (i in 1:173) {
##   pihat[i] <-
##     predict(update(crabs.color.width, subset=-i),
##             newdata=horseshoecrabs[i,], type="response")
## }


###################################################
### code chunk number 181: icda-notes.Rnw:6472-6473
###################################################
source("crabs-cv-pihat.Rdmp")


###################################################
### code chunk number 182: icda-notes.Rnw:6476-6480
###################################################
yhat <- as.numeric(pihat > 0.50)
y <- as.numeric(horseshoecrabs$Satellites > 0)
confusion <- table(y, yhat)
confusion


###################################################
### code chunk number 183: icda-notes.Rnw:6487-6491
###################################################
prop.table(confusion, 1)
sum(diag(confusion))/sum(confusion)
yhat <- as.numeric(pihat > 0.64)
table(y,yhat)


###################################################
### code chunk number 184: icda-notes.Rnw:6555-6560
###################################################
library(epicalc)
lroc(crabs.width, graph=FALSE)$auc
lroc(crabs.color, graph=FALSE)$auc
lroc(crabs.color.width, graph=FALSE)$auc
lroc(crabs.dark.width, graph=FALSE)$auc


###################################################
### code chunk number 185: icda-notes.Rnw:6563-6564 (eval = FALSE)
###################################################
## lroc(crabs.color.width, grid=FALSE, title=TRUE)


###################################################
### code chunk number 186: icda-notes.Rnw:6575-6576
###################################################
tmp <- lroc(crabs.color.width, grid=FALSE, title=TRUE)


###################################################
### code chunk number 187: icda-notes.Rnw:6690-6696
###################################################
formula(dp.fit1)
deviance(dp.fit1)
df.residual(dp.fit1)
pchisq(deviance(dp.fit1), 1, lower.tail=FALSE)
chisqstat(dp.fit1)
pchisq(chisqstat(dp.fit1), 1, lower.tail=FALSE)


###################################################
### code chunk number 188: icda-notes.Rnw:6741-6744
###################################################
dp.saturated <- update(dp.fit1, . ~ Defendant*Victim)
anova(dp.fit1, dp.saturated, test="LRT")
anova(dp.fit1, dp.saturated, test="Rao")


###################################################
### code chunk number 189: icda-notes.Rnw:6842-6845
###################################################
data(UCBAdmissions)
is.table(UCBAdmissions)
dimnames(UCBAdmissions)


###################################################
### code chunk number 190: UCBtab
###################################################
ftable(UCBAdmissions, 
       row.vars="Dept", col.vars=c("Gender","Admit"))


###################################################
### code chunk number 191: icda-notes.Rnw:6858-6861
###################################################
margin.table(UCBAdmissions, 2:1)
round(prop.table(margin.table(UCBAdmissions, 2:1), 1), 3)
oddsratio(margin.table(UCBAdmissions, 2:1))


###################################################
### code chunk number 192: icda-notes.Rnw:6866-6868
###################################################
UCBdf <- as.data.frame(UCBAdmissions)
head(UCBdf)


###################################################
### code chunk number 193: icda-notes.Rnw:6873-6877
###################################################
library(reshape2)
UCBw <- 
  dcast(UCBdf, Gender + Dept ~ Admit, value.var="Freq")
UCBw


###################################################
### code chunk number 194: icda-notes.Rnw:6882-6885
###################################################
options(contrasts=c("contr.treatment","contr.poly"))
UCB.fit1 <- glm(cbind(Admitted,Rejected) ~ Dept + Gender, 
                family=binomial, data=UCBw)


###################################################
### code chunk number 195: UCB.fit1.summary (eval = FALSE)
###################################################
## summary(UCB.fit1)


###################################################
### code chunk number 196: icda-notes.Rnw:6894-6895
###################################################
summary(UCB.fit1)


###################################################
### code chunk number 197: icda-notes.Rnw:6900-6906
###################################################
chisqstat(UCB.fit1)
df.residual(UCB.fit1)
pchisq(chisqstat(UCB.fit1), df.residual(UCB.fit1),
       lower.tail=FALSE)
UCB.fit1.stdres <- rstandard(UCB.fit1, type="pearson")
round(UCB.fit1.stdres, 2)


###################################################
### code chunk number 198: icda-notes.Rnw:6911-6912
###################################################
cbind(UCBw, "stdres" = round(UCB.fit1.stdres, 2))


###################################################
### code chunk number 199: icda-notes.Rnw:6943-6946
###################################################
UCB.fit2 <- glm(cbind(Admitted,Rejected) ~ Dept,
                family=binomial, data=UCBw,
                subset=(Dept != "A"))


###################################################
### code chunk number 200: UCB.fit2.summary (eval = FALSE)
###################################################
## summary(UCB.fit2)


###################################################
### code chunk number 201: icda-notes.Rnw:6955-6956
###################################################
summary(UCB.fit2)


###################################################
### code chunk number 202: icda-notes.Rnw:6961-6964
###################################################
chisqstat(UCB.fit2)
UCB.fit3 <- update(UCB.fit2, . ~ Dept + Gender)
anova(UCB.fit2, UCB.fit3, test="Chisq")


###################################################
### code chunk number 203: icda-notes.Rnw:6969-6972
###################################################
UCBAdmissions[,,"A"]
oddsratio(UCBAdmissions[,,"A"])
1/oddsratio(UCBAdmissions[,,"A"])


###################################################
### code chunk number 204: icda-notes.Rnw:7029-7034
###################################################
## Setup for next few plots
x <- c(1:4,6:9)*10
y <- rep(c(0,1), each=4)
abratio <- -50
betamax <- 2


###################################################
### code chunk number 205: icda-notes.Rnw:7037-7047
###################################################
beta <- c(0, 0.1, 0.25, 0.5, 1, betamax)
plot(x, y, xlab = "x", ylab = "y")
for (i in 1:length(beta)) {
   curve(plogis((abratio + x)*beta[i]), col=mycolors[i], add=TRUE)
}
leg.txt <- as.character(beta)
legend("bottomright", legend=leg.txt, bty="n",
       title=expression(beta),
       lty=1, col=mycolors[1:length(beta)], 
       text.col=mycolors[1:length(beta)])


###################################################
### code chunk number 206: icda-notes.Rnw:7054-7061
###################################################
beta <- seq(0, betamax, length=100)
alpha <- abratio*beta
loglkd <-
    apply(dbinom(y, 1, t(plogis(alpha + (beta %o% x))), log = TRUE), 2, sum)
lkd <- exp(loglkd)
plot(beta, lkd, type="l", xlab=expression(beta), 
     ylab=expression(paste("Profile Likelihood:  ", l(beta))))


###################################################
### code chunk number 207: icda-notes.Rnw:7068-7070
###################################################
plot(beta, loglkd, type="l", xlab=expression(beta), 
     ylab=expression(paste("Profile log-Likelihood:  ", L(beta))))


###################################################
### code chunk number 208: icda-notes.Rnw:7129-7130
###################################################
latex(sattab,
      file="", booktabs=TRUE,
      rowlabel=names(dimnames(sattab))[1], 
      cgroup=names(dimnames(sattab))[2],
      n.cgroup=4,
      cgroupTexCmd="normalfont"
      )


###################################################
### code chunk number 209: icda-notes.Rnw:7147-7153
###################################################
data(jobsatisfaction)
head(jobsatisfaction)
jobsatisfaction <- 
  transform(jobsatisfaction, JobSat = factor(JobSat,
                 labels = c("Diss","Little","Mod","Very"), 
                 ordered = TRUE))


###################################################
### code chunk number 210: icda-notes.Rnw:7158-7162
###################################################
library(reshape2)
jobsatw <- dcast(jobsatisfaction, Income ~ JobSat, sum,
                 value.var = "Freq")
jobsatw


###################################################
### code chunk number 211: icda-notes.Rnw:7168-7173
###################################################
library(VGAM)
jobsat.fit1 <-
  vglm(cbind(Diss,Little,Mod,Very) ~ Income,
       family=multinomial, data=jobsatw)
coef(jobsat.fit1)


###################################################
### code chunk number 212: icda-notes.Rnw:7178-7179 (eval = FALSE)
###################################################
## summary(jobsat.fit1)


###################################################
### code chunk number 213: icda-notes.Rnw:7182-7184
###################################################
tmp <- paragraphs(summary(jobsat.fit1))
tmp[1:2]


###################################################
### code chunk number 214: icda-notes.Rnw:7189-7190
###################################################
tmp[3:(length(tmp)-2)]


###################################################
### code chunk number 215: icda-notes.Rnw:7195-7196
###################################################
tmp[-(1:(length(tmp)-2))]


###################################################
### code chunk number 216: icda-notes.Rnw:7201-7211
###################################################
lp.ltx(coef(jobsat.fit1)[c("(Intercept):1","Income:1")], "jslpone",
       digits=3, format="f")
lp.ltx(coef(jobsat.fit1)[c("(Intercept):2","Income:2")], "jslptwo",
       digits=3, format="f")
lp.ltx(coef(jobsat.fit1)[c("(Intercept):3","Income:3")], "jslpthree",
       digits=3, format="f")
coef.ltx(coef(jobsat.fit1), "jscf", digits=3, format="f")
coef.ltx(exp(coef(jobsat.fit1)), "jscfexp", digits=2, format="f")
coef.ltx(10*coef(jobsat.fit1), "jscften", digits=2, format="f")
coef.ltx(exp(10*coef(jobsat.fit1)), "jscftenexp", digits=2, format="f")


###################################################
### code chunk number 217: icda-notes.Rnw:7288-7291
###################################################
jsodds <- exp(c(1,35) %*% matrix(coef(jobsat.fit1), byrow=TRUE, nrow=2))
jsodds <- c(jsodds,1)
jspihat <- jsodds/sum(jsodds)


###################################################
### code chunk number 218: jobsat.fit2
###################################################
jobsat.fit2 <-
  vglm(cbind(Diss,Little,Mod,Very) ~ 1,
       family=multinomial, data=jobsatw)


###################################################
### code chunk number 219: icda-notes.Rnw:7437-7442
###################################################
jobsat.fit2 <-
  vglm(cbind(Diss,Little,Mod,Very) ~ 1,
       family=multinomial, data=jobsatw)
deviance(jobsat.fit2)
df.residual(jobsat.fit2)
pchisq(deviance(jobsat.fit2) - deviance(jobsat.fit1), 3,
       lower.tail=FALSE)


###################################################
### code chunk number 220: icda-notes.Rnw:7445-7446 (eval = FALSE)
###################################################
## summary(jobsat.fit2)


###################################################
### code chunk number 221: icda-notes.Rnw:7449-7450
###################################################
tmp <- paragraphs(summary(jobsat.fit2))


###################################################
### code chunk number 222: icda-notes.Rnw:7456-7458
###################################################
tmp[1]
tmp[3:(length(tmp)-1)]


###################################################
### code chunk number 223: jobsat.cl1
###################################################
jobsat.cl1 <-
  vglm(cbind(Diss,Little,Mod,Very) ~ Income,
       family=cumulative(parallel=TRUE), data=jobsatw)


###################################################
### code chunk number 224: icda-notes.Rnw:7514-7527
###################################################
jscl1cfm <- coef(summary(jobsat.cl1))
jscl1cf <- jscl1cfm[,"Estimate"]
jscl1se <- jscl1cfm[,"Std. Error"]
jscl1z <- jscl1cfm[,"z value"]
coef.ltx(jscl1cf, "jsclcf", digits=4, format="f")
coef.ltx(jscl1se, "jsclse", digits=4, format="f")
coef.ltx(jscl1z, "jsclz", digits=2, format="f")
coef.ltx(jscl1z^2, "jsclzsq", digits=2, format="f")
coef.ltx(exp(jscl1cf), "jsclcfexp", digits=2, format="f")
coef.ltx(10*jscl1cf, "jsclcften", digits=3, format="f")
coef.ltx(exp(10*jscl1cf), "jsclcftenexp", digits=2, format="f")
coef.ltx(-jscl1cf, "jsclncf", digits=4, format="f")
coef.ltx(exp(-jscl1cf), "jsclncfexp", digits=3, format="f")


###################################################
### code chunk number 225: jobsat.cl0
###################################################
jobsat.cl0 <-
  vglm(cbind(Diss,Little,Mod,Very) ~ 1,
       family=cumulative(parallel=TRUE), data=jobsatw)


###################################################
### code chunk number 226: icda-notes.Rnw:7540-7541
###################################################
latex(sattab,
      file="", booktabs=TRUE,
      rowlabel=names(dimnames(sattab))[1], 
      cgroup=names(dimnames(sattab))[2],
      n.cgroup=4,
      cgroupTexCmd="normalfont"
      )


###################################################
### code chunk number 227: icda-notes.Rnw:7565-7566 (eval = FALSE)
###################################################
## jobsat.cl1 <-
##   vglm(cbind(Diss,Little,Mod,Very) ~ Income,
##        family=cumulative(parallel=TRUE), data=jobsatw)


###################################################
### code chunk number 228: icda-notes.Rnw:7569-7570 (eval = FALSE)
###################################################
## summary(jobsat.cl1)


###################################################
### code chunk number 229: icda-notes.Rnw:7573-7575
###################################################
tmp <- paragraphs(summary(jobsat.cl1))
tmp[1:2]


###################################################
### code chunk number 230: icda-notes.Rnw:7580-7581
###################################################
tmp[3:(length(tmp) - 2)]


###################################################
### code chunk number 231: icda-notes.Rnw:7618-7622
###################################################
jobsat.cl1r <-
  vglm(cbind(Very,Mod,Little,Diss) ~ Income,
       family=cumulative(parallel=TRUE), data=jobsatw)
coef(jobsat.cl1r)


###################################################
### code chunk number 232: jobsat.cl0
###################################################
jobsat.cl0 <-
  vglm(cbind(Diss,Little,Mod,Very) ~ 1,
       family=cumulative(parallel=TRUE), data=jobsatw)


###################################################
### code chunk number 233: icda-notes.Rnw:7662-7667
###################################################
jobsat.cl0 <-
  vglm(cbind(Diss,Little,Mod,Very) ~ 1,
       family=cumulative(parallel=TRUE), data=jobsatw)
deviance(jobsat.cl0)
deviance(jobsat.cl1)
pchisq(deviance(jobsat.cl0) - deviance(jobsat.cl1), 1,
       lower.tail=FALSE)


###################################################
### code chunk number 234: icda-notes.Rnw:7743-7748
###################################################
data(ideology)
head(ideology)
library(reshape2)
ideow <- dcast(ideology, Gender + Party ~ Ideology, 
               value_var="Freq")


###################################################
### code chunk number 235: icda-notes.Rnw:7753-7758
###################################################
ideow
library(VGAM)
ideo.cl1 <- 
  vglm(cbind(VLib,SLib,Mod,SCon,VCon) ~ Gender + Party,
       family=cumulative(parallel=TRUE), data=ideow)


###################################################
### code chunk number 236: icda-notes.Rnw:7761-7762 (eval = FALSE)
###################################################
## summary(ideo.cl1)


###################################################
### code chunk number 237: icda-notes.Rnw:7767-7769
###################################################
tmp <- paragraphs(summary(ideo.cl1))
tmp[c(1,3:5)]


###################################################
### code chunk number 238: icda-notes.Rnw:7774-7775
###################################################
tmp[6:(length(tmp)-1)]


###################################################
### code chunk number 239: icda-notes.Rnw:7778-7782
###################################################
deviance(ideo.cl1)
df.residual(ideo.cl1)
pchisq(deviance(ideo.cl1), df.residual(ideo.cl1),
       lower.tail = FALSE)


###################################################
### code chunk number 240: icda-notes.Rnw:7787-7801
###################################################
ideocl1cfm <- coef(summary(ideo.cl1))
ideocl1cf <- ideocl1cfm[,"Estimate"]
ideocl1se <- ideocl1cfm[,"Std. Error"]
ideocl1z <- ideocl1cfm[,"z value"]
coef.ltx(ideocl1cf, "cf", digits=3, format="f")
ecoef.ltx(ideocl1cf, "ecf", digits=2, format="f")
coef.ltx(ideocl1se, "cfse", digits=3, format="f")
coef.ltx(ideocl1z, "cfz", digits=2, format="f")
coef.ltx(ideocl1z^2, "cfzsq", digits=2, format="f")
## coef.ltx(ideocl1z^2, "ideoclzsq", digits=2, format="f")
## coef.ltx(exp(ideocl1cf), "ecf", digits=2, format="f")
## coef.ltx(10*ideocl1cf, "cften", digits=3, format="f")
## coef.ltx(exp(10*ideocl1cf), "cftenexp", digits=2, format="f")
## coef.ltx(exp(-ideocl1cf), "ideoclnecf", digits=3, format="f")


###################################################
### code chunk number 241: icda-notes.Rnw:7870-7879
###################################################
ideo.cl2 <- 
  vglm(cbind(VLib,SLib,Mod,SCon,VCon) ~ Gender,
       family=cumulative(parallel=TRUE), data=ideow)
deviance(ideo.cl2)
df.residual(ideo.cl2)
deviance(ideo.cl2) - deviance(ideo.cl1)
pchisq(deviance(ideo.cl2) - deviance(ideo.cl1),
       df.residual(ideo.cl2) - df.residual(ideo.cl1),
       lower.tail=FALSE)


###################################################
### code chunk number 242: icda-notes.Rnw:7886-7891
###################################################
ideow
ideo.csum <- t(apply(ideow[,-(1:2)], 1, cumsum))
ideo.csum
ideo.cprop <- ideo.csum[,1:4]/ideo.csum[,5]
ideo.ecl <- qlogis(ideo.cprop) # empirical cumul. logits


###################################################
### code chunk number 243: icda-notes.Rnw:7898-7911
###################################################
layout(matrix(c(1,2), nrow=1), widths=c(2,1))
opar <- par(mar = c(4,4,0,0) + 0.1)
plot(c(0.5,4.0), range(pretty(ideo.ecl)), type="n", axes=FALSE,
     xlab="Ideology", ylab="Empirical Cumulative Logits")
axis(1, 1:4, c("VLib","SLib","Mod","SCon"), lty=0)
axis(2, pretty(ideo.ecl))
for (i in 1:4) points(1:4, ideo.ecl[i,], pch=i)
plot.new()
par(mar = c(4,0,0,0) + 0.1)
legend("left", with(ideow, paste(Gender, Party)), pch=1:4,
       bty="n")
layout(1)
par(opar)


###################################################
### code chunk number 244: icda-notes.Rnw:7918-7922
###################################################
ideo.cl3 <- 
  vglm(cbind(VLib,SLib,Mod,SCon,VCon) ~ Gender*Party,
       family=cumulative(parallel=TRUE), data=ideow)
coef(summary(ideo.cl3))


###################################################
### code chunk number 245: icda-notes.Rnw:7927-7933
###################################################
deviance(ideo.cl3)
df.residual(ideo.cl3)
deviance(ideo.cl1) - deviance(ideo.cl3)
pchisq(deviance(ideo.cl1) - deviance(ideo.cl3),
       df.residual(ideo.cl1) - df.residual(ideo.cl3),
       lower.tail=FALSE)


###################################################
### code chunk number 246: icda-notes.Rnw:8349-8356
###################################################
crossover <- 
  matrix(c(12,10,49,15), nrow=2,
         dimnames=list(Drug=c("S","F"),
                       Placebo=c("S","F")))
crossover <- as.table(crossover)
crossover
mcnemar.test(crossover, correct = FALSE)


###################################################
### code chunk number 247: icda-notes.Rnw:8469-8472
###################################################
data(moviereviews)
moviereviews
cohens.kappa(moviereviews)


###################################################
### code chunk number 248: icda-notes.Rnw:8607-8615
###################################################
library(gee)
crossover
cross.df <- data.frame(crossover)
cross.df <- 
  transform(cross.df, 
            Drug = as.numeric(Drug=="S"), 
            Placebo = as.numeric(Placebo=="S"))
cross.df


###################################################
### code chunk number 249: icda-notes.Rnw:8620-8625
###################################################
Freq <- cross.df$Freq
cross.df$Freq <- NULL
cross.df <- cross.df[rep(1:4, Freq),]
rm(Freq)
head(cross.df)


###################################################
### code chunk number 250: icda-notes.Rnw:8630-8633
###################################################
rownames(cross.df) <- NULL
head(cross.df)
xtabs(~ Drug + Placebo, cross.df)


###################################################
### code chunk number 251: icda-notes.Rnw:8639-8643
###################################################
dim(cross.df)
cross.df$Subject <- factor(1:86)
crossm <- melt(cross.df)
head(crossm)


###################################################
### code chunk number 252: icda-notes.Rnw:8648-8655
###################################################
## VERY IMPORTANT: Data should be ordered by "cluster"
crossm <- crossm[order(crossm$Subject),]
head(crossm)
names(crossm)[2:3] <- c("Treat","Resp")
names(crossm)
crossm <- 
  transform(crossm, Treat=relevel(Treat, "Placebo"))


###################################################
### code chunk number 253: icda-notes.Rnw:8666-8672
###################################################
cross.gee1 <- 
  gee(Resp ~ Treat, id=Subject, data=crossm, 
      family=binomial, corstr="exchangeable")
cross.gee2 <- 
  gee(Resp ~ Treat, id=Subject, data=crossm, 
      family=binomial, corstr="independence")


###################################################
### code chunk number 254: icda-notes.Rnw:8677-8678 (eval = FALSE)
###################################################
## summary(cross.gee1)


###################################################
### code chunk number 255: icda-notes.Rnw:8681-8683
###################################################
tmp <- paragraphs(summary(cross.gee1))
tmp[1:4]


###################################################
### code chunk number 256: icda-notes.Rnw:8688-8689
###################################################
tmp[5:(length(tmp))]


###################################################
### code chunk number 257: icda-notes.Rnw:8695-8697
###################################################
coef(summary(cross.gee1))
coef(summary(cross.gee2))


###################################################
### code chunk number 258: icda-notes.Rnw:8850-8856
###################################################
library(gee)
data(depression)
head(depression)
dep.gee1 <-
  gee((response == "normal") ~ severity + drug*time, 
      id=subject, data=depression, family=binomial)


###################################################
### code chunk number 259: icda-notes.Rnw:8861-8862 (eval = FALSE)
###################################################
## summary(dep.gee1)


###################################################
### code chunk number 260: icda-notes.Rnw:8865-8867
###################################################
tmp <- paragraphs(summary(dep.gee1))
tmp[1:4]


###################################################
### code chunk number 261: icda-notes.Rnw:8870-8872
###################################################
## Fix blanks issue (tmp[5])
tmp[6]


###################################################
### code chunk number 262: icda-notes.Rnw:8877-8878
###################################################
tmp[7:(length(tmp))]


###################################################
### code chunk number 263: icda-notes.Rnw:8892-8897
###################################################
dep.gee2 <-
  gee((response == "normal") ~ severity + drug*time, 
      id=subject, data=depression, family=binomial,
      corstr="exchangeable")
dep.gee2$working.correlation


###################################################
### code chunk number 264: icda-notes.Rnw:8902-8904
###################################################
coef(summary(dep.gee1))[,c(1,2,4)]
coef(summary(dep.gee2))[,c(1,2,4)]


###################################################
### code chunk number 265: icda-notes.Rnw:8909-8913
###################################################
dep.gee3 <-
  gee((response == "normal") ~ (severity + drug)*time, 
      id=subject, data=depression, family=binomial)
round(coef(summary(dep.gee3))[,"Robust z"],2)


###################################################
### code chunk number 266: icda-notes.Rnw:9071-9078
###################################################
library(lme4)
data(depression)
head(depression)
dep.lme4.1 <- 
  glmer((response == "normal")
        ~ severity + drug*time + (1 | subject), 
        family = binomial, data = depression)


###################################################
### code chunk number 267: icda-notes.Rnw:9083-9084 (eval = FALSE)
###################################################
## summary(dep.lme4.1)


###################################################
### code chunk number 268: icda-notes.Rnw:9087-9089
###################################################
tmp <- paragraphs(summary(dep.lme4.1))
tmp[1:2]


###################################################
### code chunk number 269: icda-notes.Rnw:9094-9095
###################################################
tmp[3:(length(tmp))]


###################################################
### code chunk number 270: dpbetatab
###################################################
depbeta.tab <- coef(summary(dep.lme4.1))[,1:2]
depbeta.tab <- cbind(depbeta.tab, coef(summary(dep.gee1))[,c(1,4)])
colnames(depbeta.tab) <- rep(c("Est", "SE"), 2)
rownames(depbeta.tab) <- c("alpha", paste("beta.", 1:4, sep=""))
latex(round(depbeta.tab,2), file="",
      first.hline.double=FALSE,
      # dcolumn=TRUE,
      rowlabel="", 
      cgroup=c("GLMM", "GEE"), n.cgroup=c(2,2),
      cgroupTexCmd="normalfont")


###################################################
### code chunk number 271: icda-notes.Rnw:9144-9153
###################################################
b <- 2
sigma <- 4
m <- 12
bstar <- b/sqrt(1 + sigma^2)
set.seed(362436)
u <- rnorm(m, sd = sigma)
curve(pnorm(bstar * x), -3/bstar, 3/bstar, ylab = "Success Probability")
for (i in seq(along = u)) curve(pnorm(b * x + u[i]), lty = 2, add = TRUE)
legend("bottomright", c("Conditional","Marginal"), lty=c(2,1))


###################################################
### code chunk number 272: icda-notes.Rnw:9219-9226
###################################################
data(teratology)
## Data also include HB = mother's hemoglobin level
head(teratology)
terat.binom <-
  glm(cbind(R, N-R) ~ GRP, family = binomial,
      data = teratology)
chisqstat(terat.binom)


###################################################
### code chunk number 273: icda-notes.Rnw:9229-9230 (eval = FALSE)
###################################################
## summary(terat.binom)


###################################################
### code chunk number 274: icda-notes.Rnw:9235-9237
###################################################
tmp <- paragraphs(summary(terat.binom))
tmp[1:(length(tmp)-2)]


###################################################
### code chunk number 275: icda-notes.Rnw:9278-9291
###################################################
## Adding explicit litter variable for remaining analyses:
teratology$Litter <- as.factor(1:nrow(teratology))
## Need data in ungrouped (binary) format for GEE (???):
teratbnry <- teratology
teratbnry$N <- teratbnry$R <- NULL
teratbnry <-
  teratbnry[rep(1:nrow(teratology), teratology$N),]
rownames(teratbnry) <- NULL  # cleaning up row names
teratbnry$Response <- 
  with(teratology,
       unlist(apply(cbind(R, N-R), 1,
              function(x) rep(c("Dead","Alive"), x))))
head(teratbnry, 4)


###################################################
### code chunk number 276: icda-notes.Rnw:9299-9309
###################################################
library(gee)
terat.gee <-
  gee((Response == "Dead") ~ GRP, id = Litter,
      data = teratbnry, family = binomial, 
      corstr = "exchangeable")
coef(summary(terat.gee))[,c("Estimate","Robust S.E.")]

## Big working correlation matrix (17 x 17), but
## all correlations equal with exchangeable struc:
terat.gee$working.correlation[1,2]


###################################################
### code chunk number 277: icda-notes.Rnw:9317-9328
###################################################
## glmer can use grouped or ungrouped data.
library(lme4)
## Using grouped data
terat.glmm <-
  glmer(cbind(R, N-R) ~ GRP + (1|Litter),
        data = teratology, family = binomial)
## Using ungrouped binary data
terat.glmm <-
  glmer((Response == "Dead") ~ GRP + (1|Litter),
        data = teratbnry, family = binomial)
coef(summary(terat.glmm))


###################################################
### code chunk number 278: icda-notes.Rnw:9337-9347
###################################################
tmp <- cbind(coef(summary(terat.binom))[,c("Estimate","Std. Error")],
             coef(summary(terat.gee))[,c("Estimate","Robust S.E.")], 
             coef(summary(terat.glmm))[,c("Estimate","Std. Error")])
tmp <- round(tmp, 2)
print.est.se <- function(x) paste(x[1], " (", x[2], ")", sep="")
tmp <- cbind(apply(tmp[,1:2], 1, print.est.se),
             apply(tmp[,3:4], 1, print.est.se),
             apply(tmp[,5:6], 1, print.est.se))
colnames(tmp) <- c("Binomial ML", "GEE", "GLMM")
xtable(tmp, align = rep("r", 1 + ncol(tmp)))


###################################################
### code chunk number 279: icda-notes.Rnw:9509-9510
###################################################
latex(sattab,
      file="", booktabs=TRUE,
      rowlabel=names(dimnames(sattab))[1], 
      cgroup=names(dimnames(sattab))[2],
      n.cgroup=4,
      cgroupTexCmd="normalfont"
      )


###################################################
### code chunk number 280: icda-notes.Rnw:9592-9596
###################################################
sattab
jobsat <- as.data.frame(sattab)
names(jobsat)
names(jobsat)[2] <- "Satis"


###################################################
### code chunk number 281: icda-notes.Rnw:9601-9602
###################################################
jobsat


###################################################
### code chunk number 282: icda-notes.Rnw:9607-9613
###################################################
levels(jobsat$Income)
levels(jobsat$Satis)
options(contrasts=c("contr.SAS","contr.poly"))
jobsat.indep <-
  glm(Freq ~ Income + Satis, family=poisson, 
      data=jobsat)


###################################################
### code chunk number 283: icda-notes.Rnw:9618-9619 (eval = FALSE)
###################################################
## summary(jobsat.indep)


###################################################
### code chunk number 284: icda-notes.Rnw:9622-9624
###################################################
tmp <- paragraphs(summary(jobsat.indep))
tmp[1:3]


###################################################
### code chunk number 285: icda-notes.Rnw:9629-9630
###################################################
tmp[4:(length(tmp))]


###################################################
### code chunk number 286: icda-notes.Rnw:9633-9634
###################################################
chisqstat(jobsat.indep)


###################################################
### code chunk number 287: icda-notes.Rnw:9639-9643
###################################################
jobsat.saturated <- update(jobsat.indep, . ~ Income*Satis)
anova(jobsat.indep, jobsat.saturated, test="Chisq")
## Set contrasts back to R defaults
options(contrasts=c("contr.treatment","contr.poly"))


###################################################
### code chunk number 288: icda-notes.Rnw:9701-9710
###################################################
teens <-
  array(c(911,44,3,2, 538,456,43,279),
        dim = c(2,2,2),
        dimnames = list(cigs=c("yes","no"),
          alc=c("yes","no"), mj=c("yes","no")))
## Next line just for Table 7.4.  Not required.
teens <- aperm(teens, c(3,1,2))
teens <- as.table(teens)
ftable(teens, row.vars=c("alc","cigs"))


###################################################
### code chunk number 289: icda-notes.Rnw:9715-9722
###################################################
teens.df <- as.data.frame(teens)
teens.df
teens.df <- 
  transform(teens.df,
            cigs = relevel(cigs, "no"),
            alc = relevel(alc, "no"),
            mj = relevel(mj, "no"))


###################################################
### code chunk number 290: icda-notes.Rnw:9727-9734
###################################################
teens.AC.AM.CM <-
  glm(Freq ~ alc*cigs + alc*mj + cigs*mj,
      family=poisson, data=teens.df)
### Another way:
## teens.AC.AM.CM <-
##   glm(Freq ~ alc*cigs*mj - alc:cigs:mj,
##       family=poisson, data=teens.df)


###################################################
### code chunk number 291: icda-notes.Rnw:9737-9738 (eval = FALSE)
###################################################
## summary(teens.AC.AM.CM)


###################################################
### code chunk number 292: icda-notes.Rnw:9741-9742
###################################################
tmp <- paragraphs(summary(teens.AC.AM.CM))


###################################################
### code chunk number 293: icda-notes.Rnw:9747-9748
###################################################
tmp[c(1,3:(length(tmp)-2))]


###################################################
### code chunk number 294: icda-notes.Rnw:9757-9760
###################################################
df.residual(teens.AC.AM.CM)
deviance(teens.AC.AM.CM)
chisqstat(teens.AC.AM.CM)


###################################################
### code chunk number 295: icda-notes.Rnw:9767-9769
###################################################
teens.ACM <- update(teens.AC.AM.CM, . ~ alc*cigs*mj)
anova(teens.AC.AM.CM, teens.ACM, test="Chisq")


###################################################
### code chunk number 296: icda-notes.Rnw:9776-9777
###################################################
drop1(teens.AC.AM.CM, test="Chisq")


###################################################
### code chunk number 297: icda-notes.Rnw:9792-9794
###################################################
teens.AM.CM <- update(teens.AC.AM.CM, . ~ alc*mj + cigs*mj)
anova(teens.AM.CM, teens.AC.AM.CM, test="Chisq")


###################################################
### code chunk number 298: icda-notes.Rnw:9804-9822
###################################################
teens.AM.CM <-
  update(teens.AC.AM.CM, . ~ alc*mj + cigs*mj)
teens.AC.M <-
  update(teens.AC.AM.CM, . ~ alc*cigs + mj)
teens.A.C.M <-
  update(teens.AC.AM.CM, . ~ alc + cigs + mj)
teens.ACM <-
  update(teens.AC.AM.CM, . ~ alc*cigs* mj)
table.7.4 <-
  data.frame(predict(teens.A.C.M, type="response"))
table.7.4 <-
  cbind(table.7.4, predict(teens.AC.M, type="response"))
table.7.4 <-
  cbind(table.7.4, predict(teens.AM.CM, type="response"))
table.7.4 <-
  cbind(table.7.4, predict(teens.AC.AM.CM, type="response"))
table.7.4 <-
  cbind(table.7.4, predict(teens.ACM, type="response"))


###################################################
### code chunk number 299: icda-notes.Rnw:9829-9836
###################################################
table.7.4 <- signif(table.7.4, 3)
table.7.4 <- 
 cbind(teens.df[,c("alc","cigs","mj")],
       table.7.4)
names(table.7.4) <-
  c("alc","cigs","mj",
    "(A,C,M)","(AC,M)","(AM,CM)","(AC,AM,CM)","(ACM)")


###################################################
### code chunk number 300: icda-notes.Rnw:9841-9842
###################################################
table.7.4


###################################################
### code chunk number 301: icda-notes.Rnw:9868-9874
###################################################
AC.AM.CM <- predict(teens.AC.AM.CM, type="response")
AC.AM.CM <- array(AC.AM.CM, c(2,2,2), 
                  dimnames = list(mj=c("yes","no"),
                    cigs=c("yes","no"), alc=c("yes","no")))
AC.AM.CM <- aperm(AC.AM.CM, 3:1)
## signif(AC.AM.CM, 3)


###################################################
### code chunk number 302: icda-notes.Rnw:9903-9909
###################################################
AM.CM <- predict(teens.AM.CM, type="response")
AM.CM <- array(AM.CM, c(2,2,2), 
               dimnames = list(mj=c("yes","no"),
                 cigs=c("yes","no"), alc=c("yes","no")))
AM.CM <- aperm(AM.CM, 3:1)
## signif(AM.CM, 3)


###################################################
### code chunk number 303: satMosaic1 (eval = FALSE)
###################################################
## library(vcd)
## mosaic(sattab)


###################################################
### code chunk number 304: icda-notes.Rnw:9988-9989
###################################################
library(vcd)
mosaic(sattab)


###################################################
### code chunk number 305: icda-notes.Rnw:9996-9997 (eval = FALSE)
###################################################
## library(vcd)
## mosaic(sattab)


###################################################
### code chunk number 306: icda-notes.Rnw:10001-10002 (eval = FALSE)
###################################################
## mosaic(~ Income + Satis, data = jobsat)


###################################################
### code chunk number 307: satMosaic2 (eval = FALSE)
###################################################
## mosaic(sattab, split_vertical = TRUE)


###################################################
### code chunk number 308: icda-notes.Rnw:10013-10014
###################################################
mosaic(sattab, split_vertical = TRUE)


###################################################
### code chunk number 309: icda-notes.Rnw:10020-10022
###################################################
(sat.chisq <- chisq.test(sattab))
round(sat.chisq$expected, 1)


###################################################
### code chunk number 310: satMosaic3 (eval = FALSE)
###################################################
## mosaic(sattab, split_vertical = TRUE, main = "Observed")


###################################################
### code chunk number 311: satMosaic4 (eval = FALSE)
###################################################
## mosaic(sattab, split_vertical = TRUE, type = "expected",
##        main = "Expected")


###################################################
### code chunk number 312: icda-notes.Rnw:10038-10039
###################################################
mosaic(sattab, split_vertical = TRUE, main = "Observed")


###################################################
### code chunk number 313: icda-notes.Rnw:10041-10042
###################################################
mosaic(sattab, split_vertical = TRUE, type = "expected",
       main = "Expected")


###################################################
### code chunk number 314: icda-notes.Rnw:10049-10050
###################################################
round(sat.chisq$stdres, 1)


###################################################
### code chunk number 315: icda-notes.Rnw:10054-10055
###################################################
round(rstandard(jobsat.indep, type = "pearson"), 1)


###################################################
### code chunk number 316: satMosaic5 (eval = FALSE)
###################################################
## mosaic(sattab, gp = shading_Friendly)


###################################################
### code chunk number 317: satMosaic6 (eval = FALSE)
###################################################
## mosaic(sattab, residuals = sat.chisq$stdres,
##        gp = shading_hcl,
##        gp_args = list(p.value = sat.chisq$p.value,
##                       interpolate = c(2,4)))


###################################################
### code chunk number 318: icda-notes.Rnw:10083-10084
###################################################
mosaic(sattab, gp = shading_Friendly)


###################################################
### code chunk number 319: icda-notes.Rnw:10094-10095
###################################################
mosaic(sattab, residuals = sat.chisq$stdres,
       gp = shading_hcl,
       gp_args = list(p.value = sat.chisq$p.value,
                      interpolate = c(2,4)))


###################################################
### code chunk number 320: icda-notes.Rnw:10105-10106
###################################################
ftable(Eye ~ Sex + Hair, data = HairEyeColor)


###################################################
### code chunk number 321: icda-notes.Rnw:10118-10121
###################################################
haireye <- margin.table(HairEyeColor, 1:2) 
haireye
(he.chisq <- chisq.test(haireye))


###################################################
### code chunk number 322: icda-notes.Rnw:10132-10132
###################################################



###################################################
### code chunk number 323: heMosaic1 (eval = FALSE)
###################################################
## mosaic(haireye, residuals = he.chisq$stdres,
##     gp = shading_hcl,
##     gp_args = list(p.value = he.chisq$p.value,
##                    interpolate = c(2,4)),
##     labeling_args = list(abbreviate_labs = c(Eye = 3)))


###################################################
### code chunk number 324: icda-notes.Rnw:10148-10149
###################################################
mosaic(haireye, residuals = he.chisq$stdres,
    gp = shading_hcl,
    gp_args = list(p.value = he.chisq$p.value,
                   interpolate = c(2,4)),
    labeling_args = list(abbreviate_labs = c(Eye = 3)))


###################################################
### code chunk number 325: teenMosaic1 (eval = FALSE)
###################################################
## mosaic(teens)


###################################################
### code chunk number 326: icda-notes.Rnw:10177-10178
###################################################
mosaic(teens)


###################################################
### code chunk number 327: icda-notes.Rnw:10186-10187 (eval = FALSE)
###################################################
## mosaic(teens)


###################################################
### code chunk number 328: icda-notes.Rnw:10191-10192
###################################################
ftable(round(prop.table(teens), 3))


###################################################
### code chunk number 329: icda-notes.Rnw:10196-10198 (eval = FALSE)
###################################################
## mosaic(~ mj + cigs + alc, data = teens)
## mosaic(~ mj + cigs + alc, data = teens.df)


###################################################
### code chunk number 330: icda-notes.Rnw:10211-10223
###################################################
table.7.8 <- teens.df[,c("alc","cigs","mj","Freq")]
table.7.8 <- cbind(table.7.8,
  round(predict(teens.AM.CM, type = "response"),1))
table.7.8 <- cbind(table.7.8,
  round(rstandard(teens.AM.CM, type = "pearson"),2))
table.7.8 <- cbind(table.7.8,
  round(predict(teens.AC.AM.CM, type = "response"),1))
table.7.8 <- cbind(table.7.8,
  round(rstandard(teens.AC.AM.CM, type = "pearson"),2))
names(table.7.8) <-
  c("A","C","M","Obs","(AM,CM)","StdRes",
    "(AC,AM,CM)","StdRes")


###################################################
### code chunk number 331: icda-notes.Rnw:10229-10230
###################################################
table.7.8


###################################################
### code chunk number 332: teenMosaic4 (eval = FALSE)
###################################################
## library(vcdExtra)
## mosaic(teens.AM.CM, ~ mj + cigs + alc)


###################################################
### code chunk number 333: teenMosaic5 (eval = FALSE)
###################################################
## mosaic(teens.AC.AM.CM, ~ mj + cigs + alc)


###################################################
### code chunk number 334: icda-notes.Rnw:10264-10265
###################################################
library(vcdExtra)
mosaic(teens.AM.CM, ~ mj + cigs + alc)


###################################################
### code chunk number 335: icda-notes.Rnw:10270-10271
###################################################
mosaic(teens.AC.AM.CM, ~ mj + cigs + alc)


###################################################
### code chunk number 336: teenMosaic6 (eval = FALSE)
###################################################
## mosaic(teens.AM.CM,  ~ mj + cigs + alc,
##        residuals_type = "rstandard")


###################################################
### code chunk number 337: teenMosaic7 (eval = FALSE)
###################################################
## mosaic(teens.AC.AM.CM,  ~ mj + cigs + alc,
##        residuals_type = "rstandard")


###################################################
### code chunk number 338: icda-notes.Rnw:10292-10293
###################################################
mosaic(teens.AM.CM,  ~ mj + cigs + alc,
       residuals_type = "rstandard")


###################################################
### code chunk number 339: icda-notes.Rnw:10298-10299
###################################################
mosaic(teens.AC.AM.CM,  ~ mj + cigs + alc,
       residuals_type = "rstandard")


###################################################
### code chunk number 340: teenMosaic8 (eval = FALSE)
###################################################
## mosaic(teens.AM.CM, ~ mj + cigs + alc,
##   residuals = rstandard(teens.AM.CM, type = "pearson"))


###################################################
### code chunk number 341: teenMosaic9 (eval = FALSE)
###################################################
## mosaic(teens.AC.AM.CM, ~ mj + cigs + alc,
##   residuals = rstandard(teens.AC.AM.CM, type = "pearson"))


###################################################
### code chunk number 342: icda-notes.Rnw:10324-10325
###################################################
mosaic(teens.AM.CM, ~ mj + cigs + alc,
  residuals = rstandard(teens.AM.CM, type = "pearson"))


###################################################
### code chunk number 343: icda-notes.Rnw:10330-10331
###################################################
mosaic(teens.AC.AM.CM, ~ mj + cigs + alc,
  residuals = rstandard(teens.AC.AM.CM, type = "pearson"))


###################################################
### code chunk number 344: icda-notes.Rnw:10395-10396
###################################################
ftable(UCBAdmissions, 
       row.vars="Dept", col.vars=c("Gender","Admit"))


###################################################
### code chunk number 345: icda-notes.Rnw:10419-10427
###################################################
UCB.logit <-
  glm(cbind(Admitted, Rejected) ~ Gender + Dept,
            family = binomial, data = UCBw)
c(deviance(UCB.logit), df.residual(UCB.logit))
UCB.loglin <- 
  glm(Freq ~ Admit*Gender + Admit*Dept + Gender*Dept,
      family = poisson, data = UCBdf)
c(deviance(UCB.loglin), df.residual(UCB.loglin))


###################################################
### code chunk number 346: icda-notes.Rnw:10472-10473
###################################################
coef(UCB.logit)


###################################################
### code chunk number 347: icda-notes.Rnw:10482-10483
###################################################
coef(UCB.loglin)


###################################################
### code chunk number 348: icda-notes.Rnw:10799-10800
###################################################
exp(coef(teens.AM.CM)[5])


###################################################
### code chunk number 349: icda-notes.Rnw:10806-10811
###################################################
AM.CM.fitted <- teens
AM.CM.fitted[,,] <- predict(teens.AM.CM, type="response")
AM.CM.fitted[,"yes",]
AM.CM.fitted[,"no",]
AM.CM.fitted[,"yes",] + AM.CM.fitted[,"no",]


###################################################
### code chunk number 350: icda-notes.Rnw:10839-10842
###################################################
AM.CM.fitted["yes",,]
AM.CM.fitted["no",,]
AM.CM.fitted["yes",,] + AM.CM.fitted["no",,]


###################################################
### code chunk number 351: icda-notes.Rnw:10874-10876
###################################################
data(teens)
ftable(R + G + M ~ A + C, data = teens)


###################################################
### code chunk number 352: icda-notes.Rnw:10906-10909
###################################################
teens.df <- as.data.frame(teens)
ACM <- margin.table(teens, 1:3)
ACM.df <- as.data.frame(ACM)


###################################################
### code chunk number 353: icda-notes.Rnw:10914-10921
###################################################
teens.m6 <- 
  glm(Freq ~ A*C + A*M + C*M + A*G + A*R + G*M + G*R,
      family = poisson, data = teens.df)
AC.AM.CM <- glm(Freq ~ A*C + A*M + C*M,
                family = poisson, data = ACM.df)
coef(teens.m6)
coef(AC.AM.CM)


