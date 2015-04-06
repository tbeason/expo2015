funds <- read.csv("~/GitHub/expo2015/listoffunds.csv", stringsAsFactors=FALSE)
allFunds <- funds$Symbol
require(quantmod)
require(plyr)
require(xts)
require(PerformanceAnalytics)

getSymbols(allFunds, auto.assign=TRUE)

           
for(i in 1:length(allFunds))
{
  vname <- allFunds[i]
  if(substr(vname,1,1)=='^')
  {
    vname <- substr(vname,2,nchar(vname))
  }
  adjName <- paste(vname, "$",vname,".Adjusted",sep="")
  price <- eval(parse(text=adjName))
  if(i == 1)
  {
    allPrices <- price
  }
  if(i > 1)
  {
    allPrices <- merge(allPrices,price)
  }
}
names(allPrices) <- allFunds
calcRet <- function(pr)
{
  dailyReturn(pr,leading=FALSE)
}

for(i in 1:length(allFunds))
{
  if(i == 1)
  {
    allReturns <- calcRet(allPrices[,i])
  }
  if(i > 1)
  {
    allReturns <- merge(allReturns,calcRet(allPrices[,i]))
  }
}
names(allReturns) <- allFunds

MDD <- t(maxDrawdown(allReturns))
ExpShortfall <- t(ES(allReturns,p=0.95,method="modified",portfolio_method="single"))
annReturn <- t(Return.annualized(allReturns,scale=252))
stdReturn <- t(StdDev.annualized(allReturns,scale=252))

# getSymbols("^GSPC")
# SP <- GSPC$GSPC.Adjusted
# SP.daily <- calcRet(SP)
# SP.annualized <- t(Return.annualized(SP.daily,scale=252))
# SP.sd <- t(StdDev.annualized(SP.daily,scale=252))
# SP.mdd <- t(maxDrawdown(SP.daily))
# SP.ES <- t(ES(SP.daily,p=0.95,method="modified",portfolio_method="single"))
# Excess_SP <- t(Return.annualized.excess(allReturns,SP.daily,scale=252))
# IR_SP <- t(InformationRatio(allReturns,SP.daily,scale=252))
# ActivePrem_SP <- t(ActiveReturn(allReturns,SP.daily,scale=252))
# 
# getSymbols("^RUA")
# R3000 <- RUA$RUA.Adjusted
# R3000.daily <- calcRet(R3000)
# R3000.annualized <- t(Return.annualized(R3000.daily,scale=252))
# R3000.sd <- t(StdDev.annualized(R3000.daily,scale=252))
# R3000.mdd <- t(maxDrawdown(R3000.daily))
# R3000.ES <- t(ES(R3000.daily,p=0.95,method="modified",portfolio_method="single"))
# Excess_R3000 <- t(Return.annualized.excess(allReturns,R3000.daily,scale=252))
# IR_R3000 <- t(InformationRatio(allReturns,R3000.daily,scale=252))
# ActivePrem_R3000 <- t(ActiveReturn(allReturns,R3000.daily,scale=252))

getSymbols("^BXM")
BXM <- BXM$BXM.Adjusted
BXM.daily <- calcRet(BXM)
BXM.annualized <- t(Return.annualized(BXM.daily,scale=252))
BXM.sd <- t(StdDev.annualized(BXM.daily,scale=252))
BXM.mdd <- t(maxDrawdown(BXM.daily))
BXM.ES <- t(ES(BXM.daily,p=0.95,method="modified",portfolio_method="single"))

Excess_BXM <- t(Return.annualized.excess(allReturns,BXM.daily,scale=252))
IR_BXM <- t(InformationRatio(allReturns,BXM.daily,scale=252))
ActivePrem_BXM <- t(ActiveReturn(allReturns,BXM.daily,scale=252))

summaryTable <- cbind(funds,annReturn,stdReturn,MDD,ExpShortfall,Excess_BXM,IR_BXM,ActivePrem_BXM)
View(summaryTable)

CConly <- subset(summaryTable,Strategy=="Covered Call")
coln <- which(summaryTable$Strategy == "Covered Call")
chart.RiskReturnScatter(CConly[,9:10],Rf=0.00025,method="nocalc",add.boxplots=TRUE,add.sharpe=c(1,2))
CC.ret <- allReturns[,coln]
chart.RelativePerformance(CC.ret,BXM.daily,colorset=(1:23),main="Performance Relative to the BuyWrite Index (BXM)")
chart.RelativePerformance(allReturns,BXM.daily)

# plot all funds risk return
# ggplot(as.data.frame(CConly)) +
#   scale_color_manual(values = c("red", "green", "black","blue")) +     # need to tweak colors
#   geom_point(aes(x=CConly[,10],y=CConly[,9],colour=factor(Underlying),size=factor(Net)))+       #want size to be by NAV
#   geom_text(aes(x=CConly[,10],y=CConly[,9],label=CConly[,2],colour=factor(Equity.Holdings)),size=4,hjust=-0.1,vjust=0) +
#   labs(title = "Return vs Risk - All Funds") +
#   ylab("Annualized Return") +
#   xlab("Annualized Standard Deviation") 

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
