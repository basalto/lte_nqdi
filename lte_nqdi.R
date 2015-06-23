library(ggplot2)
library(dplyr)

setwd("/home/rjdinis/development/R/lte_nqdi")
source("which_nonnum.R")

#all_content = readLines("file.csv")
#skip_second = all_content[-2]


lte_meas_report <- read.table("le_LTEMeasurementReport_final.csv", header=TRUE, fill=TRUE, sep=',', dec=",", stringsAsFactors=FALSE, quote="\"", na.strings="NULL")
#lte_meas_report <- read.table("le_LTEMeasurementReport.txt", nrows=1000, dec=",", header=TRUE, fill=TRUE, comment.char = "", stringsAsFactors=FALSE, row.names=NULL)
#lte_meas_report <- read.table("le_LTEMeasurementReport.txt", header=TRUE, fill=TRUE, comment.char = "", dec=",", na.strings="NULL", stringsAsFactors=FALSE, row.names=NULL)
remove(lte_meas_report)

# Remove duplicate lines from data frame
lte_meas_report <- unique(lte_meas_report)

# Format fields
#lte_meas_report$RSRP <- sub(",",".",lte_meas_report$RSRP)
#lte_meas_report$RSRP <- as.numeric(lte_meas_report$RSRP)
#lte_meas_report$RSRQ <- sub(",",".",lte_meas_report$RSRQ)
#lte_meas_report$RSRQ <- as.numeric(lte_meas_report$RSRQ)
#lte_meas_report$RSSI <- sub(",",".",lte_meas_report$RSSI)
#lte_meas_report$RSSI <- as.numeric(lte_meas_report$RSSI)
#lte_meas_report$SINR0 <- sub(",",".",lte_meas_report$SINR0)
#lte_meas_report$SINR0 <- as.numeric(lte_meas_report$SINR0)

which.nonnum (lte_meas_report$BandIndicator)

table(list(lte_meas_report$Cidade))
table(list(lte_meas_report$Rede, lte_meas_report$BandIndicator))
table(list(lte_meas_report$Rede, lte_meas_report$DL_EARFCN))

lte_meas_report$SessionId <- factor(lte_meas_report$SessionId)
lte_meas_report$TestId <- factor(lte_meas_report$TestId)
lte_meas_report$Cidade <- factor(lte_meas_report$Cidade)
lte_meas_report$Rede <- factor(lte_meas_report$Rede)
lte_meas_report$CellIdentity <- factor(lte_meas_report$CellIdentity)
lte_meas_report$PhyCellId <- factor(lte_meas_report$PhyCellId)
lte_meas_report$direction <- factor(lte_meas_report$direction)
lte_meas_report$DL_EARFCN <- factor(lte_meas_report$DL_EARFCN)
lte_meas_report$BandIndicator <- factor(lte_meas_report$BandIndicator)
lte_meas_report$MsgTime <- as.POSIXct(lte_meas_report$MsgTime)

lte_meas_report$DLBandWidth <- factor(lte_meas_report$DLBandWidth)
levels(lte_meas_report$BandIndicator)[levels(lte_meas_report$BandIndicator)=="3"]  <- "3 (1800 MHz)"
levels(lte_meas_report$BandIndicator)[levels(lte_meas_report$BandIndicator)=="7"]  <- "7 (2600 MHz)"
levels(lte_meas_report$BandIndicator)[levels(lte_meas_report$BandIndicator)=="20"] <- "20 (800 MHz)"
#"1700" %in% c("1275","1300","1500","1700")

ggplot(lte_meas_report, aes(x=Cidade)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")

ggplot(lte_meas_report, aes(x=RSRP)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
h2 <- ggplot(lte_meas_report, aes(x=RSRQ)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
h3 <- ggplot(lte_meas_report, aes(x=SINR0)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fidll_gradient("Count", low = "green", high = "red")
#h4 <- ggplot(lte_meas_report, aes(x=CQI)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
multiplot(h1, h2, h3, cols=3)

# Scatter charts of RF Signal
ggplot(lte_meas_report, aes(x=RSRP, y=SINR0)) + 
  geom_point(size=0.9) + xlab("RSRP (dBm)") + 
  ylab("SINR (dB)") + ggtitle("RSRP vs SINR") + 
  facet_wrap(~ Rede, ncol=3)

ggplot(subset(lte_meas_report, (BandIndicator != "NA")), aes(x=RSRP, y=SINR0)) + 
  geom_point(size=0.9, colour="red") + 
  xlab("RSRP (dBm)") + ylab("SINR (dB)") + 
  facet_grid(Rede ~ DL_EARFCN)

ggplot(subset(lte_meas_report, (BandIndicator != "NA")), aes(x=RSRP, y=SINR0)) + 
  geom_point(size=0.9, colour="red") + 
  xlab("RSRP (dBm)") + ylab("SINR (dB)") + 
  facet_grid(Rede ~ BandIndicator)

ggplot(subset(lte_meas_report, (BandIndicator != "NA")), aes(x=RSRP, y=SINR0)) + 
  geom_point(size=0.9, colour="red") + 
  xlab("RSRP (dBm)") + ylab("SINR (dB)") + 
  facet_grid(Rede ~ BandIndicator + DL_EARFCN)

ggplot(subset(lte_meas_report, (BandIndicator != "NA")), aes(x=RSRP, y=SINR0)) + 
  geom_point(size=0.9, aes(colour=Rede)) + 
  xlab("RSRP (dBm)") + ylab("SINR (dB)") + 
  facet_grid(. ~ BandIndicator)

ggplot(subset(lte_meas_report, (BandIndicator != "NA")), aes(x=RSRP, y=RSRQ)) + 
  geom_point(size=0.9, aes(colour=Rede)) + 
  xlab("RSRP (dBm)") + ylab("RSRQ (dB)") + 
  facet_grid(. ~ BandIndicator)

p1 <- ggplot(subset(lte_meas_report, (Rede == "93")), aes(x=RSRP, y=SINR0)) + geom_point() + xlab("RSRP (dBm)") + ylab("SINR (dB)") + ggtitle("RSRP vs SINR") + facet_wrap(~ Rede, ncol=3)
p2 <- ggplot(subset(lte_meas_report, (Rede == "93")), aes(x=RSRP, y=SINR0)) + geom_point() + xlab("RSRP (dBm)") + ylab("SINR (dB)") + ggtitle("RSRP vs SINR") + facet_grid(BandIndicator ~ DL_EARFCN)
p3 <- ggplot(subset(lte_meas_report, (Rede == "93")), aes(x=RSRP, y=RSRQ)) + geom_point() + xlab("RSRP (dBm)") + ylab("RSRQ (dB)") + ggtitle("RSRP vs RSRQ") + facet_wrap(direction ~ DL_EARFCN, ncol=2)


hsdpa_ie = select(hsdpa, FTP, RNC, CQI, Codes, DTX=DTXRate, Throughput)
hsdpa_by_ftp <- group_by(hsdpa_ie, FTP)
thp_by_ftp = summarize(hsdpa_by_ftp, Codes = round(mean(Codes, na.rm = TRUE)), CQI = mean(CQI, na.rm = TRUE), DTX = mean(DTX, na.rm = TRUE), Throughput = mean(Throughput, na.rm = TRUE))

p1 <- ggplot(hsdpa, aes(x = Codes, y = DTXRate, color = Throughput)) + geom_point() + xlab("num HS-SCCH Codes") + ylab("DTX Rate %") + ggtitle("HSDPA Throughput")
p2 <- ggplot(thp_by_ftp, aes(CQI, Throughput)) + geom_point(aes(size = DTX, color = Codes), alpha = 1/2) + geom_smooth() + scale_size_area() + scale_colour_gradientn(colours=rainbow(4))
p3 <- ggplot(hsdpa, aes(x=CQI)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")
p4 <- ggplot(hsdpa_ie, aes(x=Codes, y=Throughput)) + geom_point() + geom_density2d()
p5 <- ggplot(hsdpa_ie, aes(x=Codes, y=Throughput)) + stat_density2d(aes(fill = ..level..), geom="polygon")
p6 <- qplot(hsdpa$Throughput, stat = "ecdf", geom = "step", xlab="HSDPA Throughput (Kbps)")



multiplot(p1, p2, p3, p4, p5, p6, cols=2)

#ggplot(hsdpa_ie, aes(x=Codes, y=Throughput)) + geom_point() + geom_density2d() + facet_wrap(~ RNC, ncol=3)