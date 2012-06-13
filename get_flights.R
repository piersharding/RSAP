library(RSAP)
library(reshape)
library(plotrix)

conn <- RSAPConnect("sap.yml")
parms <- list('DELIMITER' = ';',
              'QUERY_TABLE' = 'SFLIGHTS2')
res <- RSAPInvoke(conn, "RFC_READ_TABLE", parms)
RSAPClose(conn)
str(res$ENTRIES)
str(res$DATA)

# get the sflight2 data frame
sflight = res$DATA

# split the text rows to make new columns - set the field names
flds <- sub("\\s+$", "", res$FIELDS$FIELDNAME)
sflight <- data.frame(sflight, colsplit(sflight$WA, split = ";", names = flds))

# add and reformat some columns
sflight <- cbind(sflight, FLIGHTNO = paste(sub("\\s+$", "", sflight$CARRID),sflight$CONNID, sep=""))
sflight$SEGMENT <- paste(sflight$AIRPFROM, sflight$AIRPTO, sep=" - ")
sflight$CARRNAME <- sub("\\s+$", "", sflight$CARRNAME)
sflight$DISTANCE <- as.numeric(lapply(sflight$DISTANCE, FUN=function (x) {sub("\\*","", x)}))
sflight$DISTANCE <- as.numeric(lapply(sflight$DISTANCE, FUN=function (x) {if (x == 0) NA else x}))
# fudge the distance from Singapore to Frankfurt
sflight[sflight$CARRNAME == 'Qantas Airways','DISTANCE'] <- 10258

# create some aggregated views
agg_sflight <- aggregate(data.frame(SEATSMAX=sflight$SEATSMAX, SEATSOCC=sflight$SEATSOCC,OCCUPANCY=sflight$SEATSOCC/sflight$SEATSMAX), by=list(flightno=sflight$FLIGHTNO), FUN=mean, na.rm=TRUE)
airline_avgocc <- aggregate(data.frame(SEATSMAX=sflight$SEATSMAX, SEATSOCC=sflight$SEATSOCC,OCCUPANCY=sflight$SEATSOCC/sflight$SEATSMAX), by=list(carrname=sflight$CARRNAME), FUN=mean, na.rm=TRUE)
airline_sumocc <- aggregate(data.frame(SEATSOCC=sflight$SEATSOCC), by=list(carrname=sflight$CARRNAME), FUN=sum, na.rm=TRUE)

# airline occupancy avg share
x11()
lbls <- paste(airline_avgocc$carrname, "\nOR: ", sprintf("%.2f%%", airline_avgocc$OCCUPANCY*100), "\nTP: ", sprintf("%.2f%%", (airline_avgocc$OCCUPANCY/sum(airline_avgocc$OCCUPANCY))*100), sep="")
pie3D(airline_avgocc$OCCUPANCY, labels=lbls, col=rainbow(length(airline_avgocc$carrname)), main="Occupancy avg share for Airlines\nOR = Occupancy Rate\nTP = Total Proportion")

# airline occupancy sum share
x11()
lbls <- paste(airline_sumocc$carrname, "\n", sprintf("%.2f%%", (airline_sumocc$SEATSOCC/sum(airline_sumocc$SEATSOCC))*100), sep="")
pie3D(airline_sumocc$SEATSOCC, labels=lbls, col=rainbow(length(airline_sumocc$carrname)), main="Occupancy sum share for Airlines", explode=0.1)

# Stacked Bar Plot with Colors and Legend

# generate summary and filling missing combinations
d <- aggregate(SEATSOCC ~ CARRNAME:SEGMENT, data=sflight, FUN=sum, na.rm=FALSE)
d2 <- with(d, expand.grid(CARRNAME = unique(d$CARRNAME), SEGMENT = unique(d$SEGMENT)))
airline_sumsegocc <- merge(d, d2, all.y = TRUE)
airline_sumsegocc$SEATSOCC[is.na(airline_sumsegocc$SEATSOCC)] <- 0

# switch orientation to carrier * segment
counts <- data.frame(unique(airline_sumsegocc$SEGMENT))
for (a in unique(airline_sumsegocc$CARRNAME))  {counts <- cbind(counts, airline_sumsegocc$SEATSOCC[which(airline_sumsegocc$CARRNAME == a)]);}
counts[,1] <- NULL
colnames(counts) <- unique(airline_sumsegocc$CARRNAME);
rownames(counts) <- unique(airline_sumsegocc$SEGMENT);
x11()
par(xpd=T, mar=par()$mar+c(0,0,0,10))
barplot(as.matrix(counts), main="Total Occupancy by Carrier and segment", ylab="Number of Seats", col=rainbow(dim(counts)[1]), ylim=c(0, 20000))
legend(10, 20000, legend = rownames(counts), col=rainbow(dim(counts)[1]), lty=1, seg.len=1)

# switch orientation to segment * carrier
counts <- data.frame(unique(airline_sumsegocc$CARRNAME))
for (a in unique(airline_sumsegocc$SEGMENT))  {counts <- cbind(counts, airline_sumsegocc$SEATSOCC[which(airline_sumsegocc$SEGMENT == a)]);}
counts[,1] <- NULL
colnames(counts) <- unique(airline_sumsegocc$SEGMENT);
rownames(counts) <- unique(airline_sumsegocc$CARRNAME);
x11()
barplot(as.matrix(counts), main="Total Occupancy by Segment and Carrier", ylab="Number of Seats", col=rainbow(dim(counts)[1]), ylim=c(0, 15000), legend = rownames(counts))

# simple performance measure - total receipts / (miles * passengers)
agg_perf <- aggregate(data.frame(PAYMENTSUM=sflight$PAYMENTSUM, SEATSOCC=sflight$SEATSOCC, DISTANCE=sflight$DISTANCE, PERFORMANCE=(sflight$PAYMENTSUM/(sflight$SEATSOCC * sflight$DISTANCE))), by=list(carrname=sflight$CARRNAME), FUN=sum, na.rm=TRUE)
# show airlines revenue performance descending
print(agg_perf[order(-agg_perf$PERFORMANCE),])

# performance by airline over time - dollars per customer KM
sflight$FLDATEYYMM <- substr(sflight$FLDATE, start=1, stop=6)
d <- aggregate(data.frame(PAYMENTSUM=sflight$PAYMENTSUM, SEATSOCC=sflight$SEATSOCC, DISTANCE=sflight$DISTANCE, PERFORMANCE=(sflight$PAYMENTSUM/(sflight$SEATSOCC * sflight$DISTANCE))), by=list(carrname=sflight$CARRNAME, fldateyymm=sflight$FLDATEYYMM), FUN=sum, na.rm=TRUE)
d2 <- with(d, expand.grid(carrname = unique(d$carrname), fldateyymm = unique(d$fldateyymm)))
agg_perf <- merge(d, d2, all.y = TRUE)
agg_perf <- agg_perf[order(agg_perf$carrname, agg_perf$fldateyymm),]
agg_perf$PERFORMANCE[is.na(agg_perf$PERFORMANCE)] <- 0

# create time series and plot comparison
perf_series <- data.frame(1:length(unique(agg_perf$fldateyymm)))
for (a in unique(agg_perf$carrname))  {perf_series <- cbind(perf_series, agg_perf$PERFORMANCE[which(agg_perf$carrname == a)]);}
perf_series[,1] <- NULL
colnames(perf_series) <- unique(agg_perf$carrname);
# convert all to time series
for (a in length(unique(agg_perf$carrname))) {perf_series[[a]] <- ts(perf_series[,a], start=c(2011,5), frequency=12)}
# plot the first and line the rest
x11()
ts.plot(ts(perf_series, start=c(2011,5), frequency=12), gpars=list(main="Performance: dollar per customer KM", xlab="Months", ylab="Dollars", col=rainbow(dim(perf_series)[2]), xy.labels=TRUE))
legend(2012.05, 3.2, legend=colnames(perf_series), col=rainbow(dim(perf_series)[2]), lty=1, seg.len=1)
