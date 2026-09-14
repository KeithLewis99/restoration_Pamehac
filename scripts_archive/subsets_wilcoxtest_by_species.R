
output_pamehac_by_station <- read.csv("~/SPERA Project 2015 and 16/Pamehac Brook/Pamehac Depletion Estimates/PamehacDepletion/output_pamehac_by_station.csv")

View(output_pamehac_by_station)

BT_1996<-subset(output_pamehac_by_station, output_pamehac_by_station$Species=="BT" & output_pamehac_by_station$Year=="1996") 

BT_1996

wilcox.test(BT_1996$stand.species.abundance.contr[BT_1996$type=="ABOVE"],BT_1996$stand.species.abundance.contr[BT_1996$type=="BELOW"])

###Wilcoxon rank sum test

###data:  BT_1996$stand.species.abundance.contr[BT_1996$type == "ABOVE"] and BT_1996$stand.species.abundance.contr[BT_1996$type == "BELOW"]
###W = 11, p-value = 0.3333
###alternative hypothesis: true location shift is not equal to 0


BT_1991<-subset(output_pamehac_by_station, output_pamehac_by_station$Species=="BT" & output_pamehac_by_station$Year=="1991") 

BT_1991

wilcox.test(BT_1991$stand.species.abundance.contr[BT_1991$type=="ABOVE"],BT_1991$stand.species.abundance.contr[BT_1991$type=="BELOW"])




BT_1990<-subset(output_pamehac_by_station, output_pamehac_by_station$Species=="BT" & output_pamehac_by_station$Year=="1990") 

BT_1990

wilcox.test(BT_1990$stand.species.abundance.contr[BT_1990$type=="ABOVE"],BT_1990$stand.species.abundance.contr[BT_1990$type=="BELOW"])


BT_1992<-subset(output_pamehac_by_station, output_pamehac_by_station$Species=="BT" & output_pamehac_by_station$Year=="1992") 

BT_1992

wilcox.test(BT_1992$stand.species.abundance.contr[BT_1992$type=="ABOVE"],BT_1992$stand.species.abundance.contr[BT_1992$type=="BELOW"])


BT_2016<-subset(output_pamehac_by_station, output_pamehac_by_station$Species=="BT" & output_pamehac_by_station$Year=="2016") 

BT_2016

wilcox.test(BT_2016$stand.species.abundance.contr[BT_2016$type=="ABOVE"],BT_2016$stand.species.abundance.contr[BT_2016$type=="BELOW"])


###Atlantic Salmon

AS_2016<-subset(output_pamehac_by_station, output_pamehac_by_station$Species=="AS" & output_pamehac_by_station$Year=="2016") 

AS_2016

wilcox.test(AS_2016$stand.species.abundance.contr[AS_2016$type=="ABOVE"],AS_2016$stand.species.abundance.contr[AS_2016$type=="BELOW"])



AS_1992<-subset(output_pamehac_by_station, output_pamehac_by_station$Species=="AS" & output_pamehac_by_station$Year=="1992") 

AS_1992

wilcox.test(AS_1992$stand.species.biomass.contr[AS_1992$type=="ABOVE"],AS_1992$stand.species.biomass.contr[AS_1992$type=="BELOW"])


AS_1996<-subset(output_pamehac_by_station, output_pamehac_by_station$Species=="AS" & output_pamehac_by_station$Year=="1996") 

AS_1996

wilcox.test(AS_1996$stand.species.abundance.contr[AS_1996$type=="ABOVE"],AS_1996$stand.species.abundance.contr[AS_1996$type=="BELOW"])


AS_1991<-subset(output_pamehac_by_station, output_pamehac_by_station$Species=="AS" & output_pamehac_by_station$Year=="1991") 

AS_1991

wilcox.test(AS_1991$stand.species.abundance.contr[AS_1991$type=="ABOVE"],AS_1991$stand.species.abundance.contr[AS_1991$type=="BELOW"])



AS_1990<-subset(output_pamehac_by_station, output_pamehac_by_station$Species=="AS" & output_pamehac_by_station$Year=="1990") 

AS_1990

wilcox.test(AS_1990$stand.species.abundance.contr[AS_1990$type=="ABOVE"],AS_1990$stand.species.abundance.contr[AS_1990$type=="BELOW"])

#######BROOK TROUT ABUNDANCE IN 2016 COMPARED TO EARLY YEARS OF MONITORING

> wilcox.test(BT_1991$stand.species.abundance.contr,BT_2016$stand.species.abundance.contr)

Wilcoxon rank sum test

data:  BT_1991$stand.species.abundance.contr and BT_2016$stand.species.abundance.contr
W = 21, p-value = 0.01667
alternative hypothesis: true location shift is not equal to 0

> wilcox.test(BT_1990$stand.species.abundance.contr,BT_2016$stand.species.abundance.contr)

Wilcoxon rank sum test

data:  BT_1990$stand.species.abundance.contr and BT_2016$stand.species.abundance.contr
W = 21, p-value = 0.01667
alternative hypothesis: true location shift is not equal to 0

> wilcox.test(BT_1992$stand.species.abundance.contr,BT_2016$stand.species.abundance.contr)

Wilcoxon rank sum test

data:  BT_1992$stand.species.abundance.contr and BT_2016$stand.species.abundance.contr
W = 27, p-value = 0.009091
alternative hypothesis: true location shift is not equal to 0

> wilcox.test(BT_1996$stand.species.abundance.contr,BT_2016$stand.species.abundance.contr)

Wilcoxon rank sum test

data:  BT_1996$stand.species.abundance.contr and BT_2016$stand.species.abundance.contr
W = 23, p-value = 0.1
alternative hypothesis: true location shift is not equal to 0

####All Salmonids Combined

BELOW_1990<-subset(output_pamehac_salmonids, output_pamehac_salmonids$Type=="BELOW" & output_pamehac_salmonids$Year=="1990") 

BELOW_1990

BELOW_1991<-subset(output_pamehac_salmonids, output_pamehac_salmonids$Type=="BELOW" & output_pamehac_salmonids$Year=="1991") 

BELOW_1991

BELOW_1992<-subset(output_pamehac_salmonids, output_pamehac_salmonids$Type=="BELOW" & output_pamehac_salmonids$Year=="1992") 

BELOW_1992

BELOW_1996<-subset(output_pamehac_salmonids, output_pamehac_salmonids$Type=="BELOW" & output_pamehac_salmonids$Year=="1996") 

BELOW_1996

BELOW_2016<-subset(output_pamehac_salmonids, output_pamehac_salmonids$Type=="BELOW" & output_pamehac_salmonids$Year=="2016") 

BELOW_2016




wilcox.test(BELOW_1990$stand.species.abundance.contr, BELOW_1991$stand.species.abundance.contr)

wilcox.test(BELOW_1991$stand.species.abundance.contr, BELOW_1992$stand.species.abundance.contr)


wilcox.test(BELOW_1992$stand.species.abundance.contr, BELOW_1996$stand.species.abundance.contr)

wilcox.test(BELOW_1996$stand.species.abundance.contr, BELOW_2016$stand.species.abundance.contr)



wilcox.test(output_pamehac_salmonids$stand.species.abundance.contr[output_pamehac_salmonids$Year=="1990"], output_pamehac_salmonids$stand.species.abundance.contr[output_pamehac_salmonids$Year=="1991"])


