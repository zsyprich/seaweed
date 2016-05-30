#赵树阳 2120151070
#读取数据
algae = read.table('C:/Users/yanli/Desktop/Analysis.txt'）
col.names=c('season','size','speed','mxPH','mnO2','Cl','NO3','NH4','oPO4','PO4','Chla',
	'a1','a2','a3','a4','a5','a6','a7'),
na.string=c('XXXXXXX'))

#数据摘要分析
summary(algae)

#数据可视化
library(car)

#绘制各属性直方图
hist(algae$mxPH)

#绘制各属性Q-Q图
qqPlot(algae$mxPH,main='Norm QQ Plot of mxPH')

#绘制各属性盒图
#ylab为设置y轴标题；
#rug函数绘制变量的实际值，side=4表示绘制在图的右侧（1在下方，2在左侧，3在上方）；
#abline函数绘制水平线，mean表示均值，na.rm=T指计算时不考虑NA值，lty=2设置线型为虚线。
boxplot(algae$mxPH,ylab='mxPH')
rug(algae$mxPH,side=4)
abline(h=mean(algae$mxPH,na.rm=T),lty=2)

hist(algae$mnO2)
qqPlot(algae$mnO2,main='Norm QQ Plot of mnO2')
boxplot(algae$mnO2,ylab='mnO2')
rug(algae$mnO2,side=4)
abline(h=mean(algae$mnO2,na.rm=T),lty=2)

hist(algae$Cl)
qqPlot(algae$Cl,main='Norm QQ Plot of Cl')
boxplot(algae$Cl,ylab='Cl')
rug(algae$Cl,side=4)
abline(h=mean(algae$Cl,na.rm=T),lty=2)

hist(algae$NO3)
qqPlot(algae$NO3,main='Norm QQ Plot of NO3')
boxplot(algae$NO3,ylab='NO3')
rug(algae$NO3,side=4)
abline(h=mean(algae$NO3,na.rm=T),lty=2)

hist(algae$NH4)
qqPlot(algae$NH4,main='Norm QQ Plot of NH4')
boxplot(algae$NH4,ylab='NH4')
rug(algae$NH4,side=4)
abline(h=mean(algae$NH4,na.rm=T),lty=2)

hist(algae$oPO4)
qqPlot(algae$oPO4,main='Norm QQ Plot of oPO4')
boxplot(algae$oPO4,ylab='oPO4')
rug(algae$oPO4,side=4)
abline(h=mean(algae$oPO4,na.rm=T),lty=2)

hist(algae$PO4)
qqPlot(algae$PO4,main='Norm QQ Plot of PO4')
boxplot(algae$PO4,ylab='PO4')
rug(algae$PO4,side=4)
abline(h=mean(algae$PO4,na.rm=T),lty=2)

hist(algae$Chla)
qqPlot(algae$Chla,main='Norm QQ Plot of Chla')
boxplot(algae$Chla,ylab='Chla')
rug(algae$Chla,side=4)
abline(h=mean(algae$Chla,na.rm=T),lty=2)

#绘制条件盒图
library(lattice)
bwplot(size~a1,data=algae,ylab='River Size',xlab='a1')

bwplot(size~a2,data=algae,ylab='River Size',xlab='a2')
bwplot(size~a3,data=algae,ylab='River Size',xlab='a3')
bwplot(size~a4,data=algae,ylab='River Size',xlab='a4')
bwplot(size~a5,data=algae,ylab='River Size',xlab='a5')
bwplot(size~a6,data=algae,ylab='River Size',xlab='a6')
bwplot(size~a7,data=algae,ylab='River Size',xlab='a7')

#缺失数据处理

#直接剔除缺失数据
omitdata = na.omit(algae)
write.table(omitdata,'C:/Users/yanli/Desktop/omitedDa.txt',
	col.names = F,row.names = F, quote = F)

#使用最高频率数据替换
library(DMwR)
preprocess2 = algae[-manyNAs(algae),]
preprocess2 = centralImputation(preprocess2)
write.table(preprocess2,'C:/Users/yanli/Desktop/mostDa.txt'',
col.names = F,row.names = F, quote = F)

#通过变量相关性填补缺失值
symnum(cor(algae[,4:18],use='complete.obs'))
lm(formula=PO4~oPO4, data=algae)
preprocess3 = algae[-manyNAs(algae),]
fillPO4 <- function(oP){
	if(is.na(oP))
		return(NA)
	else return (42.897 + 1.293 * oP)
}
preprocess3[is.na(preprocess3$PO4),'PO4'] <- sapply(preprocess3[is.na(preprocess3$PO4),'oPO4'],fillPO4)
write.table(preprocess3,'C:/Users/yanli/Desktop/correlationDa.txt',
col.names = F,row.names = F, quote = F)

#通过数据对象之间的相似型来填补缺失值
preprocess4 = knnImputation(algae,k=10)
write.table(preprocess4,'C:/Users/yanli/similarDa.txt',
col.names = F,row.names = F, quote = F)
 