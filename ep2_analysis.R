library(readr)
library(ggpubr)
library(ggsci)
library(ggplot2)
library(ggsignif)
library(car)
library(cowplot)
library(forecast)
library(psych)
library(rstatix)

df_1 <- read_csv('/Users/leopoldlee/Desktop/R/certification_assay/plot/ep02_review/all_stat_group.csv')
df_2 <- read_csv('/Users/leopoldlee/Desktop/R/certification_assay/plot/ep02_review/all_seed_stat_long.csv')
ls_before <- read_csv('/Users/leopoldlee/Desktop/R/certification_assay/plot/ep02_review/g2_stat.csv')

DL_csv <- dplyr::filter(df_1,group == "DL")
summary(DL_csv$weight)
summary(DL_csv$length)
summary(DL_csv$width)
ls_csv <- dplyr::filter(df_1,group=="LS")
ls_csv$weight <- (1-ls_csv$weight^index_lamda)/index_lamda
summary(ls_csv$weight)
summary(ls_csv$length)
summary(ls_csv$width)
all_stat <- rbind(ls_before,DL_csv)

# 显著异常值检测
par(mfrow=c(1,6))
qqnorm(DL_csv$weight,ylab="DL weight(g)", main = "A")
qqline(DL_csv$weight)
qqnorm(DL_csv$length,ylab= "DL length(cm)", main = "B")
qqline(DL_csv$length)
qqnorm(DL_csv$width,ylab="DL width(cm)", main = "C")
qqline(DL_csv$width)
qqnorm(ls_csv$weight,ylab="LS weight(g)",main="D")
qqline(ls_csv$weight)
qqnorm(ls_csv$length,ylab="LS length(cm)", main = "E")
qqline(ls_csv$length)
qqnorm(ls_csv$width,ylab = "LS width(cm)", main = "F")
qqline(ls_csv$width)

# 第二种绘图方法
p1 <- ggqqplot(DL_csv$weight,size=0.6,ylab = "DL weight(g)")+
  theme(text = element_text(face = "bold"),axis.title.y = element_text(size = 17))
p2 <- ggqqplot(DL_csv$length,size=0.6,ylab = "DL height(cm)")+
  theme(text = element_text(face = "bold"),axis.title.y = element_text(size = 17))
p3 <- ggqqplot(DL_csv$width,size=0.6,ylab = "DL width(cm)")+
  theme(text = element_text(face = "bold"),axis.title.y = element_text(size = 17))
p4 <- ggqqplot(ls_before$weight,size=0.6,ylab = "LS weight(g)")+
  theme(text = element_text(face = "bold"),axis.title.y = element_text(size = 17))
p5 <- ggqqplot(ls_before$length,size=0.6,ylab = "LS height(cm)")+
  theme(text = element_text(face = "bold"),axis.title.y = element_text(size = 17))
p6 <- ggqqplot(ls_before$width,size=0.6,ylab = "LS width(cm)")+
  theme(text = element_text(face = "bold"),axis.title.y = element_text(size = 14))
plot_qq <- plot_grid(p1, p2, p3, p4,p5,p6,ncol=3,nrow = 2, labels=LETTERS[1:6],label_size=18)
# test
plot_grid(p1, p2, p3, p4,p5,p6,ncol=3,nrow = 2, labels=LETTERS[1:6])
ggsave("/Users/leopoldlee/Desktop/R/certification_assay/plot/ep02_review/qqplot.pdf",plot = plot_qq,width = 8,height = 6)

pp1 <- ggqqplot(all_stat,x='weight',color = 'group',palette = c("#662F00", "#D8AF97"),size=0.6,ylab = "weight(g)")+
  theme(text = element_text(face = "bold"),axis.title.y = element_text(size = 20),legend.text = element_text(size = 17),
        legend.title = element_text(size = 19))

pp2<-ggqqplot(all_stat,x='length',color = 'group',palette = c("#662F00", "#D8AF97"),size=0.6,ylab = "length(cm)")+
  theme(text = element_text(face = "bold"),axis.title.y = element_text(size = 20),legend.text = element_text(size = 17),
        legend.title = element_text(size = 19))
pp3<-ggqqplot(all_stat,x='width',color = 'group',palette = c("#662F00", "#D8AF97"),size=0.6,ylab = "width(cm)")+
  theme(text = element_text(face = "bold"),axis.title.y = element_text(size = 20),legend.text = element_text(size = 17),
        legend.title = element_text(size = 19))
pp4<-plot_grid(pp1, pp2, pp3,ncol=3, labels=LETTERS[1:3],label_size = 26)
ggsave("/Users/leopoldlee/Desktop/R/certification_assay/plot/ep02_review/qqplot2.pdf",plot = pp4,width = 21,height = 6)
# 进行shapiro-test
# DL
tapply(DL_csv$weight, DL_csv$group, shapiro.test)
tapply(DL_csv$length, DL_csv$group, shapiro.test)
tapply(DL_csv$width, DL_csv$group, shapiro.test)
# LS
tapply(ls_before$weight, ls_before$group, shapiro.test)
tapply(ls_before$length, ls_before$group, shapiro.test)
tapply(ls_before$width, ls_before$group, shapiro.test)

# 方差齐性检验
var.test(weight~group,data = all_stat)
var.test(length~group,data = all_stat)
var.test(width~group,data=all_stat)

# 不满足，选择walch T检验
t.test(weight~group,data = all_stat,alternative = 't',var.equal=F)
welch_anova_test(data = all_stat,weight~group)
welch_anova_test(data = all_stat,length~group)
welch_anova_test(data = all_stat,width~group)

# 单因素方差分析
all_stat$group <- factor(all_stat$group,levels = c("DL","LS"))
# 自用violin绘图模版
v1<-ggviolin(all_stat,x='group',y='weight',fill = 'group',alpha = 0.9,width = 0.5,
         legend = "none",
         ylab="Seed weight",xlab=FALSE,font.y = c(16,"bold","black"),font.tickslab = c(15,"bold","black"),
         add = "boxplot",add.params = list(fill="white",width=0.1,linetype=1),
         palette = c("#662F00", "#D8AF97"))+
  stat_compare_means(method = 'anova',method.args = list(alternative="t",var.equal=F),
                     label = "p.signif",
                     label.x.npc ="left",label.y = 0.13,label.x = 1.4,size=10)

v2<- ggviolin(all_stat,x='group',y='length',fill = 'group',alpha = 0.9,width = 0.5,
         legend = "none",
         ylab="Seed length",xlab=FALSE,font.y = c(16,"bold","black"),font.tickslab = c(15,"bold","black"),
         add = "boxplot",add.params = list(fill="white",width=0.1,linetype=1),
         palette = c("#662F00", "#D8AF97"))+
  stat_compare_means(method = 'anova',method.args = list(alternative="t",var.equal=F),
                     label = "p.signif",
                     label.x.npc ="left",label.y = 1.5,label.x = 1.43,size=10)

v3<-ggviolin(all_stat,x='group',y='width',fill = 'group',alpha = 0.9,width = 0.5,
         legend = "none",
         ylab="Seed width",xlab=FALSE,font.y = c(16,"bold","black"),font.tickslab = c(15,"bold","black"),
         add = "boxplot",add.params = list(fill="white",width=0.1,linetype=1),
         palette = c("#662F00", "#D8AF97"))+
  stat_compare_means(method = 'anova',method.args = list(alternative="t",var.equal=F),
                     label = "p.signif",
                     label.x.npc ="left",label.y = 0.7,label.x = 1.44,size=10)
vv4<-plot_grid(v1, v2, v3,ncol=3, labels=LETTERS[1:3],label_size = 17)
ggsave("/Users/leopoldlee/Desktop/R/certification_assay/plot/ep02_review/violin.pdf",plot = vv4,width = 8,height = 3)

# 相关性分析
library(corrplot)
related_csv <- read_csv("/Users/leopoldlee/Desktop/R/certification_assay/plot/ep02_review/survial_rate.csv")
attach(related_csv)
cor.test(as.numeric(as.factor(group)),germinatingRate,method = 'spearman')
# testdata
pdf("/Users/leopoldlee/Desktop/R/certification_assay/plot/ep02_review/correlation.pdf",width = 8,height = 8)
corrplot(corr = cor(related_csv),order = 'hclust',type='upper',tl.pos = 'd',method = 'number',number.cex = 1.3,tl.col="black",tl.cex=1,number.digits = 3)
corrplot(corr = cor(related_csv),order = 'hclust',type='lower',add = TRUE,method = 'pie',diag = FALSE, tl.pos = "n",cl.pos = "n")
dev.off()

corrplot.mixed(corr = cor(related_csv),order = 'hclust',lower = 'pie',upper = 'number',tl.pos = c("d"))
corrplot(corr = cor(related_csv[1:4]),
         method = "circle",
         order = 'hclust',
         type = "lower",
         tl.pos = "ld",
         addCoef.col = "black",
         number.cex = 1.3,
         number.digits = 3,
         diag = FALSE,
         outline = 'black')
library(GGally)
ggcorr(
  related_csv,
  name = expression(rho),
  geom = 'circle',
  max_size = 15,
  min_size = 7,
  size = 3,
  hjust = 0.75,
  nbreaks = 3,
  angle = -45,
  palette = "PuOr"
)

