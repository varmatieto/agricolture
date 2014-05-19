#############

library (ggplot2)
library (plyr)
library(grid)

# define function to create multi-plot setup (nrow, ncol)

vp.setup <- function(x,y){
    # create a new layout with grid
    grid.newpage()
    # define viewports and assign it to grid layout
    pushViewport(viewport(layout = grid.layout(x,y)))
}
# define function to easily access layout (row, col)
vp.layout <- function(x,y){
    viewport(layout.pos.row=x, layout.pos.col=y)
}

##########################################################

vilprod<- read.table ("prod_village_plus.txt", header=T, 
                      dec=".")

str(vilprod)
head(vilprod)
colnames(vilprod)

#  1] "village"  "fam_vil" "famprod"   "type" "area" "quantity" 
#  7] "unity"     "price"  "commerc"   "fammarket" "qta_ton"   "price_ton"
#  13] "price_ha" 

attach (vilprod)
fix (vilprod)

vilprod[vilprod$famprod=="Banana",]

levels(vilprod$type)<-c("new","old")

vilprod<-vilprod[order(vilprod$type),]

table(type,famprod)

####### DIVERSIFICATION 

p1<-ggplot(data=vilprod, aes(famprod, fill = famprod)) + 
    geom_bar(show_guide = F) + 
    ggtitle("frequency of different products") 

p2<-ggplot(data=vilprod, aes(village, fill = famprod)) + 
    geom_bar(show_guide = F) + 
    facet_grid(famprod ~ .) +
    ggtitle("frequency of products in villages") 


jpeg("diversification.jpg",width = 600, height = 900, units = "px")

vp.setup(2,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))
# print(p3, vp=vp.layout(3,1))
# print(p4, vp=vp.layout(4,1))

dev.off()


####### PRODUCTION 1


p1<-ggplot(data=vilprod, aes(x=village, y=area, fill = type)) + 
    geom_bar(stat="identity") + 
#    facet_grid(type ~ .) +
    ggtitle("areas per village per type") 

p2<-ggplot(data=vilprod, aes(x=village, y=qta_ton, fill = type)) + 
    geom_bar(stat="identity") + 
    #    facet_grid(type ~ .) +
    ggtitle("qta_ton per village per type") 

p3<-ggplot(data=vilprod, aes(x=village, y=area*price_ha/1000, fill = type)) + 
    geom_bar(stat="identity") + 
    ggtitle("tot_price_ton per village per type") +
    ylab("in MR$") 

p4<-ggplot(data=vilprod, aes(x=village, y=qta_ton*price_ton/1000, fill = type)) + 
    geom_bar(stat="identity") + 
    #    facet_grid(type ~ .) +
    ggtitle("tot_price_ha per village per type") +
    ylab("in MR$") 

jpeg("production.jpg",width = 600, height = 900, units = "px")

vp.setup(4,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))
print(p3, vp=vp.layout(3,1))
print(p4, vp=vp.layout(4,1))

dev.off()

####### PRODUCTION 2


p1<-ggplot(data=vilprod, aes(x=famprod, y=area, fill = type)) + 
    geom_bar(stat="identity") + 
    #    facet_grid(type ~ .) +
    ggtitle("areas per product per type") 

p2<-ggplot(data=vilprod, aes(x=famprod, y=qta_ton, fill = type)) + 
    geom_bar(stat="identity") + 
    #    facet_grid(type ~ .) +
    ggtitle("qta_ton per product per type") 

p3<-ggplot(data=vilprod, aes(x=famprod, y=area, fill = type)) + 
    geom_bar(stat="identity") + 
    facet_grid(fam_vil ~ .) +
    ggtitle("areas per product per type") 

p4<-ggplot(data=vilprod, aes(x=famprod, y=qta_ton, fill = type)) + 
    geom_bar(stat="identity") + 
    facet_grid(fam_vil ~ .) +
    ggtitle("qta_ton per product per type") 


jpeg("production_2.jpg",width = 600, height = 900, units = "px")

vp.setup(4,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))
print(p3, vp=vp.layout(3,1))
print(p4, vp=vp.layout(4,1))

dev.off()
################ PRICES ########################################


p1<-ggplot(data=vilprod, aes(x=famprod, y=price_ton, fill = type)) + 
    geom_boxplot() + 
    ggtitle("distrib price per ton per product") 

p2<-ggplot(data=vilprod, aes(x=famprod, y=price_ton, fill = fam_vil)) + 
    geom_boxplot() + 
    ggtitle("distrib price per ton per product") 

p3<-ggplot(data=vilprod, aes(x=famprod, y=price_ha, fill = type)) + 
    geom_boxplot() + 
    ggtitle("distrib price per ha per product") 

p4<-ggplot(data=vilprod, aes(x=famprod, y=price_ha, fill = fam_vil)) + 
    geom_boxplot() + 
    ggtitle("distrib price per ha per product") 

jpeg("prices.jpg",width = 600, height = 900, units = "px")

vp.setup(4,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))
print(p3, vp=vp.layout(3,1))
print(p4, vp=vp.layout(4,1))

dev.off()

################################
levels(fammarket)

p1<-ggplot(data=vilprod, aes(x=fammarket, y=commerc, fill = type)) + 
    geom_bar(stat="identity") +
    ggtitle("means of commercialization") 

p2<-ggplot(data=vilprod, aes(x=famprod, y=commerc, fill = fammarket )) + 
    geom_bar(stat="identity") +
    ggtitle("means of commercialization") 
#    geom_text(aes(label=fammarket), size=3) 

jpeg("commerz.jpg",width = 600, height = 900, units = "px")

vp.setup(2,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))


dev.off()

vilprodx<-vilprod[!vilprod$famprod=="Acerola",]

ggplot(data=vilprodx, aes(x=fammarket, y=commerc, fill = fammarket)) + 
    geom_bar(stat="identity", show_guide = F) +
    facet_wrap( ~ famprod, ncol=2) +
    ggtitle("means of commercialization") +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))

ggsave(file="commerz_2.jpg", dpi=72)


ggplot(data=vilprod, aes(x=famprod, y=commerc, fill = famprod)) + 
    geom_bar(stat="identity", show_guide = F) +
    facet_wrap( ~ fammarket, ncol=2) +
    ggtitle("means of commercialization") +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))


ggsave(file="commerz_3.jpg", dpi=144)


