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

summary(vilprod$price_ha)

levels(vilprod$village)<-LETTERS[1:11]
levels(vilprod$type)<-c("new","old")

vilprod<- vilprod[order(type),]
nvillage<-length(levels(village))
nfamprod<-length(levels(famprod))
nunity<-length(levels(unity))

table(type,famprod)

# "aguafria"      "bomjesus"      "cousta_do" "itabaiana"    
# "javari"        "junco"         "lemos"         "mangibura"    
# "massnangana"   "melos"         "samba"        

####### DIVERSIFICATION 

p1<-ggplot(data=vilprod, aes(famprod, fill = village)) + 
    geom_bar() + 
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


####### PRODUCTION


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
    ggtitle("tot_price per village per type") +
    ylab("in MR$") 

p4<-ggplot(data=vilprod, aes(x=village, y=qta_ton*price_ton/1000, fill = type)) + 
    geom_bar(stat="identity") + 
    #    facet_grid(type ~ .) +
    ggtitle("tot_price per village per type") +
    ylab("in MR$") 

jpeg("production.jpg",width = 600, height = 900, units = "px")

vp.setup(4,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))
print(p3, vp=vp.layout(3,1))
print(p4, vp=vp.layout(4,1))

dev.off()

################ PRICES ########################################


ggplot(data=vilprod, aes(x=famprod, y=price_ton, fill = type)) + 
    geom_boxplot() + 
    ggtitle("distrib price per ton per product") 

ggplot(data=vilprod, aes(x=famprod, y=price_ton, fill = fam_vil)) + 
    geom_boxplot() + 
    ggtitle("distrib price per ton per product") 

ggplot(data=vilprod, aes(x=famprod, y=price_ha, fill = type)) + 
    geom_boxplot() + 
    ggtitle("distrib price per ha per product") 

ggplot(data=vilprod, aes(x=famprod, y=price_ha, fill = fam_vil)) + 
    geom_boxplot() + 
    ggtitle("distrib price per ha per product") 








ggplot(data=vilprod, aes(x=village, y=area, fill = type)) + 
    geom_boxplot() + 
    facet_grid(type ~ .) +
    ggtitle("distrib areas per village") 

ggplot(data=vilprod, aes(x=village, fill = type)) + 
    geom_bar(stat="bin") + 
    facet_grid(type ~ .) +
    ggtitle("n.prod per village") 


ggplot(data=vilprod, aes(x=famprod, y=area, fill = type)) + 
    geom_bar(stat="identity") + 
#    facet_wrap( ~ village, ncol=2) +
    ggtitle("n.prod per village") 



# n. prodotti per villaggio == diversificazione
barplot(table(village))
barplot(table(village)/nprod)

# n. prodotti per villaggio con indicazione della famiglia di prodotti
barplot(table(famprod,village))

# area coltivata per villaggio con famiglia di prodotti

area_by_village<-ddply(vilprod,.(village), summarize,tot=sum(area))

##BUONO!!!!
barplot(area_by_village$tot, col=rainbow(11),
        names.arg = area_by_village$village, cex.names=.6,
        main=c("area for village"))

# n. prodotti piu' diffusi

barplot(table(production)[table(production)>5])
barplot(table(production)[table(production)>5])
barplot(table(famprod)[table(famprod)>7])
barplot(table(production)[table(production)>5])



# file diviso a secondo dell'unita' di misura
vilprod_uni<-vilprod[unity=="uni",]
str(vilprod_uni)

vilprod_ton<-vilprod[unity=="ton",]
str(vilprod_ton)

vilprod_kg<-vilprod[unity=="kg",]
str(vilprod_kg)


barplot(table(vilprod_uni$village))
barplot(table(vilprod_ton$village))
barplot(table(vilprod_kg$village))

prod_ton<-factor(vilprod_ton$production)
barplot(table(prod_ton), cex.names=.5)
fa_prod_ton<-factor(vilprod_ton$famprod)
barplot(table(fa_prod_ton), cex.names=.5)

prod_kg<-factor(vilprod_kg$production)
barplot(table(prod_kg), cex.names=.5)
fa_prod_kg<-factor(vilprod_kg$famprod)
barplot(table(fa_prod_kg), cex.names=.5)

prod_uni<-factor(vilprod_uni$production)
barplot(table(prod_uni), cex.names=.5)
fa_prod_uni<-factor(vilprod_uni$famprod)
barplot(table(fa_prod_uni), cex.names=.5)

ton_by_village<-ddply(vilprod_ton,.(village), summarize,tot=sum(qty,na.rm = T))
barplot(ton_by_village$tot, col=rainbow(11),
        names.arg = area_by_village$village, cex.names=.6,
        main=c("ton by village"))

uni_by_village<-ddply(vilprod_uni,.(village), summarize,tot=sum(qty,na.rm = T))
barplot(uni_by_village$tot, col=rainbow(11),
        names.arg = area_by_village$village, cex.names=.6,
        main=c("uni by village"))

kg_by_village<-ddply(vilprod_kg,.(village), summarize,tot=sum(qty,na.rm = T))
barplot(kg_by_village$tot, col=rainbow(11),
        names.arg = area_by_village$village, cex.names=.6,
        main=c("kg by village"))


############################################
# file diviso per villaggio

attach (vilprod)
byvil<-split(vilprod,village)

co_vila<-c("bomjesus","cousta_do", "melos" )

vilprod_bj<-byvil[["bomjesus"]]
str(vilprod_bj)

attach (vilprod_bj)
head (vilprod_bj)
vilprod_bj_na<-vilprod_bj[!is.na  (qty),]

attach (vilprod_bj_na)
famprod_bj<-factor (famprod)
name_famprod_bj<-levels(famprod_bj)

prod_bj<-ddply(vilprod_bj_na,.(famprod), summarize,tot=sum(qty,na.rm = T))
#barplot(
barplot(prod_bj$tot, col=rainbow(19),
        names.arg = prod_bj$famprod, cex.names=.6,
        main=c("bomjesus "))

barplot(qty, area, col=rainbow(19),
        names.arg = production, cex.names=.6,
        main=c("bomjesus "))

plot(qty,area, names.arg = production, cex.names=.6, angle = 45)

############################################
# file diviso per famiglia di prodotto
attach (vilprod)
byfamprod<-split(vilprod,famprod)

str(byfamprod)

# BANANA ####################

famprod_banana<-byfamprod[["Banana"]]

str(famprod_banana)
attach (famprod_banana)
# fix(famprod_banana)
banana_by_village<-ddply(famprod_banana,.(village), summarize,tot=sum(qty,na.rm = T))

barplot(banana_by_village$tot, col=rainbow(19),
        names.arg = banana_by_village$village, cex.names=.6,
        main=c("banana "))

# ALL FAMILIES of bonjesus ####################

# l_famprod<-levels(famprod)
l_famprod<-name_famprod_bj
name_village<-levels(vilprod$village)

#"Abacaxi"     "Acerola"     "Banana"      "Batata_Doce"
# "Caju"        "Cana"        "Citros"      "Coco"       
# "Goiaba"      "Graviola"    "Inhame"      "Jaca"       
# "Macax"       "Mamão"       "Mandioca"    "Manga"      
# "Maracujá"   


pdf("bj_products.pdf", paper=c("a4")) 
par(mfrow=c(4,1)) 

for (i in l_famprod)
{
 famprod_i<-byfamprod[[i]]


attach (famprod_i)

i_by_village<-ddply(famprod_i,.(village), summarize,tot=sum(qty,na.rm = T))

barplot(i_by_village$tot, col=rainbow(11),
        names.arg = i_by_village$village, cex.names=.5,
        main=c(i))
}
dev.off()

mtx_prod_bj<-matrix(0,11,length(l_famprod))
colnames(mtx_prod_bj)<-l_famprod
rownames(mtx_prod_bj)<-name_village
mtx_prod_bj
mtx_prod_bj["aguafria","Abacaxi" ]

for (i in l_famprod)
{
  famprod_i<-byfamprod[[i]]
  
  
  attach (famprod_i)
  
  i_by_village<-ddply(famprod_i,.(village), summarize,tot=sum(qty,na.rm = T))
  
  
  for (j in name_village)
  {
    mtx_prod_bj[j, i]<-i_by_village$tot[j]
  }
}
mtx_prod_bj

# "aguafria"      "bomjesus"      "cousta_do" "itabaiana"    
# "javari"        "junco"         "lemos"         "mangibura"    
# "massnangana"   "melos"         "samba"        

# file solo con itre villaggi

attach (vilprod)


vilprod_tre<- vilprod[(village=="bomjesus"|village=="cousta_do"|village=="melos"), ]
vilprod_tre<- vilprod[village=="bomjesus", ]

attach (vilprod_tre)
byfamprod_tre<-split(vilprod_tre,famprod)

str(byfamprod_tre)

# BANANA ####################

famprod_tre_banana<-byfamprod_tre[["Banana"]]

str(famprod_tre_banana)
attach (famprod_tre_banana)
village3<-factor(famprod_tre_banana$village)

# fix(famprod_banana)
banana_by_village<-ddply(famprod_tre_banana,.(village3), summarize,tot=sum(qty,na.rm = T))

barplot(banana_by_village$tot, col=rainbow(19),
        names.arg = banana_by_village$village, cex.names=.6,
        main=c("banana "))


