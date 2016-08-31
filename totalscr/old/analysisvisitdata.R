library(dplyr)

## ---------------------------------- ## report 2
str(report2)
# Get unique elements in each event Category var
prod2.evCat = unique(report2$eventCategory)
for (i in 1:length(evCat)) {
  eventName = evCat[i]
  nam <- paste("A", i, sep = "")
  assign(nam,   unique(filter(report2, eventCategory %in% prod2.evCat[i])$eventAction))
}  

unique(filter(report2, eventCategory== 'product page')$eventAction)

# Product Categoryy
colnames(prod)[1] <- 'product_id'
prod = data.frame(prod)
prod.product_category = data.frame(unique(prod$category))
report2.product_category = data.frame(A2)

# Form Tracking
report2.form_tracking = data.frame(A1)

## -------------------------------- ## report 1 (source_medium, device cateory)

str(report1)
prod2.evCat = unique(report2$eventCategory)
for (i in 1:length(evCat)) {
  nam <- paste("A", i, sep = "")
  assign(nam,   unique(filter(report2, eventCategory %in% prod2.evCat[i])$eventAction))
}  

colnames(prod)[1] <- 'product_id'
prod = data.frame(prod)
prod.product_category = data.frame(unique(prod$category))
report2.product_category = data.frame(A2)

## ----------------------------- ##
tmp.visit = filter(report2, eventCategory == 'add-to-basket')
tmp.visit$date_activity = as.numeric(as.Date(tmp.visit$date))
colnames(trans)
tmp.trans = filter(trans[,c(2,3,4,5,18)],customer_id ==770806 )
tmp.trans$date_activity = as.numeric(as.Date(tmp.trans$order_date))
colnames(prod)
tmp.product = prod[,c(1,2,10)]
tmp.merge = merge(x= tmp.trans,y = tmp.product, by ='product_id',all.x= T)
colnames(tmp.merge)
x = filter(tmp.merge, date_activity == 16341)
y = filter(tmp.visit, date_activity == 16341 & customer_id == 770806 )

### conclusion 
# report1 and report 2 = utterly useless if we want to create product view - user - item data
# only relevent items would be : report1 (source_medium & device_category)
