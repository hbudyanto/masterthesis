########################################################
## ========== FEATURE ENGINEERING
########################################################

source('~/PycharmProjects/dissertation/src/v1/func_v1.R') #data cleansing

########################################################
## ========== LITTLE COSMETIC MODIFICATION OF CUST TABLE
########################################################

cust <- read.csv('data/cust.csv')
cust$customer_id = as.character(cust$customer_id)

cust$created_at = as.POSIXct(cust$created_at)
cust$updated_at = as.POSIXct(cust$updated_at)
cust$first_order_date = as.POSIXct(cust$first_order_date)
cust$last_order_date = as.POSIXct(cust$last_order_date)

# arrange by created at
cust = cust[with(cust, order(created_at)), ]

# set new factors for gender
cust$gender <- as.character(cust$gender)
cust$gender[cust$gender %in% c("","Edge","Zeitler","Brocklesby")] <- "Unk"
cust$gender <- factor(paste("gender",cust$gender, sep=""))

# set new factors for days_worn behaviour
cust$days_worn <- as.character(cust$days_worn)
cust$days_worn[ cust$days_worn %in% c("Yes","N/A","NULL")] <- "durationDaysWornUnk"
cust$days_worn[ cust$days_worn %in% c("0","No","I don't wear lenses")] <- "durationDaysWornZero"
cust$days_worn[ cust$days_worn %in% c("1 day in a week","2 days in a week","3 days in a week")] <- "durationDaysWorn1to3"
cust$days_worn[ cust$days_worn %in% c("4 days in a week","5 days in a week","6 days in a week")] <- "durationDaysWorn4to6"
cust$days_worn[ cust$days_worn == "7 days in a week"] <- "durationDaysWorn7"
cust$days_worn <- factor(cust$days_w)

# set new factors for unsubscribe all
cust$unsubscribe_all <- tolower(as.character(cust$unsubscribe_all))
cust$unsubscribe_all[ cust$unsubscribe_all %in% c("yes","yes ","1")] <- "optOutAllMarketingYes"
cust$unsubscribe_all[ cust$unsubscribe_all %in% c("0","no","","null","2")] <- "optOutAllMarketingNo"
cust$unsubscribe_all <- factor(cust$unsubscribe_all)


# Features to consider :
# gender, unsubscribe_all, days_worn, tenure (last_order_date - first_order_date), 

########################################################
## ========== USER FEATURE GLOBAL
########################################################
dir.create('features')
feat.cust.global <- data.frame(cbind('customer_id'=as.character(cust$customer_id),
                                         'gender'=cust$gender,
                                          'unsubscribe_all' =cust$unsubscribe_all,
                                         'days_worn'=as.factor(cust$days_worn)))

# featurize first order date of user_id
feat.cust.global$first_order_date = cust$first_order_date
feat.cust.global$first_order_date_year = as.POSIXlt(feat.cust.global$first_order_date)$year + 1900
feat.cust.global$first_order_date_mon = as.POSIXlt(feat.cust.global$first_order_date)$mon + 1
feat.cust.global$first_order_date_mday = as.POSIXlt(feat.cust.global$first_order_date)$mday 
feat.cust.global$first_order_date_wday = as.POSIXlt(feat.cust.global$first_order_date)$wday + 1
feat.cust.global$first_order_date_yday = as.POSIXlt(feat.cust.global$first_order_date)$yday + 1

# featurize last order date of user_id
feat.cust.global$last_order_date = cust$last_order_date
feat.cust.global$last_order_date_year = as.POSIXlt(feat.cust.global$last_order_date)$year + 1900
feat.cust.global$last_order_date_mon = as.POSIXlt(feat.cust.global$last_order_date)$mon + 1
feat.cust.global$last_order_date_mday = as.POSIXlt(feat.cust.global$last_order_date)$mday 
feat.cust.global$last_order_date_wday = as.POSIXlt(feat.cust.global$last_order_date)$wday + 1
feat.cust.global$last_order_date_yday = as.POSIXlt(feat.cust.global$last_order_date)$yday + 1

# featurize tenure (last_order_date - first_order_date)
feat.cust.global$tenure = (as.numeric(feat.cust.global$last_order_date) - as.numeric(feat.cust.global$first_order_date))/(60*60*24)

# featurize gender
levels(feat.cust.global$gender) <-   levels(cust$gender)
feat.cust.global = data.frame(cbind(feat.cust.global, data.frame(model.matrix(~gender-1, feat.cust.global))))

# featurize business channel
levels(feat.cust.global$unsubscribe_all) <-   levels(cust$unsubscribe_all)
feat.cust.global = data.frame(cbind(feat.cust.global, data.frame(model.matrix(~unsubscribe_all-1, feat.cust.global))))

# featurize days worn
levels(feat.cust.global$days_worn) <-  levels(cust$days_worn)
feat.cust.global = data.frame(cbind(feat.cust.global, data.frame(model.matrix(~days_worn-1, feat.cust.global))))

feat.cust.global[,c(2:4)] <- NULL
write.table(feat.cust.global, 'features/features.cust.global.csv', row.names = F, col.names = T, sep=',')


########################################################
## ========== LITTLE COSMETIC MODIFICATION OF PROD TABLE
########################################################
prod <- read.csv('data/prod.csv')
brand <- read.csv('data/keys/name_brand.csv')

prod <- join(prod, brand, by = c('name'))
colnames(prod)[20] <- 'brand'

prod$product_id = as.character(prod$product_id)
prod$brand = as.character(prod$brand)
prod$created_at = as.POSIXct(prod$created_at)
prod$updated_at = as.POSIXct(prod$updated_at)

# featurize brand
prod$brand = as.character(prod$brand)
prod$brand[is.na(prod$brand)] <- 'other'
prod$brand <- factor(paste("brand",prod$brand, sep=""))

# featurize type
prod$type = as.character(prod$type)
prod$type[prod$type== ""] <- 'nonLens'
prod$type<- factor(paste("typeLens",prod$type, sep=""))

# refactoring manufacturer
prod$mod.manufacturer = prod$manufacturer
levels(prod$mod.manufacturer) = c('other','other','other', 'other', 'other',
                                  'amo','bausch_Lomb','ciba_vision', 'other','coopervision',
                                  'other','other','other','other','other',
                                  'other', 'johnson_johnson','other','other','other',
                                  'other','other','other','other','other',
                                  'other','other','other','other','other',
                                  'other','other')

# refactoring category
prod$mod.category = prod$category
levels(prod$mod.category) = c('dailies','other','solution','colours','dailies',
                              'eye_care','eye_care','eye_care','solution','solution',
                              'eye_care','eye_care','nondailies','solution','dailies',
                              'eye_care','other','other','nondailies','solution',
                              'nondailies')

# refactoring days perlens
prod$mod.daysperlens <- prod$daysperlens
levels(prod$mod.daysperlens) <- c('other','1','14','30','other')


# refactoring discountinued product
prod$mod.product_lifecycle <- prod$product_lifecycle
levels(prod$mod.product_lifecycle) <- c('other','discountinued','other','regular')

# refactoring pack qty
prod$mod.pack_qty <- prod$pack_qty
levels(prod$mod.pack_qty) <- c('1','more_than10','2','more_than10','3_to6',
                               'more_than10','3_to6','3_to6','more_than10','1')

########################################################
## ========== ITEM FEATURE GLOBAL
########################################################
feat.item.global = data.frame(cbind('product_id'=as.character(prod$product_id),
                                    'manufacturer'=as.factor(prod$mod.manufacturer),
                                    'category' =as.factor(prod$mod.category),
                                    'lenstype'=as.factor(prod$type),
                                    'product_lifecycle' = as.factor(prod$mod.product_lifecycle),
                                    'brand' =  as.factor(prod$brand),
                                    'pack_qty' = as.factor(prod$mod.pack_qty)))

# featurize manufacturer
levels(feat.item.global$manufacturer) <-   levels(prod$mod.manufacturer)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~manufacturer-1, feat.item.global))))

# featurize category
levels(feat.item.global$category) <-   levels(prod$mod.category)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~category-1, feat.item.global))))

# featurize lens type
levels(feat.item.global$lenstype) <-   levels(prod$type)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~lenstype-1, feat.item.global))))

# featurize product_lifecycle
levels(feat.item.global$product_lifecycle) <-   levels(prod$mod.product_lifecycle)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~product_lifecycle-1, feat.item.global))))

# featurize brand
levels(feat.item.global$brand) <-   levels(prod$brand)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~brand-1, feat.item.global))))

feat.item.global$average_cost = as.numeric(as.character(prod$average_cost))
feat.item.global$average_cost[which(is.na(feat.item.global$average_cost))] <- 0

# create bucket of price list for average cost
breaks = c(seq(0,2.5,0.5),seq(5,25,5))
tmp <- data.frame(matrix(ncol = (length(breaks)-1), nrow = nrow(feat.item.global)))
for (i in 1:(length(breaks)-1)){
  tmp[,i] <- as.numeric(feat.item.global$average_cost[i] & feat.item.global$average_cost < breaks[i+1])
}
colnames(tmp) = paste0('avg_cost_', 1:(length(breaks)-1))
feat.item.global <- cbind(feat.item.global, tmp)

# drop unecessary columns
feat.item.global[,c(2:7)] <- NULL
write.table(feat.item.global, 'features/features.item.global.csv', row.names = F, col.names = T, sep=',')



