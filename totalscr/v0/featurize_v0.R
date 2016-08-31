########################################################
## ========== FEATURE ENGINEERING
########################################################

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
cust$mod_gender = cust$gender
levels(cust$mod_gender) = c('unidentified','unidentified','unidentified','female','male','unidentified')

# set new factors for business channel
cust$mod_business_channel = cust$business_channel
levels(cust$mod_business_channel) <- c('other_marketing','other_marketing','other_marketing','adwords',
                                       'adwords','affiliates','other_marketing','other_marketing',
                                       'other_marketing','other_marketing','direct','other_marketing',
                                       'other_marketing','email_marketing','email_marketing','email_marketing',
                                       'email_marketing','google_shopping','other_marketing','organic',
                                       'other_marketing','other_marketing','other_marketing','other_marketing',
                                       'other_marketing','other_marketing','other_marketing','other_marketing',
                                       'other_marketing','refer_a_friend','other_marketing','other_marketing',
                                       'other_marketing','other_marketing','affiliates','other_marketing',
                                       'other_marketing','other_marketing','unknown','other_marketing',
                                       'other_marketing')

# set new factors for days_worn behaviour
cust$mod_days_worn = cust$days_worn
levels(cust$mod_days_worn) = c('dont_wear_lens','once_a_week','less_five_a_week','less_five_a_week','less_five_a_week','five_to_six_a_week',
                               'five_to_six_a_week','seven_a_week','dont_wear_lens','unknown','dont_wear_lens','unknown',
                               'unknown')

# set new factors for mod store name
cust$mod_store_name = cust$store_name
levels(cust$mod_store_name) = c('non_visiondirect_uk','non_visiondirect_uk','non_visiondirect_uk','non_visiondirect_uk','non_visiondirect_uk',
                                'non_visiondirect_uk','non_visiondirect_uk','non_visiondirect_uk','visiondirect_uk','non_visiondirect_uk',
                                'non_visiondirect_uk','non_visiondirect_uk','non_visiondirect_uk','non_visiondirect_uk')


########################################################
## ========== USER FEATURE GLOBAL
########################################################
dir.create('features')
feat.cust.global <- data.frame(cbind('customer_id'=as.character(cust$customer_id),
                                         'gender'=cust$mod_gender,
                                          'days_worn' =cust$mod_days_worn,
                                         'business_channel'=as.factor(cust$mod_business_channel),
                                     'store_name'=cust$mod_store_name))

# numerize gender category
feat.cust.global$gender <- as.numeric(feat.cust.global$gender)

# featurize registered date of user_id
feat.cust.global$created_at = cust$created_at
feat.cust.global$created_at_year = as.POSIXlt(feat.cust.global$created_at)$year + 1900
feat.cust.global$created_at_mon = as.POSIXlt(feat.cust.global$created_at)$mon + 1
feat.cust.global$created_at_mday = as.POSIXlt(feat.cust.global$created_at)$mday 
feat.cust.global$created_at_wday = as.POSIXlt(feat.cust.global$created_at)$wday + 1
feat.cust.global$created_at_yday = as.POSIXlt(feat.cust.global$created_at)$yday + 1

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

# featurize days since
feat.cust.global$days_lastorder_to_created = (as.numeric(feat.cust.global$last_order_date)  - as.numeric(feat.cust.global$created_at))/(60*60*24)
feat.cust.global$days_lastorder_to_firstoder = (as.numeric(feat.cust.global$last_order_date) - as.numeric(feat.cust.global$first_order_date))/(60*60*24)
#feat.cust.global$days_firstorder_to_created = (as.numeric(feat.cust.global$first_order_date) - as.numeric(feat.cust.global$created_at))/(60*60*24)

# featurize store name
levels(feat.cust.global$store_name) <-   levels(cust$mod_store_name)
feat.cust.global = data.frame(cbind(feat.cust.global, data.frame(model.matrix(~store_name-1, feat.cust.global))))

# featurize days worn
levels(feat.cust.global$days_worn) <-   levels(cust$mod_days_worn)
feat.cust.global = data.frame(cbind(feat.cust.global, data.frame(model.matrix(~days_worn-1, feat.cust.global))))

# featurize business channel
levels(feat.cust.global$business_channel) <-   levels(cust$mod_business_channel)
feat.cust.global = data.frame(cbind(feat.cust.global, data.frame(model.matrix(~business_channel-1, feat.cust.global))))

write.table(feat.cust.global, 'features/features.cust.global.csv', row.names = F, col.names = T, sep=',')


########################################################
## ========== LITTLE COSMETIC MODIFICATION OF PROD TABLE
########################################################
prod <- read.csv('data/prod.csv')

prod$product_id = as.character(prod$product_id)
prod$created_at = as.POSIXct(prod$created_at)
prod$updated_at = as.POSIXct(prod$updated_at)

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
                              'eye_care','eye_care','other_','solution','dailies',
                              'eye_care','other','other_','other_','solution',
                              'other_')

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
                                    'manufactur'=as.factor(prod$mod.manufacturer),
                                    'category' =as.factor(prod$mod.category),
                                    'daysperlens'=as.factor(prod$mod.daysperlens),
                                    'product_lifecycle' = as.factor(prod$mod.product_lifecycle),
                                    'pack_qty' = as.factor(prod$mod.pack_qty)))

# # engineered features for created at date
# feat.item.global$created_at = prod$created_at
# feat.item.global$created_at_year = as.POSIXlt(feat.item.global$created_at)$year + 1900
# feat.item.global$created_at_mon = as.POSIXlt(feat.item.global$created_at)$mon + 1
# feat.item.global$created_at_mday = as.POSIXlt(feat.item.global$created_at)$mday 
# feat.item.global$created_at_wday = as.POSIXlt(feat.item.global$created_at)$wday + 1
# feat.item.global$created_at_yday = as.POSIXlt(feat.item.global$created_at)$yday + 1

# engineered features for updated at date
feat.item.global$updated_at = prod$updated_at
feat.item.global$updated_at_year = as.POSIXlt(feat.item.global$updated_at)$year + 1900
feat.item.global$updated_at_mon = as.POSIXlt(feat.item.global$updated_at)$mon + 1
feat.item.global$updated_at_mday = as.POSIXlt(feat.item.global$updated_at)$mday 
feat.item.global$updated_at_wday = as.POSIXlt(feat.item.global$updated_at)$wday + 1
feat.item.global$updated_at_yday = as.POSIXlt(feat.item.global$updated_at)$yday + 1

# featurize nb days from created at to updated at
# feat.item.global$days_created_to_updated = (as.numeric(feat.item.global$updated_at)  - as.numeric(feat.item.global$created_at))/(60*60*24)

# featurize manufacturer
levels(feat.item.global$manufactur) <-   levels(prod$mod.manufacturer)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~manufactur-1, feat.item.global))))

# featurize daysperlens
levels(feat.item.global$daysperlens) <-   levels(prod$mod.daysperlens)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~daysperlens-1, feat.item.global))))

# featurize product_lifecycle
levels(feat.item.global$product_lifecycle) <-   levels(prod$mod.product_lifecycle)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~product_lifecycle-1, feat.item.global))))

# featurize product_lifecycle
levels(feat.item.global$pack_qty) <-   levels(prod$mod.pack_qty)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~pack_qty-1, feat.item.global))))

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

write.table(feat.item.global, 'features/features.item.global.csv', row.names = F, col.names = T, sep=',')



