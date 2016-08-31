IHS <- function(x, theta) {
  # log(theta*x+((theta*x)^2+1)^(1/2))*(1/theta)
  (1/theta)*asinh(theta * x)
}

INV.IHS <- function(x, theta) {
  (1/theta)*sinh(theta * x)
}


x= feat.cust.global$days_lastorder_to_created
x$log = log(x)

do.call(data.frame, list(mean = apply(!is.na(x), 2, mean),
                                   sd = apply(!is.na(x), 2, sd),
                                   median = apply(!is.na(x), 2, median),
                                   min = apply(!is.na(x), 2, min),
                                   max = apply(!is.na(x), 2, max),
                                   n = apply(!is.na(x), 2, length),
                                   type = sapply(x, class)))
describe(x)
hist(exp(x))
hist(x^(1/3))
hist(log(x)) # choose log
hist(log10(x))
hist(log2(x))
hist(x^2)
hist(IHS(x,0.001))
hist(INV.IHS(x,0.001))
hist(tanh(x))
hist(sinh(x))


####################################################################################


x = trans2[which(trans2$customer_id %in% cust),c("customer_id",'order_date')]
x = x [order(x[,1]),]
x = data.table(x)
x[, id:= seq_along(order_date), by = customer_id]
x
x1 = x %>% filter(id > 1) %>% arrange(desc(customer_id))

merge(x=x, y=x1, by =c('customer_id'), all.x = T)


#####################


# featurize payment_method
feat = data.frame(cbind(feat, data.frame(model.matrix(~payment_method-1, feat))))
feat = data.frame(cbind(feat, data.frame(model.matrix(~price_type-1, feat))))
feat = data.frame(cbind(feat, data.frame(model.matrix(~order_type-1, feat))))

# drop them off       
feat$payment_method <- NULL
feat$price_type <- NULL
feat$order_type <- NULL

# cater feature for factors
tmp = feat[!duplicated(feat),-(3:4)]
tmp = tmp[!duplicated(tmp),]

#rownames(tmp) <- paste0(tmp$customer_id, tmp$product_id)
#tmp = feat_tmp[which(feat_tmp$customer_id=='u100006' & feat_tmp$product_id== 'p190'),]

tmp$mod_cust_prod = paste0(tmp$customer_id,tmp$product_id)
key = tmp[,c('customer_id','product_id','mod_cust_prod')]
key = key[!duplicated(key),]
tmp$customer_id <- NULL
tmp$product_id <- NULL

melted <- melt(tmp,id.vars=c('mod_cust_prod'))
tmp_ddply = ddply(melted, .(mod_cust_prod, variable),summarize, max=max(value))
tmp_melt = melt(tmp_ddply, id=c("mod_cust_prod", "variable"), variable.name="variable2")
tmp = dcast(tmp_melt, mod_cust_prod ~ variable + variable2)
feat.factors = join(key,tmp,by="mod_cust_prod")
str(feat.factors)



###############################################

# delete the duplication
feat = feat[!duplicated(feat),]

rownames(feat) <- paste0(feat$customer_id, feat$product_id)

# cater feature for global_VAT
x = feat[which(feat$customer_id=='u100002' & feat$product_id== 'p21'),
         c('customer_id','product_id','global_line_total_exc_vat')]
melted <- melt(x,id.vars=c('customer_id','product_id'))
tmp_mean= dcast(melted, customer_id+product_id ~ variable, mean)
colnames(tmp_mean)[3] <- 'avg_mean_payment'

tmp_count= dcast(melted, customer_id+product_id ~ variable, length)
colnames(tmp_count)[3] <- 'count_product'


