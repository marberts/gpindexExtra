library(gpindexExtra)

dat <- data.frame(price = 1:15, quantity = 1:15, period = 1:3, product = rep(1:5, each = 3))

#as.numeric(GEKSIndex(dat, "price", "quantity", "period", prodID = "product", window = 3))
all.equal(with(dat, tornqvist_geks(price, quantity, period, product)), 
          c("1" = 1, "2" = 1.107331768121160, "3" = 1.217529850588242))

#as.numeric(GEKSIndex(dat, "price", "quantity", "period", "fisher", prodID = "product", window = 3))
all.equal(with(dat, geks(fisher_index)(price, quantity, period, product)), 
          c("1" = 1, "2" = 1.106291370758592, "3" = 1.215570623328200))

#as.numeric(GEKSIndex(dat, "price", "quantity", "period", "walsh", prodID = "product", window = 3))
all.equal(with(dat, geks(arithmetic_index("Walsh1"))(price, quantity, period, product)), 
          c("1" = 1, "2" = 1.105909525759052, "3" = 1.214852529874356))
