library(gpindexExtra)
library(IndexNumR)

all.equal(tornqvist_geks(1, 1, 1, 1), c("1" = 1))

all.equal(fisher_geks(integer(0), numeric(0), logical(0), character(0)),
          structure(numeric(0), names = character(0)))

dat <- data.frame(price = 1:15, quantity = 1:15, period = 1:3, product = rep(1:5, each = 3))

all.equal(as.numeric(with(dat, tornqvist_geks(price, quantity, period, product))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", prodID = "product", window = 3)))

all.equal(as.numeric(with(dat, geks(fisher_index)(price, quantity, period, product))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", "fisher", prodID = "product", window = 3)))

all.equal(as.numeric(with(dat, geks(arithmetic_index("Walsh1"))(price, quantity, period, product))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", "walsh", prodID = "product", window = 3)))

dat <- dat[-c(1, 5, 15), ]

all.equal(as.numeric(with(dat, geks(matched(geometric_index("Tornqvist")))(price, quantity, period, product, na.rm = TRUE))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", prodID = "product", window = 3)))

all.equal(as.numeric(with(dat, geks(matched(fisher_index))(price, quantity, period, product, na.rm = TRUE))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", "fisher", prodID = "product", window = 3)))

all.equal(as.numeric(with(dat, geks(matched(arithmetic_index("Walsh1")))(price, quantity, period, product, na.rm = TRUE))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", "walsh", prodID = "product", window = 3)))
