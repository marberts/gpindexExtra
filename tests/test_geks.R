library(gpindexExtra)
library(IndexNumR)

# Corner cases
all.equal(tornqvist_geks(1, 1, 1, 1), c("1" = 1))

all.equal(fisher_geks(integer(0), numeric(0), logical(0), character(0)),
          structure(numeric(0), names = character(0)))

# Compare with the GEKSIndex from IndexNumR
dat <- data.frame(price = 1:65, quantity = 1:65, period = 1:13, product = rep(1:5, each = 13))
dat <- dat[c(65:60, 3:59, 1:2), ]

all.equal(as.numeric(with(dat, tornqvist_geks(price, quantity, period, product))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", prodID = "product", window = 13)))

all.equal(as.numeric(with(dat, fisher_geks(price, quantity, period, product))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", "fisher", prodID = "product", window = 13)))

all.equal(as.numeric(with(dat, fisher_geks(price, quantity, period, product, 3))), 
          {res <- as.numeric(GEKSIndex(dat, "price", "quantity", "period", "fisher", prodID = "product", window = 13)); res[11:13] / res[11]})

all.equal(as.numeric(with(dat, geks(arithmetic_index("Walsh1"))(price, quantity, period, product))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", "walsh", prodID = "product", window = 13)))

dat <- dat[-c(2:3, 7, 15, 64), ]

all.equal(as.numeric(with(dat, geks(balanced(geometric_index("Tornqvist")))(price, quantity, period, product, na.rm = TRUE))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", prodID = "product", window = 13)))

all.equal(as.numeric(with(dat, geks(balanced(fisher_index))(price, quantity, period, product, na.rm = TRUE))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", "fisher", prodID = "product", window = 13)))

all.equal(as.numeric(with(dat, geks(balanced(arithmetic_index("Walsh1")))(price, quantity, period, product, na.rm = TRUE))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", "walsh", prodID = "product", window = 13)))

# Tests for grouped operator
x <- c(1, NA, 3, 4)
w <- c(NA, 1, 1, 2)
v <- c(1, 2, NA, 4)

all.equal(arithmetic_mean(x, w, na.rm = TRUE),
          balanced(arithmetic_mean)(x, w, na.rm = TRUE))

all.equal(arithmetic_mean(x, w),
          balanced(arithmetic_mean)(x, w))

all.equal(arithmetic_mean(x, w, na.rm = TRUE),
          balanced(weighted.mean)(x, w, na.rm = TRUE))

all.equal(balanced(fisher_mean)(x, v, w, na.rm = TRUE), 4)

x <- c(1, NA, 3, 4)
w <- c(1, NA, 1, 2)
v <- c(1, 2, NA, 4)

all.equal(balanced(fisher_mean)(x, v, w, na.rm = TRUE),
          fisher_mean(c(1, 4), c(1, 4), c(1, 2)))
