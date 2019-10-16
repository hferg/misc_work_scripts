# script to transform land cover variables according to PCA
# for binomial linear modelling

# load some test data.
# land use.
lu <- raster::stack("~/Documents/hfg_work/data/leep_landuse/landuse_leep_raster.gri")
# and points.
load("./test_data/speed_testing/megachile_willughbiella_data.RData")
pd <- dat
# and now get the data extracted for these points.
# reproject the uk data!
ukdat <- pd$uk_data
sp::coordinates(ukdat) <- ~x + y
sp::proj4string(ukdat) <- sp::CRS("+proj=longlat +datum=WGS84")
ukdat <- sp::spTransform(ukdat, sp::CRS(sp::proj4string(lu))) 
pd_dat <- raster::extract(lu, ukdat)

# calculate the mean of all the data (i.e. across all points)
# first remove any NA rows - I guess these points would also have
# to be removed from the main dataframe?

m <- mean(pd_dat, na.rm = TRUE)
cv <- cov(pd_dat)

# and the covariance matrix
# How slow/fast would it be to just make a new map which has the principle
# components?
all_v <- raster::getValues(lu)
all_d <- all_v[apply(all_v, 1, function(x) all(!is.na(x))), ]
cv <- cov(all_d)
ev <- eigen(cv)

# centre all data (subtract mean from)
all_d <- all_d - m
# project the vectors into each of the n-1 eigen directions.
# this is important since it is reproduceable - I want to basically
# replicate the results of prcomp or pricomp so that the transformation
# can be specified to Exeter.

# CHECK - the last vector of eigen values ought to be 0
# this refelcts the perfect multicolinearity between
# the data that are constrained to sum to 100

# Do the same using standard PCA functions and compare outcomes - 
# then work out what needs to be retained for the future land use - 
# i.e. how do I compute pcas for the future data. Use the future
# data itself? Seems wrong, since loadings will be different...

pc <- princomp(all_d)
all_pc <- all_v
all_pc[apply(all_pc, 1, function(x) all(!is.na(x))), ] <- pc$scores
lu_pc <- raster::setValues(lu, all_pc)
library(ggfortify)
autoplot(pc, loadings = TRUE, loadings.label = TRUE, loadings.label.size = 8)

# compare prcomp
pc_2 <- prcomp(all_d)
autoplot(pc_2, loadings = TRUE, loadings.label = TRUE, loadings.label.size = 8)
all_pc_2 <- all_v
all_pc_2[apply(all_pc_2, 1, function(x) all(!is.na(x))), ] <- pc_2$x

lu_pc_2 <- raster::setValues(lu, all_pc_2)
