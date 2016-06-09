###################################################
### Code for Section 7.2
###################################################
UKborder <- readRDS("data/GEO/UK/AdminBoundaries/UK_adm0.rds")
UKadmin1 <- readRDS("data/GEO/UK/AdminBoundaries/UK_adm1.rds")

library(spdep)
temp <- poly2nb(UKadmin1)
nb2INLA("~/UK.graph", temp)
UK.adj <- "~/UK.graph"

library(INLA)
H <- inla.read.graph(filename="~/UK.graph")
image(inla.graph2matrix(H), xlab="", ylab="")

formula <- y ~ 1 + f(NUTS1_code, model="bym", graph=UK.adj)

###
library(INLA)

# Load the data
df <- readRDS("~/Dropbox/Projects/kehra/data/Pollution/allPollutantsClima.rds")
df <- df[with(df, order(Date, id)), ]
df <- df[, c("id", "Date", "Z", "Longitude", "Latitude", "WS", "TEMP", "HMIX", "PREC", "WD", "PM10") ]
head(df)

coordinates <- df[df$Date=="2014-01-01",c("id", "Longitude", "Latitude")]
rownames(coordinates) <- coordinates[,"id"]
# Borders (in km)
borders <- readRDS("data/GEO/UK/AdminBoundaries/UK_adm0.rds")
tmp<- slot(borders, 'polygons')
sub.tmp <- slot(tmp[[1]],'Polygons')
borders <- data.frame(sub.tmp[[1]]@coords)
names(borders) <- c("Longitude", "Latitude")

n_stations <- length(coordinates$id) #24 stations
n_data <- length(df$id) #4368 space-time data
n_days <- n_data/n_stations #182 time points

df$time <- rep(1:n_days, each=n_stations)
coordinates.allyear <- as.matrix(coordinates[df$id, c("Longitude","Latitude")])
dim(coordinates.allyear)

df$logPM10 <- log(df$PM10)
mean_covariates <- apply(df[,3:10],2,mean, na.rm=TRUE)
sd_covariates <- apply(df[,3:10],2,sd, na.rm=TRUE)
df[,3:10] <- scale(df[,3:10],center=mean_covariates, scale=sd_covariates)

# # Load the covariate arrays (each array except for A is 56x72x182)
# library(dismo)
# AltitudeGRID <- getData("alt", country = "GBR")
# load("Air pollution in Piemonte/Covariates/Altitude_GRID.Rdata") #A; AltitudeGRID
# load("Air pollution in Piemonte/Covariates/WindSpeed_GRID.Rdata") #WS; WindSpeedGRID
# load("Air pollution in Piemonte/Covariates/HMix_GRID.Rdata") #HMIX; HMixMaxGRID
# load("Air pollution in Piemonte/Covariates/Emi_GRID.Rdata") #EMI; EmiGRID
# load("Air pollution in Piemonte/Covariates/Temp_GRID.Rdata") #TEMP; Mean_Temp
# load("Air pollution in Piemonte/Covariates/Prec_GRID.Rdata") #PREC; Prec
# # Load the Piemonte grid c(309,529),c(4875,5159),dims=c(56,72)
# load("Air pollution in Piemonte/Covariates/Piemonte_grid.Rdata")
# 
# # Extract the standardized covariate for day i_day (you get a 56X72X8 matrix)
# i_day <- 122
# which_date <- unique(df$Date)[i_day]
# print(paste("**---- You will get a prediction for", which_date, "---**"))
# 
# # Standardise the covariates for the selected day
# source("~/Dropbox/Development/Rdev/RINLA/Air pollution in Piemonte/Covariates/covariates_selector.R")
# covariate_array_std <- covariates_selector_funct(i_day, mean_covariates,
#                                                  sd_covariates)
# 
# # Set to NA the (standardized) altitude values > 7 (1000 n)
# elevation <- covariate_array_std[,,1]
# index_mountains <- which(elevation>7)
# elevation[elevation>7] <- NA
# covariate_array_std[,,1] <- elevation
# 
# # Reshape the 3D array (56x72x8) into a dataframe (4032x8) with the 8 covariates on the columns
# covariate_matrix_std <- data.frame(apply(covariate_array_std,3,function(X) c(t(X))))
# colnames(covariate_matrix_std) <- colnames(df[,3:10])
# # colnames(covariate_matrix_std) <- "colnames(df[,3])"
# 
# # *** Code for Figure 7.8 top
# plot(Piemonte_grid,col="grey",pch=18, asp=1, xlim=range(Piemonte_grid$Longitude_km))
# lines(borders, lwd=3, asp=1)
# points(coordinates$Longitude, coordinates$Latitude, pch=20, cex=2)
# # ***

# *** Code for Figure 7.8 bottom
Piemonte_mesh <- inla.mesh.2d(loc=cbind(coordinates$Longitude,
                                        coordinates$Latitude),
                              loc.domain=borders, 
                              offset=c(10, 140), 
                              max.edge=c(50, 1000))
Piemonte_mesh <- inla.mesh.create(borders) 
plot(Piemonte_mesh,asp=1,main="")
lines(borders, lwd=3)
points(coordinates$Longitude, coordinates$Latitude, pch=20, cex=2)
# ***

Piemonte_spde <- inla.spde2.matern(mesh=Piemonte_mesh, alpha=2)
A_est <- inla.spde.make.A(mesh=Piemonte_mesh,
                          loc=coordinates.allyear,
                          group=df$time,
                          n.group=n_days)
dim(A_est)

s_index <- inla.spde.make.index(name="spatial.field",
                                n.spde=Piemonte_spde$n.spde,
                                n.group=n_days)
names(s_index)

stack_est <- inla.stack(data=list(logPM10=df$logPM10),
                    A=list(A_est, 1),
                    effects=list(c(s_index,list(Intercept=1)),
                                 list(df[,3:10])), tag="est")
#                                list(df[,3])), tag="est")

A_pred <- inla.spde.make.A(mesh=Piemonte_mesh,
                           loc=as.matrix(Piemonte_grid),
                           group=i_day,  #selected day for prediction
                           n.group=n_days)
stack_pred <- inla.stack(data=list(logPM10=NA),
                         A=list(A_pred,1),
                         effects=list(c(s_index,list(Intercept=1)), list(covariate_matrix_std)),
                         tag="pred")

stack <- inla.stack(stack_est, stack_pred)

formula <- logPM10 ~ -1 + Intercept + A + Longitude + Latitude + WS + TEMP + HMIX + PREC + EMI +
  f(spatial.field, model=Piemonte_spde,group=spatial.field.group, control.group=list(model="ar1"))

# ATTENTION: the run is computationally intensive!
output <- inla(formula,
       data=inla.stack.data(stack, spde=Piemonte_spde),
       family="gaussian",
       control.predictor=list(A=inla.stack.A(stack), compute=TRUE))

# Fixed effects betas
fixed.out <- round(output$summary.fixed,3)
# Hyperparameters sigma2eps and AR(1) a
rownames(output$summary.hyperpar)

sigma2e_marg <- inla.tmarginal(function(x) 1/x,output$marginals.hyperpar[[1]])
sigma2e_m1 <- inla.emarginal(function(x) x, sigma2e_marg)
sigma2e_m2 <- inla.emarginal(function(x) x^2, sigma2e_marg)
sigma2e_stdev <- sqrt(sigma2e_m2 - sigma2e_m1^2)
sigma2e_quantiles <- inla.qmarginal(c(0.025, 0.5, 0.975), sigma2e_marg)

ar <- output$summary.hyperpar["GroupRho for spatial.field",]

# Spatial parameters sigma2 and range
mod.field <- inla.spde2.result(output, name="spatial.field", Piemonte_spde)

var.nom.marg <- mod.field$marginals.variance.nominal[[1]]
var.nom.m1 <- inla.emarginal(function(x) x, var.nom.marg)
var.nom.m2 <- inla.emarginal(function(x) x^2, var.nom.marg)
var.nom.stdev <- sqrt(var.nom.m2 - var.nom.m1^2)
var.nom.quantiles <- inla.qmarginal(c(0.025, 0.5, 0.975), var.nom.marg)

range.nom.marg <- mod.field$marginals.range.nominal[[1]]
range.nom.m1 <- inla.emarginal(function(x) x, range.nom.marg)
range.nom.m2 <- inla.emarginal(function(x) x^2, range.nom.marg)
range.nom.stdev <- sqrt(range.nom.m2 - range.nom.m1^2)
range.nom.quantiles <- inla.qmarginal(c(0.025, 0.5, 0.975), range.nom.marg)

index_pred <- inla.stack.index(stack,"pred")$data
lp_marginals <- output$marginals.linear.predictor[index_pred]

lp_mean <- unlist(lapply(lp_marginals, function(x) inla.emarginal(exp, x)))
lp_grid_mean <- matrix(lp_mean, 56, 72, byrow=T)

# Select only points inside Piemonte and set NA to the outer points
lp_grid_mean[index_mountains] <- NA
library(splancs)
inside_Piemonte <- matrix(inout(Piemonte_grid, borders), 56, 72, byrow=T)
inside_Piemonte[inside_Piemonte==0] <- NA
inside_lp_grid_mean <- inside_Piemonte *  lp_grid_mean

seq.x.grid <- seq(range(Piemonte_grid[,1])[1],range(Piemonte_grid[,1])[2],length=56)
seq.y.grid <- seq(range(Piemonte_grid[,2])[1],range(Piemonte_grid[,2])[2],length=72)

# *** Code for Figure 7.9
print(levelplot(x=inside_lp_grid_mean,
                row.values=seq.x.grid,
                column.values=seq.y.grid,
                ylim=c(4875,5159), xlim=c(309,529),
                col.regions=gray(seq(.9,.2,l=100)),
                aspect="iso",
                contour=TRUE, labels=FALSE, pretty=TRUE,
                xlab="",ylab=""))
trellis.focus("panel", 1, 1, highlight=FALSE)
lpoints(borders,col=1,cex=.25)
lpoints(coordinates$Longitude, coordinates$Latitude,col=1,lwd=2,pch=21)
trellis.unfocus()
# ***

# *** Code for Figure 7.10
threshold <- log(50)
prob  <- lapply(X=lp_marginals, FUN=function(x) inla.pmarginal(marginal=x,threshold))
tailprob_grid <- matrix(1-unlist(prob),56,72, byrow=T)

tailprob_grid[index_mountains] <- NA
inside_tailprob_grid <- inside_Piemonte *  tailprob_grid

print(levelplot(x=inside_tailprob_grid,
                row.values=seq.x.grid,
                column.values=seq.y.grid,
                ylim=c(4875,5159), xlim=c(309,529),
                at=seq(0,1,by=.1),
                col.regions=gray(seq(.9,.2,l=100)),
                aspect="iso",
                contour=TRUE, labels=FALSE, pretty=TRUE,
                xlab="",ylab=""))
trellis.focus("panel", 1, 1, highlight=FALSE)
lpoints(borders,col=1,cex=.25)
lpoints(coordinates$Longitude, coordinates$Latitude,col=1,lwd=2,pch=21)
trellis.unfocus()
# ***

###################################################
### Code for Section 7.2.1 (run the code for Section 7.2 first)
###################################################
library(maptools)
asl <- readShapePoly("Air pollution in Piemonte/ASL_Piemonte/ASL_Piemonte.shp")
asl@data$COD

library(sp)
coords <- SpatialPoints(coordinates[,2:3]*1000) #coords of the stations
match_coords_asl <- over(coords,asl)
table(match_coords_asl$COD)

grid <- SpatialPoints(Piemonte_grid*1000) #change km --> m
match_grid_asl <- over(grid,asl)
table(match_grid_asl$COD)

sum(is.na(match_grid_asl$ID)) #number of NAs
match_grid_asl <- match_grid_asl[!is.na(match_grid_asl$ID),]

# *** Code for Figure 7.11
plot(asl,asp=1)
points(grid, col="grey",pch=18)
points(coords,pch=19)
# ***

AL_ind <- as.numeric(rownames(match_grid_asl[match_grid_asl$COD=="AL",]))
length(AL_ind)
AT_ind <- as.numeric(rownames(match_grid_asl[match_grid_asl$COD=="AT",]))
BI_ind <- as.numeric(rownames(match_grid_asl[match_grid_asl$COD=="BI",]))
CN1_ind <- as.numeric(rownames(match_grid_asl[match_grid_asl$COD=="CN1",]))
#points(grid[CN1_ind,],col=2)
CN2_ind <- as.numeric(rownames(match_grid_asl[match_grid_asl$COD=="CN2",]))
NO_ind <- as.numeric(rownames(match_grid_asl[match_grid_asl$COD=="NO",]))
TO_ind <-  as.numeric(rownames(match_grid_asl[match_grid_asl$COD=="TO",]))
TO3_ind <- as.numeric(rownames(match_grid_asl[match_grid_asl$COD=="TO3",]))
TO4_ind <- as.numeric(rownames(match_grid_asl[match_grid_asl$COD=="TO4",]))
TO5_ind <- as.numeric(rownames(match_grid_asl[match_grid_asl$COD=="TO5",]))
VC_ind <- as.numeric(rownames(match_grid_asl[match_grid_asl$COD=="VC",]))
VCO_ind <- as.numeric(rownames(match_grid_asl[match_grid_asl$COD=="VCO",]))
#points(grid[VCO_ind,],col=3)

dim_lp <- nrow(inla.stack.A(stack)) + ncol(inla.stack.A(stack))

lc_AL_vec <- rep(NA,times=dim_lp)
lc_AL_vec[index_pred][AL_ind] <- 1/length(AL_ind)
lc_AL <- inla.make.lincomb(Predictor = lc_AL_vec)
names(lc_AL) <- "lc_AL"

lc_AT_vec <- rep(NA,dim_lp)
lc_AT_vec[index_pred][AT_ind]<-1/length(AT_ind)
lc_AT <- inla.make.lincomb(Predictor = lc_AT_vec)
names(lc_AT) <- "lc_AT"

lc_BI_vec <- rep(NA,dim_lp)
lc_BI_vec[index_pred][BI_ind]<-1/length(BI_ind)
lc_BI <- inla.make.lincomb(Predictor = lc_BI_vec)
names(lc_BI) <- "lc_BI"

lc_CN1_vec <- rep(NA,dim_lp)
lc_CN1_vec[index_pred][CN1_ind]<-1/length(CN1_ind)
lc_CN1 <- inla.make.lincomb(Predictor = lc_CN1_vec)
names(lc_CN1) <- "lc_CN1"

lc_CN2_vec <- rep(NA,dim_lp)
lc_CN2_vec[index_pred][CN2_ind]<-1/length(CN2_ind)
lc_CN2 <- inla.make.lincomb(Predictor = lc_CN2_vec)
names(lc_CN2) <- "lc_CN2"

lc_NO_vec <- rep(NA,dim_lp)
lc_NO_vec[index_pred][NO_ind]<-1/length(NO_ind)
lc_NO <- inla.make.lincomb(Predictor = lc_NO_vec)
names(lc_NO) <- "lc_NO"

lc_TO_vec <- rep(NA,dim_lp)
lc_TO_vec[index_pred][TO_ind]<-1/length(TO_ind)
lc_TO <- inla.make.lincomb(Predictor = lc_TO_vec)
names(lc_TO) <- "lc_TO"

lc_TO3_vec <- rep(NA,dim_lp)
lc_TO3_vec[index_pred][TO3_ind]<-1/length(TO3_ind)
lc_TO3 <- inla.make.lincomb(Predictor = lc_TO3_vec)
names(lc_TO3) <- "lc_TO3"

lc_TO4_vec <- rep(NA,dim_lp)
lc_TO4_vec[index_pred][TO4_ind]<-1/length(TO4_ind)
lc_TO4 <- inla.make.lincomb(Predictor = lc_TO4_vec)
names(lc_TO4) <- "lc_TO4"

lc_TO5_vec <- rep(NA,dim_lp)
lc_TO5_vec[index_pred][TO5_ind]<-1/length(TO5_ind)
lc_TO5 <- inla.make.lincomb(Predictor = lc_TO5_vec)
names(lc_TO5) <- "lc_TO5"

lc_VC_vec <- rep(NA,dim_lp)
lc_VC_vec[index_pred][VC_ind]<-1/length(VC_ind)
lc_VC <- inla.make.lincomb(Predictor = lc_VC_vec)
names(lc_VC) <- "lc_VC"

lc_VCO_vec <- rep(NA,dim_lp)
lc_VCO_vec[index_pred][VCO_ind]<-1/length(VCO_ind)
lc_VCO <- inla.make.lincomb(Predictor = lc_VCO_vec)
names(lc_VCO) <- "lc_VCO"

lc_all_ASL <- c(lc_AL, lc_AT, lc_BI, lc_CN1, lc_CN2,
                lc_NO, lc_TO, lc_TO3, lc_TO4, lc_TO5,
                lc_VC, lc_VCO)
length(lc_all_ASL)

#ATTENTION: the run is computationally intensive!
output_asl <- inla(formula,
       data=inla.stack.data(stack, spde=Piemonte_spde),
       family="gaussian",
       lincomb = lc_all_ASL,
       control.predictor=list(A=inla.stack.A(stack), compute=TRUE))

output_asl$summary.lincomb.derived

asl_lp_marginals <- output_asl$marginals.lincomb.derived
asl_lp_mean_exp <- lapply(asl_lp_marginals, function(x) inla.emarginal(exp,x))

asl <- readShapePoly(paste(my.dir,"Air pollution in Piemonte/ASL_Piemonte/ASL_Piemonte.shp",sep=""))

# Extract asl coordinates
# Some operations for fixing label names and label positions on the map
asl_coords <- coordinates(asl)
asl_coords[4,2] <- asl_coords[4,2] + 20000
asl_coords <- SpatialPoints(asl_coords)
labels <- data.frame(asl_coords, attr(asl,"data")$COD)

# *** Code for Figure 7.12
asl_labels  <- list("ltext",x=labels[,1:2],labels=labels[,3],col=1,cex=1.5)
asl_lp <- data.frame(exp.mean = unlist(asl_lp_mean_exp))
asl_lp$COD <- substr(rownames(asl_lp),4,6)

asl_lp <- data.frame(exp.mean = unlist(asl_lp_mean_exp))
asl_lp$COD <- substr(rownames(asl_lp),4,6)
attr(asl, "data") <- merge(attr(asl, "data"),asl_lp,by="COD",sort=F)

spplot(obj=asl, zcol= "exp.mean", main="", col.regions=gray(64:0/64),
       contour=T, sp.layout=list(asl_labels),
       at=seq(min(inside_lp_grid_mean,na.rm=T),max(inside_lp_grid_mean,na.rm=T),length=40))
# ****


