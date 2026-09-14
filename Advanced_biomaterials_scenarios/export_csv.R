#############################################################################################
# export_csv.R
# Runs the Advanced_biomaterials_scenarios.R calculation logic (skipping plotting/xlsx
# dependencies) and exports the resulting multi-dimensional scenario arrays as tidy CSVs.
#############################################################################################

setwd("/home/user/scenarios/Advanced_biomaterials_scenarios")

# Set input parameters ####
countrylist<-c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech_Republic","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovak_Republic","Slovenia","Spain","Sweden","United_Kingdom","EU28")
histdata<-c("Total","Bitumen","Lubricants_Waxes","non_E_consumption","PRIMES_projection[ktoe]")
histtime<-c(2000:2010,seq(2015,2050,5))

# Specific input data explained in the Schipfer et al., 2016 ####
non_E_share<-c(0.896,0.082,0.022) #for polymers, solvents and surfactants resp.
poly_share<-c(0.485,0.07,0.445) #for PE&PP, PET and more durable polymers (others) resp.
biobased_08<-c(0,0.15,0.13,0.63,1.52)*1000 #in tonnes for bitumen, lubr., poly., solv. & surf. (BioChem_Report)
biobased_MS<-c(10,11,15,20,27,28) #assuming biobased production to be situated in MS with funding (OECD,2013)
bio_poly_share<-c(0.87,0,0.13) #for PLA, Starch blends, PHA, PBAT vs PET vs PUR,PA and others resp.
polymers_14<-1.697*0.154*1000 #biobased polymers global production * European share for 2014
gluc_current<-1/c(0.7185,0.53,0.286,0.472)
gluc_theoretical<-1/c(0.8,0.556,0.311,0.511)
oil_current<-1/c(0.8,0.7691)
oil_theoretical<-1/c(1,0.4615)
gluc<-gluc_current
oil<-oil_current


# Calculate heating values for biogenic building blocks ####
HHV<-function(C,H,O,N,S,ash){0.3491*C+1.1783*H-0.1034*O-0.0151*N+0.1005*S-0.0211*ash} #Channiwala and Parikh
# glucose heating value for pure C6H12O6
weight_g<-(6*12.011+12*1.008+6*15.999)
gluc_HHV<-HHV((6*12.011)/weight_g,(12*1.008)/weight_g,(6*15.999)/weight_g,0,0,0)*100
# lignin heating value for pure C10H12O3
weight_l<-(10*12.011+12*1.008+3*15.999)
lig_HHV<-HHV((10*12.011)/weight_l,(12*1.008)/weight_l,(3*15.999)/weight_l,0,0,0)*100
# plant oil heating value for pure C18H32O2
weight_o<-(18*12.011+32*1.008+2*15.999)
oil_HHV<-HHV((18*12.011)/weight_o,(32*1.008)/weight_o,(2*15.999)/weight_o,0,0,0)*100
HHVs<-c(gluc_HHV,gluc_HHV,lig_HHV,oil_HHV)

# Read array on fossil based materials. Historic data from IEA&OSCE and the PRIMES reference scenario
ref<-readRDS("OSCE_IEA_PRIMES_data.rds")

# Project fossil non-E consumption up to 2050 based on PRIMES reference scenario ####
for(i in 1:28){
  for(j in 12:19){
    growth<-ref[5,j,i]/ref[5,j-1,i]
    ref[2:4,j,i]<-ref[2:4,j-1,i]*growth
  }
}
ref[,,]<-replace(ref,is.na(ref),0)
ref[1,12:19,1:28]<-apply(ref[2:4,12:19,1:28],c(2,3),sum)
ref[,12:19,29]<-apply(ref[,12:19,1:28],c(1,2),sum)

# Estimate product groups to be substituted ####
fossil<-c("Production","Bitumen","Lubricants","PE_PP","PET","Other","Solvents","Surfactants")

substitut<-array(0,dim=c(length(fossil),length(histtime),length(countrylist)))
dimnames(substitut)[[1]]<-fossil
dimnames(substitut)[[2]]<-histtime
dimnames(substitut)[[3]]<-countrylist

substitut[2:3,,]<-ref[2:3,,]
substitut[4,,]<-ref[4,,]*non_E_share[1]*poly_share[1]
substitut[5,,]<-ref[4,,]*non_E_share[1]*poly_share[2]
substitut[6,,]<-ref[4,,]*non_E_share[1]*poly_share[3]
substitut[7,,]<-ref[4,,]*non_E_share[2]
substitut[8,,]<-ref[4,,]*non_E_share[3]
substitut[1,,]<-apply(substitut[2:8,,],c(2,3),sum)

# Current biobased-shares in the different MS ####
bio<-c("Production","Bitumen","Lubricants","PHB_PLA","PET","Other_polymers","Solvents","Surfactants")
scenariotime<-seq(2015,2050,5)

biobased<-array(0,dim=c(length(bio),length(scenariotime),length(countrylist)))
dimnames(biobased)[[1]]<-bio
dimnames(biobased)[[2]]<-scenariotime
dimnames(biobased)[[3]]<-countrylist


development<-ref[1,12,29]/ref[1,9,29] #fossil relation 2015/2008
biobased[c(2:3,7:8),1,29]<-biobased_08[c(1:2,4:5)]*development #assuming similar development for all but polymers

polymers_15<-polymers_14+(polymers_14-biobased_08[3])/5 #assuming linear growth rate
biobased[4:6,1,29]<-polymers_15*bio_poly_share

productshare<-substitut[2:8,12,biobased_MS]/apply(substitut[2:8,12,biobased_MS],c(1),sum)
biobased[2:8,1,biobased_MS]<-biobased[2:8,1,29]*productshare

# Projection of substitution up to 2050 for two scenarios ####
names<-c("Reference","Full_substitution")

scenarios<-array(0,dim=c(length(bio),length(scenariotime),length(countrylist),length(names)))
dimnames(scenarios)[[1]]<-bio
dimnames(scenarios)[[2]]<-scenariotime
dimnames(scenarios)[[3]]<-countrylist
dimnames(scenarios)[[4]]<-names

scenarios[,,,c(1,2)]<-biobased[,,]

for(i in 2:8){ #reference scenario
  scenarios[3,i,,1]<-scenarios[3,i-1,,1]+substitut[3,19,]*(0.4-(scenarios[3,5,,1]/(substitut[3,12,]+scenarios[3,1,,1])))*((1+(i-2)*2)/49)
  scenarios[4,i,,1]<-scenarios[4,i-1,,1]+substitut[4,19,]*(0.4-(scenarios[4,5,,1]/(substitut[4,12,]+scenarios[4,1,,1])))*((1+(i-2)*2)/49)
  scenarios[5,i,,1]<-scenarios[5,i-1,,1]+substitut[5,19,]*(0.4-(scenarios[5,5,,1]/(substitut[5,12,]+scenarios[5,1,,1])))*((1+(i-2)*2)/49)
  scenarios[6,i,,1]<-scenarios[6,1,,1]
  scenarios[7,i,,1]<-scenarios[7,i-1,,1]+substitut[7,19,]*(0.4-(scenarios[7,5,,1]/(substitut[7,12,]+scenarios[7,1,,1])))*((1+(i-2)*2)/49)
  scenarios[8,i,,1]<-scenarios[8,i-1,,1]+substitut[8,19,]*((1+(i-2)*2)/49)
}

for(i in 2:8){ #full_transition scenario
  scenarios[2,i,,2]<-scenarios[2,i-1,,2]+substitut[2,19,]*(0.7-(scenarios[2,1,,2]/(substitut[2,12,]+scenarios[2,1,,2])))*((1+(i-2)*2)/49)
  scenarios[3,i,,2]<-scenarios[3,i-1,,2]+substitut[3,19,]*(0.7-(scenarios[3,1,,2]/(substitut[3,12,]+scenarios[3,1,,2])))*((1+(i-2)*2)/49)
  scenarios[4,i,,2]<-scenarios[4,i-1,,2]+substitut[4,19,]*(0.7-(scenarios[4,1,,2]/(substitut[4,12,]+scenarios[4,1,,2])))*((1+(i-2)*2)/49)
  scenarios[5,i,,2]<-scenarios[5,i-1,,2]+substitut[5,19,]*(0.7-(scenarios[5,1,,2]/(substitut[5,12,]+scenarios[5,1,,2])))*((1+(i-2)*2)/49)
  scenarios[6,i,,2]<-scenarios[6,i-1,,2]+substitut[6,19,]*(0.7-(scenarios[6,1,,2]/(substitut[6,12,]+scenarios[6,1,,2])))*((1+(i-2)*2)/49)
  scenarios[7,i,,2]<-scenarios[7,i-1,,2]+substitut[7,19,]*(0.7-(scenarios[7,1,,2]/(substitut[7,12,]+scenarios[7,1,,2])))*((1+(i-2)*2)/49)
  scenarios[8,i,,2]<-scenarios[8,i-1,,2]+substitut[8,19,]*((1+(i-2)*2)/49)
}

# Cleaning array from NAs and summarising all products and all countries
scenarios<-replace(scenarios,is.na(scenarios),0)
scenarios[1,,,]<-apply(scenarios[2:8,,,],c(2,3,4),sum)
scenarios[,2:8,29,]<-apply(scenarios[,2:8,1:28,],c(1,2,4),sum)

# Estimation of demand for starch/sugar/oil & wood(cellulose) ####

types<-c("Total","Starch&Sugar_based_gluc","Wood_based_gluc","Wood_based_lign","Oil_based")

biomass<-array(0,dim=c(length(types),length(scenariotime),length(countrylist),length(names)))
dimnames(biomass)[[1]]<-types
dimnames(biomass)[[2]]<-scenariotime
dimnames(biomass)[[3]]<-countrylist
dimnames(biomass)[[4]]<-names

##reference scenario (1): Starting with 2035 all glycose from starch &sugar capacities substituted with wood based
#glucose demand for capacities up to 2030:

biomass[2,1:4,1:28,1]<-scenarios[4,1:4,1:28,1]*gluc[1]+scenarios[5,1:4,1:28,1]*gluc[2]+
  scenarios[6,1:4,1:28,1]*gluc[3]+scenarios[7,1:4,1:28,1]*gluc[4]
#stagnating glucose demand from 2035 to 2050:
biomass[2,5:8,1:28,1]<-matrix(rep(t(biomass[2,4,1:28,1]),4),nrow=4,ncol=28,byrow=TRUE)
#glucose demand from wood starting with 2035:
biomass[3,5:8,1:28,1]<-scenarios[4,5:8,1:28,1]*gluc[1]+scenarios[5,5:8,1:28,1]*gluc[2]+
  scenarios[6,5:8,1:28,1]*gluc[3]+scenarios[7,5:8,1:28,1]*gluc[4]-biomass[2,5:8,1:28,1]

##full_substitution scenario (2): Starting with 2025 ... plus bitumen from wood
#glucose demand for capacities up to 2020:
biomass[2,1:2,1:28,2]<-scenarios[4,1:2,1:28,2]*gluc[1]+scenarios[5,1:2,1:28,2]*gluc[2]+
  scenarios[6,1:2,1:28,2]*gluc[3]+scenarios[7,1:2,1:28,2]*gluc[4]
#stagnating glucose demand from 2025 to 2050:
biomass[2,3:8,1:28,2]<-matrix(rep(t(biomass[2,2,1:28,2]),6),nrow=6,ncol=28,byrow=TRUE)
#glucose demand from wood starting with 2025:
biomass[3,3:8,1:28,2]<-scenarios[4,3:8,1:28,2]*gluc[1]+scenarios[5,3:8,1:28,2]*gluc[2]+
  scenarios[6,3:8,1:28,2]*gluc[3]+scenarios[7,3:8,1:28,2]*gluc[4]-biomass[2,3:8,1:28,2]
#bitumen with 1:1 from lignin
biomass[4,,1:28,2]<-scenarios[2,,1:28,2]


##both scenarios lubricants and surfactants from oil
biomass[5,,1:28,]<-scenarios[3,,1:28,]*oil[1]+scenarios[8,,1:28,]*oil[2]

#summarising all products and all countries
biomass[1,,,]<-apply(biomass[2:5,,,],c(2,3,4),sum)
biomass[,,29,]<-apply(biomass[,,1:28,],c(1,2,4),sum)

#############################################################################################
# Export the multi-dimensional scenario matrices to tidy CSVs
# (region, scenario, product, unit, one column per scenario year)
#############################################################################################

array_to_wide_csv <- function(arr, product_dim_name, out_file, unit) {
  df <- as.data.frame.table(arr, responseName = "value", stringsAsFactors = FALSE)
  colnames(df)[1:4] <- c("product", "year", "region", "scenario")
  df$unit <- unit
  wide <- reshape(
    df,
    idvar = c("region", "scenario", "product", "unit"),
    timevar = "year",
    direction = "wide"
  )
  value_cols <- setdiff(colnames(wide), c("region", "scenario", "product", "unit"))
  year_order <- sub("^value\\.", "", value_cols)
  wide <- wide[, c("region", "scenario", "product", "unit", value_cols)]
  colnames(wide)[5:ncol(wide)] <- year_order
  wide <- wide[order(wide$region, wide$scenario, match(wide$product, dimnames(arr)[[1]])), ]
  write.csv(wide, out_file, row.names = FALSE)
  invisible(wide)
}

array_to_wide_csv(scenarios, "product", "advanced_biomaterials_production_scenarios.csv", "kt")
array_to_wide_csv(biomass, "product", "advanced_biomaterials_biomass_demand.csv", "kt")

cat("Done. Files written:\n")
cat(" - advanced_biomaterials_production_scenarios.csv\n")
cat(" - advanced_biomaterials_biomass_demand.csv\n")
