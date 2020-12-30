#These are the required packages. This code will automatically load them or install then load
#if they are not present. 

load.lib <- c("equivalence", "TOSTER", "dplyr")

install.lib<-load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)



#This dataset combines the john et al. results with our own. The data used 
#here was given to us by Leslie John. Note, we did not include this dataset within 
#our OSF page. For those wishing to replicate this script and rerun these analyses, 
#permission to use the data must be acquired from Leslie John. 

data <- read.csv ("education_psychology_qrp_real.csv")


#This section of code does two things. First, it removes missing values from 
#the dataset. The second thing it does is split the dataset into the education
#and psychology componenet. The purpose of this is to have two distint vectors
#to compare using the TOST function. 

#The code is organized by item using the subscript _1, _2, and _3. For the test 
#of equivelancy for proportions, the lower and upper bounds are specified. Here, 
#we specified them as -0.05 and 0.05. For proportions, the TOST package requires
#that the mean and sample sizes be specified. Here, we use the mean() function to 
#calculate the mean and the nrow() function to calculate the sample size. 

omit_1 <- dplyr::select(data, omit_prev, Field)
omit_1  <- omit_1 [!is.na(omit_1$omit_prev),]
Ed <- subset (omit_1, omit_1$Field == "Education")
Psych <- subset (omit_1, omit_1$Field == "Psychology")
omit_equiv_1 <- tost(Ed$omit_prev, Psych$omit_prev, epsilon = 1, paired = FALSE)

omit_2 <- dplyr::select(data, omit_admit, Field)
omit_2  <- omit_2 [!is.na(omit_2$omit_admit),]
Ed <- subset (omit_2, omit_2$Field == "Education")
Psych <- subset (omit_2, omit_2$Field == "Psychology")
omit_equiv_2 <- TOSTtwo.prop(prop1 = mean(Ed$omit_admit), prop2 = mean(Psych$omit_admit),
				     n1 = nrow(Ed), n2 = nrow(Psych), 
				     low_eqbound = -0.05, high_eqbound = 0.05, alpha = .05, 
				     verbose = FALSE, plot = FALSE)

omit_3 <- dplyr::select(data, omit_opinion, Field)
omit_3  <- omit_3 [!is.na(omit_3$omit_opinion),]
Ed <- subset (omit_3, omit_3$Field == "Education")
Psych <- subset (omit_3, omit_3$Field == "Psychology")
omit_equiv_3 <- TOSTtwo.prop(prop1 = mean(Ed$omit_opinion), prop2 = mean(Psych$omit_opinion),
				     n1 = nrow(Ed), n2 = nrow(Psych), 
				     low_eqbound = -0.05, high_eqbound = 0.05, alpha = .05,
				     verbose = FALSE, plot = FALSE)

harking_1 <- dplyr::select(data, harking_prev, Field)
harking_1  <- harking_1 [!is.na(harking_1$harking_prev),]
Ed <- subset (harking_1, harking_1$Field == "Education")
Psych <- subset (harking_1, harking_1$Field == "Psychology")
harking_equiv_1 <- tost(Ed$harking_prev, Psych$harking_prev, epsilon = 1, paired = FALSE)

harking_2 <- dplyr::select(data, harking_admit, Field)
harking_2  <- harking_2 [!is.na(harking_2$harking_admit),]
Ed <- subset (harking_2, harking_2$Field == "Education")
Psych <- subset (harking_2, harking_2$Field == "Psychology")
harking_equiv_2 <- TOSTtwo.prop(prop1 = mean(Ed$harking_admit), prop2 = mean(Psych$harking_admit),
				     n1 = nrow(Ed), n2 = nrow(Psych), 
				     low_eqbound = -0.05, high_eqbound = 0.05, alpha = .05, 
				     verbose = FALSE, plot = FALSE)

harking_3 <- dplyr::select(data, harking_opinion, Field)
harking_3  <- harking_3 [!is.na(harking_3$harking_opinion),]
Ed <- subset (harking_3, harking_3$Field == "Education")
Psych <- subset (harking_3, harking_3$Field == "Psychology")
harking_equiv_3 <- TOSTtwo.prop(prop1 = mean(Ed$harking_opinion), prop2 = mean(Psych$harking_opinion),
				     n1 = nrow(Ed), n2 = nrow(Psych), 
				     low_eqbound = -0.05, high_eqbound = 0.05, alpha = .05,
				     verbose = FALSE, plot = FALSE)

rounding_1 <- dplyr::select(data, rounding_prev, Field)
rounding_1  <- rounding_1 [!is.na(rounding_1$rounding_prev),]
Ed <- subset (rounding_1, rounding_1$Field == "Education")
Psych <- subset (rounding_1, rounding_1$Field == "Psychology")
rounding_equiv_1 <- tost(Ed$rounding_prev, Psych$rounding_prev, epsilon = 1, paired = FALSE)

rounding_2 <- dplyr::select(data, rounding_admit, Field)
rounding_2  <- rounding_2 [!is.na(rounding_2$rounding_admit),]
Ed <- subset (rounding_2, rounding_2$Field == "Education")
Psych <- subset (rounding_2, rounding_2$Field == "Psychology")
rounding_equiv_2 <- TOSTtwo.prop(prop1 = mean(Ed$rounding_admit), prop2 = mean(Psych$rounding_admit),
				     n1 = nrow(Ed), n2 = nrow(Psych), 
				     low_eqbound = -0.05, high_eqbound = 0.05, alpha = .05, 
				     verbose = FALSE, plot = FALSE)

rounding_3 <- dplyr::select(data, rounding_opinion, Field)
rounding_3  <- rounding_3 [!is.na(rounding_3$rounding_opinion),]
Ed <- subset (rounding_3, rounding_3$Field == "Education")
Psych <- subset (rounding_3, rounding_3$Field == "Psychology")
rounding_equiv_3 <- TOSTtwo.prop(prop1 = mean(Ed$rounding_opinion), prop2 = mean(Psych$rounding_opinion),
				     n1 = nrow(Ed), n2 = nrow(Psych), 
				     low_eqbound = -0.05, high_eqbound = 0.05, alpha = .05,
				     verbose = FALSE, plot = FALSE)

DE_arking_1 <- dplyr::select(data, DE_arking_prev, Field)
DE_arking_1  <- DE_arking_1 [!is.na(DE_arking_1$DE_arking_prev),]
Ed <- subset (DE_arking_1, DE_arking_1$Field == "Education")
Psych <- subset (DE_arking_1, DE_arking_1$Field == "Psychology")
DE_arking_equiv_1 <- tost(Ed$DE_arking_prev, Psych$DE_arking_prev, epsilon = 1, paired = FALSE)

DE_arking_2 <- dplyr::select(data, DE_arking_admit, Field)
DE_arking_2  <- DE_arking_2 [!is.na(DE_arking_2$DE_arking_admit),]
Ed <- subset (DE_arking_2, DE_arking_2$Field == "Education")
Psych <- subset (DE_arking_2, DE_arking_2$Field == "Psychology")
DE_arking_equiv_2 <- TOSTtwo.prop(prop1 = mean(Ed$DE_arking_admit), prop2 = mean(Psych$DE_arking_admit),
				     n1 = nrow(Ed), n2 = nrow(Psych), 
				     low_eqbound = -0.05, high_eqbound = 0.05, alpha = .05, 
				     verbose = FALSE, plot = FALSE)

DE_arking_3 <- dplyr::select(data, DE_arking_opinion, Field)
DE_arking_3  <- DE_arking_3 [!is.na(DE_arking_3$DE_arking_opinion),]
Ed <- subset (DE_arking_3, DE_arking_3$Field == "Education")
Psych <- subset (DE_arking_3, DE_arking_3$Field == "Psychology")
DE_arking_equiv_3 <- TOSTtwo.prop(prop1 = mean(Ed$DE_arking_opinion), prop2 = mean(Psych$DE_arking_opinion),
				     n1 = nrow(Ed), n2 = nrow(Psych), 
				     low_eqbound = -0.05, high_eqbound = 0.05, alpha = .05,
				     verbose = FALSE, plot = FALSE)

peeking_1 <- dplyr::select(data, peeking_prev, Field)
peeking_1  <- peeking_1 [!is.na(peeking_1$peeking_prev),]
Ed <- subset (peeking_1, peeking_1$Field == "Education")
Psych <- subset (peeking_1, peeking_1$Field == "Psychology")
peeking_equiv_1 <- tost(Ed$peeking_prev, Psych$peeking_prev, epsilon = 1, paired = FALSE)

peeking_2 <- dplyr::select(data, peeking_admit, Field)
peeking_2  <- peeking_2 [!is.na(peeking_2$peeking_admit),]
Ed <- subset (peeking_2, peeking_2$Field == "Education")
Psych <- subset (peeking_2, peeking_2$Field == "Psychology")
peeking_equiv_2 <- TOSTtwo.prop(prop1 = mean(Ed$peeking_admit), prop2 = mean(Psych$peeking_admit),
				     n1 = nrow(Ed), n2 = nrow(Psych), 
				     low_eqbound = -0.05, high_eqbound = 0.05, alpha = .05, 
				     verbose = FALSE, plot = FALSE)

peeking_3 <- dplyr::select(data, peeking_opinion, Field)
peeking_3  <- peeking_3 [!is.na(peeking_3$peeking_opinion),]
Ed <- subset (peeking_3, peeking_3$Field == "Education")
Psych <- subset (peeking_3, peeking_3$Field == "Psychology")
peeking_equiv_3 <- TOSTtwo.prop(prop1 = mean(Ed$peeking_opinion), prop2 = mean(Psych$peeking_opinion),
				     n1 = nrow(Ed), n2 = nrow(Psych), 
				     low_eqbound = -0.05, high_eqbound = 0.05, alpha = .05,
				     verbose = FALSE, plot = FALSE)

simul_data_1 <- dplyr::select(data, simul_data_prev, Field)
simul_data_1  <- simul_data_1 [!is.na(simul_data_1$simul_data_prev),]
Ed <- subset (simul_data_1, simul_data_1$Field == "Education")
Psych <- subset (simul_data_1, simul_data_1$Field == "Psychology")
simul_data_equiv_1 <- tost(Ed$simul_data_prev, Psych$simul_data_prev, epsilon = 1, paired = FALSE)

simul_data_2 <- dplyr::select(data, simul_data_admit, Field)
simul_data_2  <- simul_data_2 [!is.na(simul_data_2$simul_data_admit),]
Ed <- subset (simul_data_2, simul_data_2$Field == "Education")
Psych <- subset (simul_data_2, simul_data_2$Field == "Psychology")
simul_data_equiv_2 <- TOSTtwo.prop(prop1 = mean(Ed$simul_data_admit), prop2 = mean(Psych$simul_data_admit),
				     n1 = nrow(Ed), n2 = nrow(Psych), 
				     low_eqbound = -0.05, high_eqbound = 0.05, alpha = .05, 
				     verbose = FALSE, plot = FALSE)

simul_data_3 <- dplyr::select(data, simul_data_opinion, Field)
simul_data_3  <- simul_data_3 [!is.na(simul_data_3$simul_data_opinion),]
Ed <- subset (simul_data_3, simul_data_3$Field == "Education")
Psych <- subset (simul_data_3, simul_data_3$Field == "Psychology")
simul_data_equiv_3 <- TOSTtwo.prop(prop1 = mean(Ed$simul_data_opinion), prop2 = mean(Psych$simul_data_opinion),
				     n1 = nrow(Ed), n2 = nrow(Psych), 
				     low_eqbound = -0.05, high_eqbound = 0.05, alpha = .05,
				     verbose = FALSE, plot = FALSE)

#This section of the code will call the output for each item comparison. 

omit_equiv_1
harking_equiv_1
rounding_equiv_1
DE_arking_equiv_1
peeking_equiv_1
simul_data_equiv_1

omit_equiv_2 
harking_equiv_2 
rounding_equiv_2 
DE_arking_equiv_2 
peeking_equiv_2 
simul_data_equiv_2 

omit_equiv_3 
harking_equiv_3 
rounding_equiv_3 
DE_arking_equiv_3 
peeking_equiv_3 
simul_data_equiv_3 