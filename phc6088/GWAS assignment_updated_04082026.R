#################################
##### Data Preparation #######
#################################


# Import Raw Datasets #

setwd("C:/Users/jitsu/OneDrive/MS Bio/UF/Class/2026/Spring 2026/Stat Ana Genetic/Group Assignment")

## Study 3: Loya ## 
gwas3<-read.delim("3GCST90468179.tsv")

## Study 4:Loh ##
gwas4<-read.delim("GCST90029011.tsv")

## Study 5:GCST90476398 ##
gwas5<-read.delim("GCST90476398.tsv")

save(gwas3, file = "gwas2.RData")
save(gwas4, file = "gwas3.RData")
save(gwas5, file = "gwas4.RData") #New Dataset#



# Explore Data #

colnames(gwas3)
colnames(gwas4)
colnames(gwas5)

#### Merge dataset ###

library(dplyr)

#Rename variables 

m1 <- gwas4 %>% # New Dataset Loh #
  rename(
    chromosome_1 = chromosome,
    position_1 = base_pair_location,
    effective_allele_1 = effect_allele,
    other_allele_1 = other_allele,
    beta_1 = beta,
    se_1 = standard_error,
    p_1 = p_value,
    sample_size_1 = n,
    rs_id = hm_rsid
  )


m2 <- gwas5 %>% # GCST90476398 #
  rename(
    position = base_pair_location,
    effective_allele_2 = effect_allele,
    other_allele_2 = other_allele,
    beta_2 = beta,
    se_2 = standard_error,
    p_2 = p_value,
    rs_id=rsid
  )

m3 <- gwas3 %>% #Loya #
  rename(
    chromosome_3 = chromosome,
    position_3 = base_pair_location,
    effective_allele_3 = effect_allele,
    other_allele_3 = other_allele,
    beta_3 = beta,
    se_3 = standard_error,
    p_3 = p_value,
    sample_size_3 = n

  )

m3 <- m3 %>% filter(!is.na(rs_id))
m2 <- m2 %>% filter(!is.na(rs_id))
m1 <- m1 %>% filter(!is.na(rs_id))


## Merge 3 datasets by rsid ##
 

save(mergeddata, file = "mergeddata.RData")
head(mergeddata)
names(mergeddata)

## Check Beta ##
library(dplyr)

mergeddata1 <- mergeddata %>%
  mutate(
    Betaf1 = case_when(
      effective_allele_1 == other_allele_2 &
      effective_allele_1 == other_allele_3 &
      other_allele_1  == effective_allele_2 &
      other_allele_1  == effective_allele_3 ~ -beta_1,
      
      TRUE ~ beta_1
    ),
    
    Betaf2 = case_when(
      effective_allele_1 == other_allele_2 &
      effective_allele_1 == effective_allele_3 &
      other_allele_1  == effective_allele_2 &
      other_allele_1  == other_allele_3 ~ -beta_2,
      
      TRUE ~ beta_2
    ),
    
    Betaf3 = case_when(
      effective_allele_1 == effective_allele_2 &
        effective_allele_1 == other_allele_3 &
      other_allele_1  == other_allele_2 &
      other_allele_1  == effective_allele_3 ~ -beta_3,
      
      TRUE ~ beta_3
    )
  )

save(mergeddata1, file = "mergeddata1.RData")

#################################
##### Analysis #######
#################################

# Fixed Effects Meta-Analysis 
library(data.table)
library(metafor)

setDT(mergeddata1)

results <- mergeddata1[, {
  
  betas <- c(beta_1, beta_2, beta_3)
  ses   <- c(se_1, se_2, se_3)
  
  # remove missing
  keep <- !is.na(betas) & !is.na(ses)
  betas <- betas[keep]
  ses   <- ses[keep]
  
  if(length(betas) < 2){
    .(beta_fixed = NA, se_fixed = NA, z_fixed = NA, p_fixed = NA)
  } else {
    res <- metafor::rma(yi = betas, sei = ses, method = "FE", data = data.frame(betas, ses))
    
    .(
      beta_fixed = res$b,
      se_fixed   = res$se,
      z_fixed    = res$zval,
      p_fixed    = res$pval
    )
  }
  
}, by =  rs_id]

# Apply SNP selection method #
significant_snps <- results[p_fixed < 5e-8]
results_sorted <- results[order(p_fixed)]

# Report results #
nrow(significant_snps)
head(significant_snps)

save(results, file = "results.RData")
save(significant_snps, file = "sig_snps.RData")


#### Random Effect Meta Analysis ####

results_random <- mergeddata[, {
  
  betas <- c(beta_1, beta_3)
  ses   <- c(se_1, se_3)
  
  keep <- !is.na(betas) & !is.na(ses)
  betas <- betas[keep]
  ses   <- ses[keep]
  
  if(length(betas) < 2){
    .(beta_random = NA, se_random = NA, z_random = NA, p_random = NA)
  } else {
    res <- rma(yi = betas, sei = ses, method = "REML")
    
    .(
      beta_random = res$b,
      se_random   = res$se,
      z_random    = res$zval,
      p_random    = res$pval
    )
  }
  
}, by = SNP]