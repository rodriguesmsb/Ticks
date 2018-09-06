require(reshape2); require(dplyr); require(vegan); require(iNEXT); require(ggplot2); require(tidyverse);
require(ggforce)


#########################################################################################
#Define functions
#########################################################################################
permanova_table <- function(x, name){
    x <- fortify(x)
    x <- x %>% mutate(Observation = row.names(x))
    x <- x[ c(7,1:6)]
    x <- x %>% replace(., is.na(.), "-")
    write.csv(x, name, row.names = FALSE)
}

#########################################################################################
dados <- read_csv("2018_09_04_dados.csv")

nmds <- dcast(Ambiente + Ponto + Estagio  ~ Especie, data = dados)
nmds <- nmds %>% mutate(Estagio = ifelse(Estagio == "Adulto", "Adult", "Nymph"))
#########################################################################################
#iNEXT
#########################################################################################

dados <- dcast(Ambiente + Data + Estagio  ~ Especie, data = dados)


#Creating a dataset for extrapolation curves
dados_area <- dados %>% select(-c(Data, Estagio)) %>%
    group_by(Ambiente) %>% summarise_all(sum)


#Split data into lists
Peri_urban <- dados_area[dados_area$Ambiente == "Periurbano", (2:10)]

#Remove species with less than 1 observations
Peri_urban <- Peri_urban %>% select_if(funs(. >= 1))

#Insert column number as the first observation in the list
Peri_urban <- as.numeric(unlist(Peri_urban[1, ]))



Rural <- dados_area[dados_area$Ambiente == "Rural", (2:10)]
Rural <- Rural %>% select_if(funs(. >= 1))

#Insert column number as the first observation in the list
Rural <- as.numeric(unlist(Rural[1, ]))

#Create a data frame to be used with iNext function
dados_inext <- list(Peri_urban = Peri_urban, 
                    Rural= Rural)


#COmpute values
curvas <- iNEXT(dados_inext, q = c(0,1,2), 
                datatype = "abundance", 
                nboot = 100, se = TRUE, 
                conf = .95)


#Create a data frame using data to plot with ggplot2
#Types indicates the data that will be fortyfied
hill_numbers <- fortify(curvas)
hill_ci <- fortify(curvas$AsyEst)

sample_coverage <- fortify(curvas, type = 2)


hill_numbers <- hill_numbers %>%
    mutate(order = ifelse(order == 0, "Species richness",
                          ifelse(order == 1, "Shannon diversity", "Simpson diversity")))



#Write cvs
write.csv(sample_coverage, "sample_coverage.csv", row.names = FALSE)
write.csv(hill_numbers, "hill_numbers.csv", row.names = FALSE)
write.csv(hill_ci, "hill_ci.csv", row.names = FALSE)


#########################################################################################
#Permanova
#########################################################################################
all_stage <- nmds %>%
    select(-c(Estagio)) %>%
    group_by(Ambiente, Ponto) %>%
    summarise_all(funs(sum)) %>%
    mutate_if(is.numeric, funs(replace(.,. >= 1, 1)))
    
nymph_stage <- nmds %>%
    filter(Estagio == "Nymph") %>%
    mutate_if(is.numeric, funs(replace(.,. >= 1, 1)))

adult_stage <- nmds %>%
    filter(Estagio == "Adult") %>%
    mutate_if(is.numeric, funs(replace(.,. >= 1, 1)))


#Create ,atrix distances
all_distancy <- vegdist(all_stage[ ,4:11], method = "bray", binary = TRUE)
adu_distancy <- vegdist(adult_stage[ ,4:12], method = "bray", binary = TRUE)
nym_distancy <- vegdist(nymph_stage[ ,4:12], method = "bray", binary = TRUE)


#RuninG Permanova
all_mod <- adonis(all_distancy ~ all_stage$Ambiente, permutations = 4999)
adu_mod <- adonis(adu_distancy ~ adult_stage$Ambiente, permutations = 4999)
nym_mod <- adonis(nym_distancy ~ nymph_stage$Ambiente, permutations = 4999)


#Exporting ANOVA Result
permanova_table(x = all_mod$aov.tab, name = "Perm_res_all.csv")
permanova_table(x = adu_mod$aov.tab, name = "Perm_res_adult.csv")
permanova_table(x = nym_mod$aov.tab, name = "Perm_res_nymph.csv")



rsconnect::deployApp()

