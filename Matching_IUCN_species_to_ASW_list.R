## In this chunk of code, I have attempted to standardise names across the Amphibian Species of the World (ASW) species checklist (8 July 2025) and the IUCN range polygons (from Luedtke et al. 2023).
## Luedtke et al. (2023) uses an older version of amphibian taxonomy and thus, may not have up-to-date species names or generic classifications.

## Required package libraries
library(AmphiNom); library(phangorn); library(sf); library(tidyverse)

## Loading the range polygon shapefile dataset.
gaa_ranges <- read_sf(file.path(wd$data, "GAA2_fixedgeom.shp")) %>%
  select(sci_name, geometry) %>% drop_na(sci_name)

# I will first load the latest ASW species list using the AmphiNom package. The AmphiNom::getTaxonomy() returns the latest ASW species list. I have already donwloaded the dataframe `asw`, and will use that.

asw <- read_csv(file.path(wd$data, "asw_8Jul2025.csv")) %>%
  mutate(binomial = as.character(species)) %>%
  filter(order == "Anura")
## Then, I will load all species synonyms from ASW using AmphiNom::get_synoynms function. This takes a long time. I have already downloaded the table, so I will simply read it in.
asw_full <- get_synonyms(asw_taxonomy = asw, Order = "Anura")
asw_full <- read_csv(file.path(wd$data, "asw_synonym_table_9jul2025.csv"))

### Next, I will compare the synonymy table against the species assessed by Luedtke et al. (2023) IUCN ranges and match them using the AmphiNom::asw_sync function.
match_iucn_asw <- asw_sync(query = gaa_ranges$species, asw = asw_full, interactive = F)
synonym_report(match_iucn_asw, verbose = F) # this shows me the report and lists any issues during the process.

## I then save the matched species dataframe and manually resolve the issues.
write_csv(match_iucn_asw, file.path(wd$data,"match_iucn_asw.csv"))
match_iucn_asw <- read_csv(file.path(wd$data, "match_iucn_asw.csv"))

## Once I matched all species tips to a valid ASW species name, I will remove any duplicates or  invalid species.
new_iucn_species <- match_iucn_asw %>% mutate(genus = word(ASW_names,1)) %>%
  filter(to_remove == 0) #species to be removed were given a values of 1.

## In the next step, i will join this df with the sf_dataframe (`gaa_ranges`) and drop any non matchs (with ASW names).
gaa_ranges <- left_join(gaa_ranges, match_iucn_asw, by=c("sci_name" = "query")) %>%
  select(ASW_names, geometry) %>% drop_na(ASW_names)

## My resulting sf_df contains a geometry column. A species may have multiple rows in this dataframe, this is due to species that have multiple separate polygons or the ones I had marked as synonyms of other valid species during my manual check. I will now unionise the polygons, retaining one row per species.

gaa_ranges <- gaa_ranges %>% rename(species = ASW_names) %>%
  group_by(species) %>% summarize(geometry = st_union(geometry)) %>% 
  drop_na(species)