## In this chunk of code, I have attempted to standardise names across the Amphibian Species of the World (ASW) species checklist (8 July 2025) and the Portik et al. (2023) Phylogenetic tree names.
## Portik et al. (2023) uses an older version of amphibian taxonomy and thus, may not have up-to-date species names or generic classifications.

## Required package libraries
library(AmphiNom); library(phangorn); library(ape); library(tidyverse)

## Loading the frog phylogeny
frog_tree <- read.tree(file.path(wd$data,"TreePL-Rooted_Anura_bestTree.tre"))

# I will first load the latest ASW species list using the AmphiNom package. The AmphiNom::getTaxonomy() returns the latest ASW species list. I have already donwloaded the dataframe `asw`, and will use that.

asw <- read_csv(file.path(wd$data, "asw_8Jul2025.csv")) %>%
  mutate(binomial = as.character(species)) %>%
  filter(order == "Anura")
## Then, I will load all species synonyms from ASW using AmphiNom::get_synoynms function. This takes a long time. I have already downloaded the table, so I will simply read it in.
asw_full <- get_synonyms(asw_taxonomy = asw, Order = "Anura")
asw_full <- read_csv(file.path(wd$data, "asw_synonym_table_9jul2025.csv"))

### Next, I will compare the synonymy table against the species tips in Portik et al. (2023) frog phylogeny and match them using the AmphiNom::asw_sync function.
match_tree_asw <- asw_sync(query = tree_species$binomial, asw = asw_full, interactive = F)
synonym_report(match_tree_asw, verbose = F) # this shows me the report and lists any issues during the process.

## I then save the matched species dataframe and manually resolve the issues.
## write_csv(match_tree_asw, file.path(wd$data,"match_tree_asw.csv"))
match_tree_asw <- read_csv(file.path(wd$data, "match_tree_asw.csv"))

## Once I matched all species tips to a valid ASW species name, I will remove any duplicates or  invalid species.
new_tree_species <- match_tree_asw %>% mutate(genus = word(ASW_names,1),
                                              query = str_replace(query, " ","_"),
                                              ASW_names = str_replace(ASW_names, " ","_")) %>%
  filter(to_remove == 0) #species to be removed were given a values of 1.

## I will then prune the portik frog tree so that it only contains the Anurans of interest (no outgroups).
asw_frog_tree <- drop.tip(frog_tree, setdiff(frog_tree$tip.label, new_tree_species$query))

## Next, I will carefully change the phylogeny tip labels to only retain the valid ASW names and I will replace the tree tips with new names and check monophyly again.
new_tree_species <- new_tree_species %>% mutate(query = factor(query, levels = asw_frog_tree$tip.label)) %>% arrange(query)
asw_frog_tree$tip.label <- new_tree_species$ASW_names

####### The following function will check for genus monophyly and return me a dataframe of genera and their results for monophyly.
check_genus_monophyly <- function(tree, verbose = TRUE) {
  # Extract genus names from tip labels (assumes Genus_species format)
  genera <- unique(sub("_.*", "", tree$tip.label))
  if (verbose) cat("🔍 Checking", length(genera), "genera for monophyly...\n")
  
  # Prepare output table
  results <- data.frame(
    Genus = genera,
    Monophyletic = NA,
    Expected_tips = NA,
    Observed_tips = NA,
    Note = NA,
    stringsAsFactors = FALSE
  )
  
  # Get descendant tip sets for each internal node (via phangorn)
  if (!requireNamespace("phangorn", quietly = TRUE)) {
    stop("Please install the 'phangorn' package to use this function.")
  }
  biparts <- phangorn:::bip(tree)
  
  for (i in seq_along(genera)) {
    genus <- genera[i]
    tip_indices <- which(sub("_.*", "", tree$tip.label) == genus)
    n_tips <- length(tip_indices)
    results$Expected_tips[i] <- n_tips
    
    if (n_tips == 1) {
      results$Monophyletic[i] <- TRUE
      results$Observed_tips[i] <- 1
      results$Note[i] <- "Monotypic"
      next
    }
    
    mrca_node <- ape::getMRCA(tree, tip_indices)
    
    if (is.na(mrca_node)) {
      results$Note[i] <- "MRCA not found"
      if (verbose) cat("⚠️  Genus", genus, ": MRCA not found\n")
      next
    }
    
    observed <- length(biparts[[mrca_node]])
    results$Observed_tips[i] <- observed
    results$Monophyletic[i] <- (observed == n_tips)
  }
  
  if (verbose) {
    n_fail <- sum(results$Monophyletic == FALSE, na.rm = TRUE)
    cat("✅ Finished. Found", n_fail, "non-monophyletic genera.\n")
  }
  
  return(results)
}

# Using the function
genus_monophyly_report <- check_genus_monophyly(asw_frog_tree)
genus_monophyly_report <- left_join(genus_monophyly_report, asw, by=c("Genus" = "genus")) %>%
  select(family, Genus:Note) %>% distinct(Genus, .keep_all = T)
