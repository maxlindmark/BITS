library(taxize)
library(tidyr)
library(dplyr)

tax <- tibble(
  latin_name = c(
    "Gadus morhua",
    "Platichthys flesus", "Platichthys solemdali",
    "Limanda limanda",
    "Merlangius merlangus",
    "Pleuronectes platessus",
    "Scophthalmus maximus", "Psetta maxima"
  ),
  common_name = c(
    "cod",
    "flounder", "flounder",
    "dab",
    "whiting",
    "plaice",
    "turbot", "turbot"
  )
) |>
  mutate(
    `T` = as.numeric(get_tsn(latin_name)),
    W = as.numeric(get_wormsid(latin_name))
  ) |>
  pivot_longer(c("T", "W"),
    names_to = "SpecCodeType",
    values_to = "SpecCode"
  ) |>
  drop_na(SpecCode)

write_csv(tax, paste0(here::here(), "/output/taxa.csv"))
