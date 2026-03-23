## Small function, mostly for the sake of normalizing authorship values when
# making taxonomic joins.
## This seeks to eliminate diacritical marks and allow for umlauts
normalize_accents <- function(string) {
  normalized_string <- string %>%
    # Replace umlauts with normalized versions
    stringi::stri_replace_all_fixed(c("ä", "ö", "ü", "Ä", "Ö", "Ü"),
                                    c("ae", "oe", "ue", "Ae", "Oe", "Ue"),
                                    vectorize_all = FALSE) %>%
    # Remove accents
    stringi::stri_trans_general("Latin-ASCII") %>%
    # Lowercase
    tolower()
  return(normalized_string)
}