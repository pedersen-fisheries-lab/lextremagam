## code to generate reference list of feature_codes

feature_codes <- c("start_edge_flat", "increase", "peak", "decrease", "trough", "increase_step", "decrease_step", "end_edge_flat")
usethis::use_data(feature_codes, overwrite = TRUE)
