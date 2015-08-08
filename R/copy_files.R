#### copy files ####

# I keep all RMD files in seperate folder structures and here 
# all the files I want to publish are first copied to the notes folder

# copy selected HTML files info notes folder using relatve path to 
# location of HTML file

html.files <- 
c("skalierung_thurstone/scaling_thurstone.html", 
  "cross_validation/cross_validation.html",
  "strategic_consensus_mapping_sray/strategic_consensus_mapping_sray.html",
  "regression_custom_hypotheses/regression_custom_hypotheses.html", 
  "qq_plot_by_hand/qq_plot_by_hand.html", 
  "multiple_factor_analysis/rmd/multiple_factor_analysis.html",
  "ctt_spearman_brown/ctt_spearman_brown.html",
  "regression_partial/regression_partiell.html",
  "svd_image_compression/rmd/svd_image_compression.html")


base <- basename(html.files)
#to <- file.path("notes", base)
to <- base
if (! all(file.exists(html.files)))
    warning("Some files do not exist")
res <- file.copy(html.files, to, overwrite = TRUE)
if (!all(res))
  warning("Some files were not copied")
  



