# Authors: Raphael Bacher, UGA
# raphael.bacher@univ-grenoble-alpes.fr
#
#---------------------------------------------

#define input/output/ref/res from command line args (should in principle never be changed)
args <- commandArgs(TRUE)
is_run_local = length(args) == 0
if (is_run_local) {
  input = output = ref = res = "./"
} else {
  if (!exists("input"))  input = trimws(args[1]) #get args from command line and remove white spaces
  if (!exists("output")) output = trimws(args[2])
  if (!exists("ref"))    ref = "/ref/"
  if (!exists("res"))    res = "/res/"  
}
  
# load submited results from participant
e_LRRC1 = readRDS(paste0(input, res, "chlg1_modlin_res.rds"))

# read ref data (if on the server)
rds_filename = paste0(input, ref, "/chlg1_modlin_e_full.rds")
if (file.exists(rds_filename)) {
  exp_grp_full = readRDS(rds_filename)
} else {
  exp_grp_full = NULL
}

# define scoring function
score <- function(e_LRRC1, exp_grp_full, output_file){
  if (is.null(exp_grp_full)) {
    mse = nbNA = notNA = sdse = NA    
  } else {    
    lm1 <- lm(exp_grp_full$LRRC1~e_LRRC1[rownames(exp_grp_full)])
    res <- residuals(lm1)
    mse <- mean(res^2)
    nbNA = sum(is.na(e_LRRC1[rownames(exp_grp_full)]))
    notNA = sum(!is.na(e_LRRC1[rownames(exp_grp_full)]))
    sdse <- sd(res^2)
  }
  cat(sprintf("MSE:   %f\n", mse  ), file=output_file, append=FALSE)
  cat(sprintf("nbNA:  %f\n", nbNA ), file=output_file, append=TRUE)
  cat(sprintf("notNA:  %f\n", notNA ), file=output_file, append=TRUE)
  cat(sprintf("SDSE: %f\n", sdse), file=output_file, append=TRUE)
  cat(readLines(output_file), sep = "\n") 
}

# define output file (should in principle never be changed)
output_file = paste0(output,"/scores.txt")
score(e_LRRC1, exp_grp_full, output_file)

# export codallab bundle (if local run with reference data, it means if admin run)
if (file.exists(rds_filename) & is_run_local) {
  zip_filename = "reference_data.zip"
  zip(zip_filename, "chlg1_modlin_e_full.rds")

  zip_filename = "scoring_program.zip"
  zip(zip_filename, "scoring_program/metadata")
  zip(zip_filename, "scoring_program/scoring.r")    

  zip_filename = "starting_kit.zip"
  zip(zip_filename, "starting_kit.Rmd")
  zip(zip_filename, "chlg1_modlin_e.rds")
  zip(zip_filename, "chlg1_modlin_d.rds")
  zip(zip_filename, "scoring_program/metadata")
  zip(zip_filename, "scoring_program/scoring.r")
  zip(zip_filename, "scoring_program/scoring.r")
  zip(zip_filename, ".Rhistory")

  zip_filename = "./codalab_bundle.zip"
  zip(zip_filename, "competition.yaml")
  zip(zip_filename, "overview.html")
  zip(zip_filename, "get_starting_kit.html")
  zip(zip_filename, "logo.png")
  zip(zip_filename, "reference_data.zip")
  zip(zip_filename, "scoring_program.zip")
  zip(zip_filename, "starting_kit.zip")
  
  file.remove("reference_data.zip")
  file.remove("scoring_program.zip")
  file.remove("starting_kit.zip")    
}

# stop("EFN")

