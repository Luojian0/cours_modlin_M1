# Authors: Raphael Bacher, UGA
# raphael.bacher@univ-grenoble-alpes.fr
#
#---------------------------------------------

#define input/output from command line args (should in principle never be changed)
args <- commandArgs(TRUE)
if (length(args)==0) input = output = "./"
if (!exists("input")) input = trimws(args[1]) #get args from command line and remove white spaces
if (!exists("output")) output = trimws(args[2])

# load results data from participant, here one RData file with inside the variables Aest,Dest,exectime
e_LRRC1 = readRDS(paste0(input,"/res/chlg1_modlin_res.rds"))

# read ref data, here two rds files
if (file.exists("/ref/chlg1_modlin_e_full.rds")) {
  exp_grp_full = readRDS(paste0(input,"/ref/chlg1_modlin_e_full.rds"))
} else {
  exp_grp_full = NULL
}

# define scoring function
score <- function(e_LRRC1, exp_grp_full, output_file){
  if (is.null(exp_grp_full)) {
    mse = nbNA = sdse = NA    
  } else {    
    lm1 <- lm(exp_grp_full$LRRC1~e_LRRC1[rownames(exp_grp_full)])
    res <- residuals(lm1)
    mse <- mean(res^2)
    nbNA = sum(is.na(e_LRRC1[rownames(exp_grp_full)]))
    sdse <- sd(res^2)
  }
  cat(sprintf("MSE:   %f\n", mse  ), file=output_file, append=FALSE)
  cat(sprintf("nbNA:  %f\n", nbNA ), file=output_file, append=TRUE)
  cat(sprintf("SDSE: %f\n", sdse), file=output_file, append=TRUE)
  cat(readLines(output_file), sep = "\n") 
}

# define output file (should in principle never be changed)
output_file = paste0(output,"/scores.txt")
score(e_LRRC1, exp_grp_full, output_file)

