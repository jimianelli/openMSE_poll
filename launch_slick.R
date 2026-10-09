# Launch Slick App for EBS Pollock MSE Results
# Run this script in RStudio (interactive session required)

library(Slick)

# Load the Slick object
slick_obj <- readRDS("EBS_pollock_slick.rds")

# Check validity
cat("Checking Slick object...\n")
Check(slick_obj)

# Launch the app
cat("\nLaunching Slick App...\n")
App(slick_obj)
