##############################
## Kjøring på mobilt kontor ##
##############################


source("dev/sysSetenv.R")
intensivberedskap::kjor_appNIRbered(browser = TRUE, logAsJson = FALSE)

rm('RegData')

devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)



nordicscir::kjor_NSapper(register='nordicscir', browser = TRUE)
