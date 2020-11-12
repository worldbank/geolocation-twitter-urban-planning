# Download GADM

setwd(file.path(gadm_dir, "data"))
ken_adm_1 <- getData('GADM', country='KEN', level=1)

