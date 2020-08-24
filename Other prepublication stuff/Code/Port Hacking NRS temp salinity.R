# Port Hacking NRS Temp/Salinity

library(tidyverse)
library(tidync)

filename <- "Data/PH Temp_Sal/IMOS_ANMN-NSW_CDSTZ_20160828T190001Z_PH100_FV01_PH100-1609-SBE37SM-RS232-104_END-20161202T013501Z_C-20170117T044031Z.nc"

subs <- tidync(filename) %>% hyper_filter(PSAL_quality_control = PSAL_quality_control != -999)
