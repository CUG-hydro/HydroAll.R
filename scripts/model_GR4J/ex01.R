library(airGR)
library(magrittr)
library(data.table)
library(dplyr)

data(L0123001)
df = BasinObs %>% data.table() %>%
  rename(date = DatesR) %>%
  mutate(date = as.Date(date)) %>%
  .[, .(date, P, E, Robs = Qmm)]

r <- create_GR4J(df)
print(r)

# summary(BasinObs, digits = 2)

# need to rewrite the print method
# Inds_Run <- BasinObs[, which(date >= make_date(1990) & date <= make_date(1999, 12, 31))]


# str(InputsModel)
# str(InputsCrit)
# plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])
