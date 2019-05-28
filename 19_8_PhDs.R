library(readr)
library(dplyr)

phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")


# Graph comparing fields for my wife and me

fieldchoices <- c("Statistics (mathematics)")
ourfields <- filter(phd_field, 
                    field %in% c("Microbiology", "Statistics (mathematics)"))
ggplot(ourfields, aes(x = year, y = n_phds, color = field)) +
  geom_line()



# Comparisons of broad categories of degree

sumfields <- aggregate(n_phds ~ broad_field + year, 
                       data = phd_field, FUN = "sum")
ggplot(sumfields, aes(x = year, y = n_phds, color = broad_field)) +
  geom_line()
