##--------------------------------------------------------------------------------------------##
# This script was made by Aldo Carmona-Baez
# Date created: Jan 27, 2022
# Last modified: Jan 29, 2022
##--------------------------------------------------------------------------------------------##

library(tidyr)
library(dplyr)
library(tibble)
library(ggplot2)
library(ComplexHeatmap)
library(circlize)


# read input data ---------------------------------------------------------

heatmap_data <- read.csv("earpit_deseq_transformed_top25_012322.csv")

# assign col names based on first column and transpose data
heatmap_data <- setNames(data.frame(t(heatmap_data[,-1])), heatmap_data[,1])[]

View(heatmap_data)
str(heatmap_data)


# Making data set for heat map --------------------------------------------
# Keep only the rows that have ASV abundance info. (Remove metadata rows)
heatmap_data_formatted <-
  heatmap_data %>% 
    rownames_to_column("var") %>% 
    filter(stringr::str_detect(var, 'ASV|X') ) %>%
    select(-var)

# Add row names to filtered data set and make numeric class for heat map
rownames(heatmap_data_formatted) <- rownames(heatmap_data)[grep('ASV|X', rownames(heatmap_data))]
heatmap_data_formatted <- mutate_all(heatmap_data_formatted, function(x) as.numeric(as.character(x))) %>% 
  as.matrix()
View(heatmap_data_formatted)
colnames(heatmap_data_formatted)
tail(heatmap_data_formatted)


# Making data set for metadata --------------------------------------------
# Keep only the rows that have ASV abundance info. (Remove metadata rows)
metadata <-
heatmap_data %>%
  rownames_to_column("var") %>% 
  filter(stringr::str_detect(var, 'ASV|X', negate = TRUE)) %>% 
  column_to_rownames(var = "var") %>% 
  t() %>% 
  as.data.frame() %>%
  rownames_to_column(var = "Samples") %>% 
  select(#Samples,
         ABCC11.Genotype,
         ABCC11.Phenotype,
         deodorant.antiperspirant.binned,
         Site)

View(metadata)
head(metadata)
class(metadata)
str(metadata)

# create heat map ----------------------------------------------------------

View(cbind(colnames(heatmap_data_formatted), metadata$Samples))

column_ha = HeatmapAnnotation(df = metadata,
                              col = list(ABCC11.Genotype = c("AA" = "#E66100", "GA" = "#5D3A9B", "GG" = "#E1BE6A"),
                                         ABCC11.Phenotype = c("Dry" = "#FFC20A", "Wet" = "#0C7BDC"),
                                         deodorant.antiperspirant.binned = c("deoderant.antiperspirant" = "#117733",
                                                                             "deodorant.only" = "#44AA99",
                                                                             "no.product" = "#AA4499"),
                                         Site = c("Ear" = "#88CCEE",
                                                  "Pit" = "#DDCC77"))
                              )

Heatmap(heatmap_data_formatted,
        column_names_gp = grid::gpar(fontsize = 8),
        row_names_gp = grid::gpar(fontsize = 8),
        top_annotation = column_ha,
        cluster_columns = TRUE)


ha = HeatmapAnnotation(
  foo = c(1:4, NA, 6:10), 
  bar = c(NA, sample(letters[1:3], 9, replace = TRUE)),
  col = list(foo = col_fun,
             bar = c("a" = "red", "b" = "green", "c" = "blue")
  ),
  na_col = "black"
)