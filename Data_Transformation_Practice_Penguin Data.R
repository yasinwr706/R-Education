# The goal of this script is to provide practice examples for data transformation in R.
# We will use Penguins Data set. (These questions or practice works are based on Chatgpt)
#loading the data set
# Install and load the package if necessary
if (!require("palmerpenguins")) {
  install.packages("palmerpenguins")
  library(palmerpenguins)
}
glimpse(penguins)

#Row-Wise Operations
# Filter the dataset to include only penguins of the species "Adelie."
penguins |> 
  filter(species== 'Adelie')

## Out of 344 rows 152 were selected.

#Find all penguins with a bill length greater than 40 mm 
#and a body mass less than 4000 g.

penguins |> 
  filter(bill_length_mm>40 & body_mass_g<4000)

## Out of 344 76 rows were selected.

#Extract rows for penguins on the islands 
#"Torgersen" or "Biscoe."

penguins |> 
  filter(island == 'Torgersen'| island == 'Biscoe' )

#Out of 344, 210 rows were selected.

#Identify all rows where sex is missing (NA).

penguins |> 
  filter(is.na(sex))

penguins |> 
  filter(is.na(species))

#Arrange the dataset by body mass in descending order.
penguins |> 
  arrange(desc(body_mass_g))


## Column-Wise Operations

#Add a new column called bill_ratio that calculates the ratio of 
#bill_length_mm to bill_depth_mm.

penguins |> 
  mutate(bill_ratio= (bill_length_mm/bill_depth_mm), .before=species)

# #Create a new column called size_category that categorizes penguins as 
# "small" or "large" based on their body mass 
# (e.g., small if body mass is <3500 g, otherwise large).

penguins |> 
  mutate(size_category= ifelse(body_mass_g<3500,'small', 'large'))

# Select only the columns related to 
# measurements (bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g).

penguins |> 
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)


#Rename the column species to penguin_species.
penguins |> 
  rename(penguin_species= species)

#Relocate the island column to the first position in the dataset.

penguins |> 
  relocate(island, .before = 1)

# Group-Wise Operations

#Group the dataset by species and calculate the 
#average body mass for each species.

penguins |> 
  group_by(species) |> 
  summarize(average=(mean(body_mass_g, na.rm=TRUE)))

#Find the maximum flipper_length_mm for each combination 
#of species and island.
penguins |> 
  group_by(species, sex) |> 
  summarize(max= max(flipper_length_mm))

#Count the number of penguins in each species and sex group.

penguins |> 
  group_by(species, sex) |> 
  summarize(n=n())

#Identify the heaviest penguin in each species.
penguins |> 
  group_by(species) |> 
  summarize(max= max(body_mass_g, na.rm=TRUE))

#Summarize the dataset to show the mean and standard deviation 
#of bill_length_mm for each island.

penguins |> 
  group_by(island) |> 
  summarise(mean= mean(bill_length_mm, na.rm=TRUE), 
            sd= sd(bill_length_mm, na.rm=TRUE))

## Combining Transformations

#Filter the dataset to include only penguins with complete 
#data (no missing values), then calculate the mean body 
#mass for each species.

penguins |> 
  drop_na() |> 
  group_by(species) |> 
  summarize(mean= mean(body_mass_g))

##Create a new column that scales body_mass_g (subtract the mean and divide by the standard deviation),
#then arrange the dataset by this scaled value.
penguins |> 
  mutate(scales= ((body_mass_g-mean(body_mass_g, na.rm=TRUE))/sd(body_mass_g, na.rm=TRUE)
                  )) |> 
  arrange(scales)

#For each species, find the smallest bill_length_mm 
#and return the associated rows with other columns.
penguins |> 
  group_by(species) |> 
  slice_min(bill_length_mm)

## Use of Slice Function
# slice_head(): Select the first 3 rows of each species
penguins |> 
  group_by(species) |> 
  slice_head(n=3)

#slice_tail(): Select the last 2 rows of each species
penguins |> 
  group_by(species) |> 
  slice_tail(n = 2)  # Select last 2 rows for each species

#slice_min(): Select the row with the smallest bill_depth_mm for each species

penguins |> 
  group_by(species) |> 
  slice_min(bill_depth_mm, n=1)

#slice_max(): Select the row with the largest flipper_length_mm for each species

penguins |> 
  group_by(species) |> 
  slice_max(flipper_length_mm, n = 1)  # Row with the largest flipper_length_mm for each species

#Practice Problems:
#Problem 1: For each species, find the row with the largest bill_length_mm.
penguins |> 
  group_by(species) |> 
  slice_max(bill_length_mm)

#Problem 2: For each species, find the first 3 penguins based on their bill_depth_mm.

penguins |> 
  group_by(species) |> 
  slice_max(bill_depth_mm, n=3)

#Problem 3: Find the penguin with the highest body_mass_g for each species.

penguins |> 
  group_by(species) |> 
  slice_max(body_mass_g, n=1)

#Problem 4: For each species, find the row with the smallest 
#flipper_length_mm and display all the columns for that row.

penguins |> 
  group_by(species) |> 
  slice_min(flipper_length_mm, n=1)

##Problem 5: For each species, find the last penguin recorded by date 
#(if date is available in the dataset, or alternatively use island or another factor).
  penguins |> 
    group_by(species) |> 
    slice_tail(n=1)
  
  #Problem 6: For each species, find the penguin with the median body_mass_g.
  penguins |> 
    group_by(species) |> 
    filter(body_mass_g== median(body_mass_g, na.rm=TRUE))