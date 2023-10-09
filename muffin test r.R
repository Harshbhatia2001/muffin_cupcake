# set up so that all variables of tibbles are printed
options(dplyr.width = Inf)


# load useful libraries
library(tidyverse)

library(recipes) # could also load the tidymodels package

# load in the data
muffin_cupcake_data_orig <- read_csv("C:/Users/bhati_c1lruuv/recipes_muffins_cupcakes.csv")

# look at data
muffin_cupcake_data_orig

muffin_cupcake_data <- muffin_cupcake_data_orig %>%
  
  # rename all columns 
  rename_all(function(.name) {
    .name %>%  
      
      # replace all names with the lowercase versions
      tolower %>%
      # replace all spaces with underscores
      str_replace(" ", "_")
  })

# check that this did what I wanted
muffin_cupcake_data

# add an additional ingredients column that is categorical
muffin_cupcake_data <- muffin_cupcake_data %>%
  mutate(additional_ingredients = c("fruit", 
                                    "fruit", 
                                    "none", 
                                    "nuts", 
                                    "fruit", 
                                    "fruit", 
                                    "nuts", 
                                    "none", 
                                    "none", 
                                    "nuts",
                                    "icing",
                                    "icing",
                                    "fruit",
                                    "none",
                                    "fruit",
                                    "icing",
                                    "none",
                                    "fruit",
                                    "icing",
                                    "icing"))
# add some random missing values here and there just for fun
set.seed(26738)
muffin_cupcake_data <- muffin_cupcake_data %>%
  
  # only add missing values to numeric columns
  mutate_if(is.numeric,
            function(x) {
              # randomly decide if 0, 2, or 3 values will be missing from each column
              n_missing <- sample(0:3, 1)
              # replace n_missing randomly selected values from each column with NA
              x[sample(1:20, n_missing)] <- NA
              return(x)
            })
muffin_cupcake_data

library(rsample)
muffin_cupcake_split <- initial_split(muffin_cupcake_data)
muffin_cupcake_train <- training(muffin_cupcake_split)
muffin_cupcake_test <- testing(muffin_cupcake_split)
rm(muffin_cupcake_data)

muffin_cupcake_train

muffin_cupcake_test

# define the recipe (it looks a lot like applying the lm function)
model_recipe <- recipe(type ~ flour + milk + sugar + butter + egg + 
                         baking_powder + vanilla + salt + additional_ingredients, 
                       data = muffin_cupcake_train)
  
summary(model_recipe)

# define the steps we want to apply
model_recipe_steps <- model_recipe %>% 
  # mean impute numeric variables
  step_impute_mean(all_numeric()) %>%
  # convert the additional ingredients variable to dummy variables
  step_dummy(additional_ingredients) %>%
  # rescale all numeric variables except for vanilla, salt and baking powder to lie between 0 and 1
  step_range(all_numeric(), min = 0, max = 1, -vanilla, -salt, -baking_powder) %>%
  # remove predictor variables that are almost the same for every entry
  step_nzv(all_predictors()) 

model_recipe_steps

prepped_recipe <- prep(model_recipe_steps, training = muffin_cupcake_train)

prepped_recipe

muffin_cupcake_train_preprocessed <- bake(prepped_recipe, muffin_cupcake_train) 
muffin_cupcake_train_preprocessed

muffin_cupcake_test_preprocessed <- bake(prepped_recipe, muffin_cupcake_test)
muffin_cupcake_test_preprocessed

# End of this activity 
