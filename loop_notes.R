library(datasets)
data("iris")
?iris

species_factor <- factor(iris$Species)
species_factor

# Get the mean of sepal length for the species virginica.
iris$Sepal.Length
tapply(iris$Sepal.Length, species_factor, mean)

# BIG IDEA
# SQL GROUP BY implented in R using FACTORs

tapply(iris$Sepal.Length, iris$Species, mean)

apply(iris[, 1:4], 2, mean)

data("mtcars")
head(mtcars)

# Calculate mpg by cyl
#This does it
tapply(mtcars$mpg, mtcars$cyl, mean)

# This, too. Cool way not to have to keep qualifying the data frame for every variable.
with(mtcars, tapply(mpg, cyl, mean))

# This doesn't do it, but it pulls apart the data sets by the grouping column.
split(mtcars, mtcars$cyl)

# Therefore, this works. Think about it.
sapply(split(mtcars$mpg, mtcars$cyl), mean)

# This averages all the columns, but no grouping.
apply(mtcars, 2, mean)

# Absolute difference between average hp of 4-cyl and 8-cyl cars.
head(mtcars)

tapply(mtcars$hp, mtcars$cyl, mean)
split(mtcars$hp, mtcars$cyl)

split(mtcars$hp, mtcars$cyl)$"8"

mean(split(mtcars$hp, mtcars$cyl)$"8") - mean(split(mtcars$hp, mtcars$cyl)$"4") 
