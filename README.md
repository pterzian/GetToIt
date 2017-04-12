# GetToIt

Well for now it actually does one thing, which is good on a scale of 0 to 1, but actually poor if you think of it.

## Installation

You can install GetToIt from github with:

```R
# install.packages("devtools")
devtools::install_github("NA/NA")
```

## Example

Let's create a list of list with magic, then making a dataframe out of it, to plot each list as multiple boxplot

```R
#build the list
magic <- list(list(c(rnorm(20,2,1)), c(rnorm(8,8,1))),
              list(c(rnorm(20,3,1)), c(rnorm(8,7,1))),    
              list(c(rnorm(20,4,1)), c(rnorm(8,6,1)))) 

#give names to the plots
names(magic) <- c(1:length(magic))

#and pick an order
order= c(1,3,2)

p <- ggBoxPlot(magic, order)

...
```
