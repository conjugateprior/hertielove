# hertielove

You have a class full of terrible grades, scored out of 100 - maybe they are 
percentages. You want to curve them to fit Hertie's expectations about 
grade distributions using a smooth transformation that maintains monotonicity, 
and isn't especially ad-hoc. In particular you'd like not to have to threshold or 
make individual grade changes. We call this the #HertieLove.

This package applies #HertieLove a class of grades. It works by taking the 
raw grades as ability estimates from a 2PL IRT model, and then figuring out 
what values of the 2PL's parameters would generate the appropriate 
grade quantiles and applies those to the original score estimates. This 
curving function is also called `hertielove`.

The package also has a utility function that transforms grades out of 100 onto 
'German' grades, which start at 1 (the best) and move in numerically uneven
but conventional intervals to 5 (the worst). This function is called `degrade`
since it generates grades suitable for DE.

## Installation

```
remotes::install_github("conjugateprior/hertielove")
```
(you may have to install the `remotes` package first)

## Usage

```
grades <- rbinom(25, prob = 0.66, size = 100) # fake some grades
loved <- hertielove(grades) # curved grades

deloved <- degrade(loved) # German grades for those curved grades
```
