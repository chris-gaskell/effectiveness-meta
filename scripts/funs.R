# Functions ####
## Classifiers
### Cohen's d (Guidelines are .2 = Small,  .5 = Medium,  .8 = Large) ####
CohenClass <- function(Cohen)
{if(Cohen < .2)
{print("no effect")
}else if(Cohen >= .2 & Cohen < .5)
{print("small")} else if(Cohen >= .5 & Cohen < .8)
{print("medium")} else {print('large')}
}
### I2 (Guidelines are .25 = low, .5 = moderate, .75 = substantial [Higgins et al., 2003]) ####
HeteroClass <- function(I2)
{if(I2 < .25)
{print("no")
}else if(I2 >= .25 & I2 < .5)
{print("low")} else if(I2 >= .5 & I2 < .75)
{print("moderate")} else {print('high')}
}
