## RUnit is a unit testing package for R

##Uncomment the following line to install it
# install.packages('RUnit')
library('RUnit')
 
## We want to test the cacheMatrix file
source('cacheMatrix.R')
 
## Our test suite consists of all files found in the tests directory
test.suite <- defineTestSuite("caching",
                              dirs = file.path("tests"),
                              testFileRegexp = '^\\d+\\.R')
 
## Run the tests
test.result <- runTestSuite(test.suite)
 
## And print the results
printTextProtocol(test.result)

