
library(RUnit)
library(RSAP)
 
test.suite <- defineTestSuite("example",
                              dirs = file.path("tests"),
                              testFileRegexp = '^\\d+\\.R')
 
test.result <- runTestSuite(test.suite)
 
printTextProtocol(test.result)

