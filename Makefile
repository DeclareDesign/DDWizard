run_tests:
	cd tests; rm .cache/*.RDS; echo "library(RUnit); runTestSuite(defineTestSuite('all', '.'))" | R -q --vanilla; cd ..

run_tests_designers:
	cd tests; echo "library(RUnit); runTestSuite(defineTestSuite('designers', '.', 'runit_designers.R'))" | R -q --vanilla; cd ..
