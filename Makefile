run_tests:
	cd tests; rm .cache/*.RDS; echo "library(RUnit); runTestSuite(defineTestSuite('all', '.'))" | R -q --vanilla; cd ..

