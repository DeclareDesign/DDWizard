run_tests:
	cd tests; echo "library(RUnit); runTestSuite(defineTestSuite('all', '.'))" | R -q --vanilla; cd ..

