run_tests: run_tests_common run_tests_designers run_tests_inspect_helpers

run_tests_common:
	cd tests; rm .cache/*.RDS; echo "library(RUnit); runTestSuite(defineTestSuite('common', '.', 'runit_common.R'))" | R -q --vanilla; cd ..

run_tests_designers:
	cd tests; echo "library(RUnit); runTestSuite(defineTestSuite('designers', '.', 'runit_designers.R'))" | R -q --vanilla; cd ..

run_tests_inspect_helpers:
	cd tests; echo "library(RUnit); runTestSuite(defineTestSuite('inspect_helpers', '.', 'runit_inspect_helpers.R'))" | R -q --vanilla; cd ..

