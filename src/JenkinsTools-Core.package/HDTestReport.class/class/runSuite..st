running
runSuite: aTestSuite
	^ self new
		initializeOn: aTestSuite; 
		run;
		done