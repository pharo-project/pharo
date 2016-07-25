actions
runTestSuites: testSuites 
	testSuites
		do: [:testSuite | self runSuite: testSuite]
		displayingProgress: 'Running Tests'