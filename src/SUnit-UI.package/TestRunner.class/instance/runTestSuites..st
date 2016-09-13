actions
runTestSuites: testSuites 
	CurrentExecutionEnvironment runTestsBy: [
		testSuites
			do: [:testSuite | self runSuite: testSuite]
			displayingProgress: 'Running Tests'
	]