*SUnit-Core
runTestCase: aTestCase

	| testEnv |
	testEnv := TestExecutionEnvironment new.
	
	testEnv beActiveDuring: [
		testEnv runTestCase: aTestCase]