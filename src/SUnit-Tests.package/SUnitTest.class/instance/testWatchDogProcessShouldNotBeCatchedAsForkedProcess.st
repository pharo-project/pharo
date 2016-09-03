testing
testWatchDogProcessShouldNotBeCatchedAsForkedProcess

	| env |
	env := CurrentExecutionEnvironment value.
	
	self assert: env forkedProcesses isEmpty