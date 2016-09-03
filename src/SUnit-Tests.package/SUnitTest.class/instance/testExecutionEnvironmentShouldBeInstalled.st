testing
testExecutionEnvironmentShouldBeInstalled

	| env |
	env := CurrentExecutionEnvironment value.

	self assert: env class equals: TestExecutionEnvironment.
	self assert: env testCase equals: self