testing
testExecutionEnvironment

	| newEnv |
	newEnv := ExecutionEnvironmentStub new.
	newEnv beActiveDuring: [  
		self assert: CurrentExecutionEnvironment value equals: newEnv.	
	
		DefaultExecutionEnvironment beActiveDuring: [ 
			self assert: CurrentExecutionEnvironment value equals: DefaultExecutionEnvironment instance].
	
		self assert: CurrentExecutionEnvironment value equals: newEnv.		
	]