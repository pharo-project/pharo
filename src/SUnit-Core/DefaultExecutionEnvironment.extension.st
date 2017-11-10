Extension { #name : #DefaultExecutionEnvironment }

{ #category : #'*SUnit-Core' }
DefaultExecutionEnvironment >> runTestCase: aTestCase [

	| testEnv |
	testEnv := TestExecutionEnvironment new.
	
	testEnv beActiveDuring: [
		testEnv runTestCase: aTestCase]
]

{ #category : #'*SUnit-Core' }
DefaultExecutionEnvironment >> runTestsBy: aBlock [

	TestExecutionEnvironment new beActiveDuring: aBlock
]
