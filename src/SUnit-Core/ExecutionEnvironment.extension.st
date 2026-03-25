Extension { #name : 'ExecutionEnvironment' }

{ #category : '*SUnit-Core' }
ExecutionEnvironment >> maxTimeForTest: aDuration [
	"just for compatibility with TestExecutionEnvironment"

	
]

{ #category : '*SUnit-Core' }
ExecutionEnvironment >> runTestCase: aTestCase [

	self subclassResponsibility
]

{ #category : '*SUnit-Core' }
ExecutionEnvironment >> runTestsBy: aBlock [

	self subclassResponsibility
]
