Extension { #name : #ExecutionEnvironment }

{ #category : #'*SUnit-Core' }
ExecutionEnvironment >> runTestCase: aTestCase [

	self subclassResponsibility 
]

{ #category : #'*SUnit-Core' }
ExecutionEnvironment >> runTestsBy: aBlock [

	self subclassResponsibility 
]
