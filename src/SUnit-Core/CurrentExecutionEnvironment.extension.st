Extension { #name : #CurrentExecutionEnvironment }

{ #category : #'*SUnit-Core' }
CurrentExecutionEnvironment class >> runTestCase: aTestCase [

	self value runTestCase: aTestCase

]

{ #category : #'*SUnit-Core' }
CurrentExecutionEnvironment class >> runTestsBy: aBlock [

	self value runTestsBy: aBlock
]
