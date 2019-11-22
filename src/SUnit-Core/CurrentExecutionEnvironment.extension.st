Extension { #name : #CurrentExecutionEnvironment }

{ #category : #'*SUnit-Core' }
CurrentExecutionEnvironment class >> ifAll: testCases shareEnvironmentClassThenUseItDuring: aBlock [ 

	"when all the test cases share the same test execution environment class, create new instance
	of it and use it for all the tests. In other case, do not install any environment but execute 
	aBlock anyway."

	| anEnvironmentClass |

	(testCases notEmpty and: [ 
		anEnvironmentClass := testCases anyOne executionEnvironmentClass.
		testCases allSatisfy: [ :each | each executionEnvironmentClass = anEnvironmentClass ] ] )
		ifTrue: [ anEnvironmentClass new beActiveDuring: aBlock ]
		ifFalse: [ aBlock value ]
]

{ #category : #'*SUnit-Core' }
CurrentExecutionEnvironment class >> runTestCase: aTestCase [

	self value runTestCase: aTestCase

]

{ #category : #'*SUnit-Core' }
CurrentExecutionEnvironment class >> useTestingDuring: aBlock for: aTestCase [

	"if a test execution environment is already set, use it during execution of the block. Otherwise, create a new one for the given test case"
			
	self value isTest 
		ifTrue:  [ aBlock value ]
		ifFalse: [ 
			 | testEnv |
			testEnv := aTestCase executionEnvironmentClass new.
			testEnv beActiveDuring: aBlock ]
]
