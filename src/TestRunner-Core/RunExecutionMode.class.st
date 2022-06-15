"
I ececute a test suite then return its TestResult.
"
Class {
	#name : #RunExecutionMode,
	#superclass : #AbstractExecutionMode,
	#category : #'TestRunner-Core-Configuration'
}

{ #category : #execution }
RunExecutionMode class >> executeCase: aTestCase [

	| aResult |
	aResult := TestResult new.
	[ 
	[ 
	aTestCase announce: TestCaseStarted withResult: aResult.
	CurrentExecutionEnvironment runTestCase: aTestCase.
	aTestCase announce: TestCaseEnded withResult: aResult.
	aResult addPass: aTestCase ]
		on:
			aResult class failure , aResult class skip , aResult class warning
			, aResult class error
		do: [ :ex | ex sunitAnnounce: aTestCase toResult: aResult ] ] 
		ensure: [ 
		aTestCase classForTestResource resetResources: aTestCase resources ].
	^ aResult
]

{ #category : #execution }
RunExecutionMode class >> executeSuite: aTestSuite [

	^ aTestSuite run
]
