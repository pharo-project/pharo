Class {
	#name : #NewCommandLineRunnerTest,
	#superclass : #NewTestRunnerTest,
	#category : #'TestRunner-Tests-Core'
}

{ #category : #tests }
NewCommandLineRunnerTest >> ignoredSelectors [

	^ #( #testFailedTestFaildWhileRerunningDoesNotChangeTheResult
	     NewCommandLineRunnerTest #( #NewTestRunnerTest ) >> #testRerunningFailedTest
	     NewCommandLineRunnerTest #( #NewTestRunnerTest ) >> #testRerunningFailedTest2
	     NewCommandLineRunnerTest #( #NewTestRunnerTest ) >> #testTryingToRunFailuresAfterRunningMultipleTestCaseClass )
]

{ #category : #running }
NewCommandLineRunnerTest >> setUp [

	super setUp.
	runner := NewCommandLineRunner new
]

{ #category : #tests }
NewCommandLineRunnerTest >> testReporterDatasShouldBeResetBeetwenTwoExecution [

	runner reporter: XmlReporter new.
	runner run: GreenTest.
	self assert: runner reporter datasAreReseted
]

{ #category : #tests }
NewCommandLineRunnerTest >> testRunnerHasDefaultReporter [

	runner hasDefaultReporter
]
