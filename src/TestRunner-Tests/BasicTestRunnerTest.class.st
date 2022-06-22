Class {
	#name : #BasicTestRunnerTest,
	#superclass : #TestCase,
	#instVars : [
		'runner'
	],
	#category : #'TestRunner-Tests-Core'
}

{ #category : #tests }
BasicTestRunnerTest >> classForRunner [

	^ BasicTestRunner
]

{ #category : #running }
BasicTestRunnerTest >> setUp [

	super setUp.
	runner := self classForRunner new
]

{ #category : #tests }
BasicTestRunnerTest >> testDefaultSuitePreparatorOfTheRunnerIsNotNil [

	self assert: runner suitePreparator isNotNil
]

{ #category : #tests }
BasicTestRunnerTest >> testFailedTestFaildWhileRerunningDoesNotChangeTheResult [

	"first execution"

	[ 
	YellowTest doNotIgnoreTest.
	runner run: YellowTest.
	self
		assert: runner testsCount equals: 2;
		assert: runner failureCount equals: 1.

	"second execution"
	runner runFailures.
	self
		assert: runner testsCount equals: 2;
		assert: runner failureCount equals: 1 ] ensure: [ 
		YellowTest ignoreTest ]
]

{ #category : #tests }
BasicTestRunnerTest >> testGivingTestToTheRunner [

	"test that a runner has initialy any test to run.
	That ensure it runs only tests added by user.
	"

	runner testSuiteFromClass: ExampleSetTest.
	self assert: runner hasTestsToRun
]

{ #category : #tests }
BasicTestRunnerTest >> testNewCreatedRunnerHasAnyTestsToRun [

	"test that a runner has initialy any test to run.
	that ensure when running a tests that the result contain
	only the tests added by user.
	"

	self deny: runner hasTestsToRun
]

{ #category : #tests }
BasicTestRunnerTest >> testOnRunnerResultWithGreenTest [

	runner run: GreenTest.
	self
		assert: runner runCount equals: 2;
		assert: runner passedCount equals: 2;
		assert: runner failureCount equals: 0
]

{ #category : #tests }
BasicTestRunnerTest >> testOnRunnerResultWithRedTest [

	[ 
	RedTest doNotIgnoreTest.
	runner run: RedTest.
	self
		assert: runner runCount equals: 3;
		assert: runner passedCount equals: 1;
		assert: runner failureCount equals: 1;
		assert: runner errorCount equals: 1 ] ensure: [ RedTest ignoreTest ]
]

{ #category : #tests }
BasicTestRunnerTest >> testOnRunnerResultWithYellowTest [

	[ 
	YellowTest doNotIgnoreTest.
	runner run: YellowTest.
	self
		assert: runner testsCount equals: 2;
		assert: runner passedCount equals: 1;
		assert: runner failureCount equals: 1 ] ensure: [ 
		YellowTest ignoreTest ]
]

{ #category : #tests }
BasicTestRunnerTest >> testRerunningFailedTest [

	"we will run a test who fail only during the first execution"

	"first execution"

	[ 
	FailThenPass doNotIgnoreTest.
	runner run: FailThenPass.
	self assert: runner runCount equals: 2.
	self assert: runner failureCount equals: 1.

	"second execution"
	runner runFailures.
	self assert: runner runCount equals: 2.
	self assert: runner failureCount equals: 0 ] ensure: [ 
		FailThenPass ignoreTest ]
]

{ #category : #tests }
BasicTestRunnerTest >> testRerunningFailedTest2 [

	"we will run a test who fail only during the first execution"

	"first execution"

	[ 
	RedTest doNotIgnoreTest.
	runner run: RedTest.
	self
		assert: runner runCount equals: 3;
		assert: runner failureCount equals: 1;
		assert: runner errorCount equals: 1.

	"second execution"
	runner runFailures.
	runner result.
	self
		assert: runner runCount equals: 3;
		assert: runner failureCount equals: 1;
		assert: runner errorCount equals: 1 ] ensure: [ RedTest ignoreTest ]
]

{ #category : #tests }
BasicTestRunnerTest >> testRunnerDoesNotAccumulateRsult [

	| firstRunningCount secondRunningCount |
	firstRunningCount := (runner run: GreenTest) runCount.

	secondRunningCount := (runner run: GreenTest) runCount.
	self assert: secondRunningCount equals: firstRunningCount
]

{ #category : #tests }
BasicTestRunnerTest >> testRunningTestClassSide [

	self
		assert: (self classForRunner run: GreenTest) passedCount
		equals: 2.

	self
		assert: (self classForRunner run: GreenTest) failureCount
		equals: 0
]

{ #category : #tests }
BasicTestRunnerTest >> testSuitesFromANonTestCaseIsEmpty [

	runner testSuiteFromClass: Integer.
	self deny: runner hasTestsToRun
]

{ #category : #tests }
BasicTestRunnerTest >> testSuitesFromAnAbstractTestCaseIsEmpty [

	self classForRunner run: AthensCairoExportSurfaceTest.
	self deny: runner hasTestsToRun
]
