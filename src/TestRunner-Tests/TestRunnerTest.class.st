Class {
	#name : #TestRunnerTest,
	#superclass : #TestCase,
	#instVars : [
		'runner'
	],
	#category : #'TestRunner-Tests-Core'
}

{ #category : #tests }
TestRunnerTest >> classForRunner [

	^ NewTestRunner
]

{ #category : #tests }
TestRunnerTest >> setUp [
	super setUp.
	runner := self classForRunner new suitePreparator: ShuffleSuitePreparator.
]

{ #category : #tests }
TestRunnerTest >> testDefaultSuitePreparatorOfTheRunnerIsNotNil [

	self assert: runner suitePreparator isNotNil
]

{ #category : #tests }
TestRunnerTest >> testFailedTestFaildWhileRerunningDoesNotChangeTheResult [

	"first execution"

	[ 
	YellowTest doNotIgnoreTest.
	runner runClass: YellowTest.
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
TestRunnerTest >> testGivingTestToTheRunner [

	"test that a runner has initialy any test to run.
	That ensure it runs only tests added by user.
	"

	runner testSuiteFromClass: ExampleSetTest.
	self assert: runner hasTestsToRun
]

{ #category : #tests }
TestRunnerTest >> testNewCreatedRunnerHasAnyTestsToRun [

	"test that a runner has initialy any test to run.
	that ensure when running a tests that the result contain
	only the tests added by user.
	"

	self deny: runner hasTestsToRun
]

{ #category : #tests }
TestRunnerTest >> testOnRunnerResultWithGreenTest [

	runner runClass: GreenTest.
	self
		assert: runner runCount equals: 2;
		assert: runner passedCount equals: 2;
		assert: runner failureCount equals: 0
]

{ #category : #tests }
TestRunnerTest >> testOnRunnerResultWithRedTest [

	[ 
	RedTest doNotIgnoreTest.
	runner runClass: RedTest.
	self
		assert: runner runCount equals: 3;
		assert: runner passedCount equals: 1;
		assert: runner failureCount equals: 1;
		assert: runner errorCount equals: 1 ] ensure: [ RedTest ignoreTest ]
]

{ #category : #tests }
TestRunnerTest >> testOnRunnerResultWithYellowTest [

	[ 
	YellowTest doNotIgnoreTest.
	runner runClass: YellowTest.
	self
		assert: runner testsCount equals: 2;
		assert: runner passedCount equals: 1;
		assert: runner failureCount equals: 1 ] ensure: [ 
		YellowTest ignoreTest ]
]

{ #category : #tests }
TestRunnerTest >> testRerunningFailedTest [

	"we will run a test who fail only during the first execution"

	"first execution"

	[ 
	FailThenPass doNotIgnoreTest.
	runner runClass: FailThenPass.
	self assert: runner runCount equals: 2.
	self assert: runner failureCount equals: 1.

	"second execution"
	runner runFailures.
	self assert: runner runCount equals: 2.
	self assert: runner failureCount equals: 0 ] ensure: [ 
		FailThenPass ignoreTest ]
]

{ #category : #tests }
TestRunnerTest >> testRerunningFailedTest2 [

	"we will run a test who fail only during the first execution"

	"first execution"

	[ 
	RedTest doNotIgnoreTest.
	runner runClass: RedTest.
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
TestRunnerTest >> testRunnerCanAccumulateResult [

	| firstRunningCount secondRunningCount |
	[ 
	YellowTest doNotIgnoreTest.
	runner accumulateResult.
	runner runClass: GreenTest. "2 tests"
	firstRunningCount := runner runCount.

	runner runClass: YellowTest. "2 tests"
	secondRunningCount := runner runCount.
	self assert: secondRunningCount equals: firstRunningCount + 2 ] 
		ensure: [ YellowTest ignoreTest ]
]

{ #category : #tests }
TestRunnerTest >> testRunnerDoesNotAccumulateRsult [

	| firstRunningCount secondRunningCount |
	firstRunningCount := (runner runClass: GreenTest) runCount.

	secondRunningCount := (runner runClass: GreenTest) runCount.
	self assert: secondRunningCount equals: firstRunningCount
]

{ #category : #tests }
TestRunnerTest >> testRunningACollectionOfTestCases [

	"first execution"

	[ 
	YellowTest doNotIgnoreTest.
	runner runAll: { 
			GreenTest.
			YellowTest }.
	self assert: runner runCount equals: 4.
	self assert: runner passedCount equals: 3 ] ensure: [ 
		YellowTest ignoreTest ]
]

{ #category : #tests }
TestRunnerTest >> testRunningACollectionOfTestCasesWithSomeAbstractClass [

	runner runAll: { 
			AthensCairoExportSurfaceTest.
			AthensCairoPDFSurfaceTest.
			AthensCairoSVGSurfaceTest }.

	self assert: runner runCount equals: 8
]

{ #category : #tests }
TestRunnerTest >> testRunningAnEmptyCollectionOfTestCases [

	"first execution"

	runner runAll: {  }.
	self assert: runner runCount equals: 0
]

{ #category : #tests }
TestRunnerTest >> testRunningFromAnEmptyCollectionOfPackages [

	"first execution"

	| packages |
	packages := OrderedCollection new.
	runner runPackages: packages.
	self assert: runner runCount equals: 0.
	self assert: runner passedCount equals: 0
]

{ #category : #tests }
TestRunnerTest >> testRunningTestClassSide [

	self
		assert: (self classForRunner runClass: GreenTest) passedCount
		equals: 2.

	self
		assert: (self classForRunner runClass: GreenTest) failureCount
		equals: 0
]

{ #category : #tests }
TestRunnerTest >> testSuiteCreation [

	self deny: runner hasTestsToRun.

	runner testSuiteFromClass: GreenTest.

	self assert: runner hasTestsToRun
]

{ #category : #tests }
TestRunnerTest >> testSuiteCreationFromAbstractClass [

	self deny: runner hasTestsToRun.

	runner testSuiteFromClass: AbstractEnumerationVisitorTest.

	self deny: runner hasTestsToRun
]

{ #category : #tests }
TestRunnerTest >> testSuiteCreationFromTestClassWithSubClasses [

	self deny: runner hasTestsToRun.

	runner testSuiteFromClass: AbstractEnumerationVisitorTest.

	self deny: runner hasTestsToRun
]

{ #category : #tests }
TestRunnerTest >> testSuiteWithMultipleTestCase [

	| exepectedNumberOfTest |
	self deny: runner hasTestsToRun.

	exepectedNumberOfTest := YellowTest suite tests size
	                         + GreenTest suite tests size
	                         + RedTest suite tests size.

	[ 
	YellowTest doNotIgnoreTest.
	RedTest doNotIgnoreTest.
	runner testSuitesFromClasses: { 
			GreenTest.
			YellowTest.
			RedTest }.
	self assert: runner suite tests size equals: exepectedNumberOfTest ] 
		ensure: [ 
			YellowTest ignoreTest.
			RedTest ignoreTest ]
]

{ #category : #tests }
TestRunnerTest >> testSuitesFromANonTestCaseIsEmpty [

	runner testSuiteFromClass: Integer.
	self deny: runner hasTestsToRun
]

{ #category : #tests }
TestRunnerTest >> testSuitesFromAnAbstractTestCaseIsEmpty [

	self classForRunner runClass: AthensCairoExportSurfaceTest.
	self deny: runner hasTestsToRun
]

{ #category : #tests }
TestRunnerTest >> testTryingToRunFailuresAfterRunningMultipleTestCaseClass [

	[ 
	FailThenPass reset.
	FailThenPass doNotIgnoreTest.
	YellowTest doNotIgnoreTest.

	runner
		accumulateResult;
		runAll: { 
				FailThenPass.
				YellowTest }.

	self assert: runner runCount equals: 4.
	self assert: runner failureCount equals: 2.

	runner runFailures.
	self assert: runner runCount equals: 4.
	self assert: runner failureCount equals: 1 ] ensure: [ 
		FailThenPass ignoreTest.
		YellowTest ignoreTest ]
]
