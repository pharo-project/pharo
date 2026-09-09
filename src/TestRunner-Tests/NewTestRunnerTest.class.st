Class {
	#name : 'NewTestRunnerTest',
	#superclass : 'TestCase',
	#instVars : [
		'runner'
	],
	#category : 'TestRunner-Tests-Core',
	#package : 'TestRunner-Tests',
	#tag : 'Core'
}

{ #category : 'tests' }
NewTestRunnerTest >> classForRunner [

	^ NewTestRunner
]

{ #category : 'running' }
NewTestRunnerTest >> setUp [

	super setUp.
	runner := self classForRunner new.
	MockTest doNotIgnore
]

{ #category : 'running' }
NewTestRunnerTest >> tearDown [

	MockTest ignore.
	super tearDown 
]

{ #category : 'tests' }
NewTestRunnerTest >> testDefaultSuitePreparatorOfTheRunnerIsNotNil [

	self assert: runner suitePreparator isNotNil
]

{ #category : 'tests' }
NewTestRunnerTest >> testFailedTestFaildWhileRerunningDoesNotChangeTheResult [

	"first execution"

	runner runClass: YellowTest.
	self
		assert: runner testsCount equals: 2;
		assert: runner failureCount equals: 1.

	"second execution"
	runner runFailures.
	self
		assert: runner testsCount equals: 2;
		assert: runner failureCount equals: 1 
]

{ #category : 'tests' }
NewTestRunnerTest >> testGivingTestToTheRunner [

	"test that a runner has initialy any test to run.
	That ensure it runs only tests added by user.
	"

	runner testSuiteFromClass: ExampleSetTest.
	self assert: runner hasTestsToRun
]

{ #category : 'tests' }
NewTestRunnerTest >> testNewCreatedRunnerHasAnyTestsToRun [

	"test that a runner has initialy any test to run.
	that ensure when running a tests that the result contain
	only the tests added by user.
	"

	self deny: runner hasTestsToRun
]

{ #category : 'tests' }
NewTestRunnerTest >> testOnRunnerResultWithGreenTest [

	runner runClass: GreenTest.
	self
		assert: runner runCount equals: 2;
		assert: runner passedCount equals: 2;
		assert: runner failureCount equals: 0
]

{ #category : 'tests' }
NewTestRunnerTest >> testOnRunnerResultWithRedTest [

	runner runClass: RedTest.
	self
		assert: runner runCount equals: 3;
		assert: runner passedCount equals: 1;
		assert: runner failureCount equals: 1;
		assert: runner errorCount equals: 1
]

{ #category : 'tests' }
NewTestRunnerTest >> testOnRunnerResultWithYellowTest [

	runner runClass: YellowTest.
	self
		assert: runner testsCount equals: 2;
		assert: runner passedCount equals: 1;
		assert: runner failureCount equals: 1 
]

{ #category : 'tests' }
NewTestRunnerTest >> testRerunningFailedTest [

	"we will run a test who fail only during the first execution"

	"first execution"
	runner runClass: FailThenPassTest.
	self assert: runner runCount equals: 2.
	self assert: runner failureCount equals: 1.

	"second execution"
	runner runFailures.
	self assert: runner runCount equals: 2.
	self assert: runner failureCount equals: 0
]

{ #category : 'tests' }
NewTestRunnerTest >> testRerunningFailedTest2 [

	"we will run a test who fail only during the first execution"

	"first execution"
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
		assert: runner errorCount equals: 1
]

{ #category : 'tests' }
NewTestRunnerTest >> testRunCasesAccumulatesResults [

	runner 
		accumulateResult;
		runCase: GreenTest suite tests first;
		runCase: GreenTest suite tests second.

	self assert: runner runCount equals: 2
]

{ #category : 'tests' }
NewTestRunnerTest >> testRunCasesDontAccumulatesResults [

	| firstRunningCount |
	firstRunningCount := runner 
		doNotAccumulateResult;
		runCase: GreenTest suite tests first;
		runCase: GreenTest suite tests second.

	self 
		assert: runner runCount equals: 0;
		assert: firstRunningCount passedCount equals: 1
]

{ #category : 'tests' }
NewTestRunnerTest >> testRunCountOnNew [

	self assert: runner runCount equals: 0
]

{ #category : 'tests' }
NewTestRunnerTest >> testRunnerCanAccumulateResult [

	| firstRunningCount secondRunningCount |
	runner accumulateResult.
	runner runClass: GreenTest. "2 tests"
	firstRunningCount := runner runCount.

	runner runClass: YellowTest. "2 tests"
	secondRunningCount := runner runCount.
	self assert: secondRunningCount equals: firstRunningCount + 2
]

{ #category : 'tests' }
NewTestRunnerTest >> testRunnerDoesNotAccumulateRsult [

	| firstRunningCount secondRunningCount |
	firstRunningCount := (runner runClass: GreenTest) runCount.

	secondRunningCount := (runner runClass: GreenTest) runCount.
	self assert: secondRunningCount equals: firstRunningCount
]

{ #category : 'tests' }
NewTestRunnerTest >> testRunningACollectionOfTestCases [

	"first execution"
	runner runClasses: { 
			GreenTest.
			YellowTest }.
	self assert: runner runCount equals: 4.
	self assert: runner passedCount equals: 3
]

{ #category : 'tests' }
NewTestRunnerTest >> testRunningACollectionOfTestCasesWithSomeAbstractClass [

	runner runClasses: { 
			AthensCairoExportSurfaceTest.
			AthensCairoPDFSurfaceTest.
			AthensCairoSVGSurfaceTest }.

	self assert: runner runCount equals: 8
]

{ #category : 'tests' }
NewTestRunnerTest >> testRunningAnEmptyCollectionOfTestCases [

	"first execution"

	runner runClasses: {  }.
	self assert: runner runCount equals: 0
]

{ #category : 'tests' }
NewTestRunnerTest >> testRunningFromAnEmptyCollectionOfPackages [

	"first execution"

	| packages |
	packages := OrderedCollection new.
	runner runPackages: packages.
	self assert: runner runCount equals: 0.
	self assert: runner passedCount equals: 0
]

{ #category : 'tests' }
NewTestRunnerTest >> testRunningTestClassSide [

	self
		assert: (self classForRunner runClass: GreenTest) passedCount
		equals: 2.

	self
		assert: (self classForRunner runClass: GreenTest) failureCount
		equals: 0
]

{ #category : 'tests' }
NewTestRunnerTest >> testSuiteCreation [

	self deny: runner hasTestsToRun.

	runner testSuiteFromClass: GreenTest.

	self assert: runner hasTestsToRun
]

{ #category : 'tests' }
NewTestRunnerTest >> testSuiteCreationFromAbstractClass [

	self deny: runner hasTestsToRun.

	runner testSuiteFromClass: AbstractEnumerationVisitorTest.

	self deny: runner hasTestsToRun
]

{ #category : 'tests' }
NewTestRunnerTest >> testSuiteCreationFromTestClassWithSubClasses [

	self deny: runner hasTestsToRun.

	runner testSuiteFromClass: AbstractEnumerationVisitorTest.

	self deny: runner hasTestsToRun
]

{ #category : 'tests' }
NewTestRunnerTest >> testSuiteWithMultipleTestCase [

	| exepectedNumberOfTest |
	self deny: runner hasTestsToRun.

	exepectedNumberOfTest := YellowTest suite tests size
	                         + GreenTest suite tests size
	                         + RedTest suite tests size.

	runner testSuitesFromClasses: { 
			GreenTest.
			YellowTest.
			RedTest }.
	self assert: runner suite tests size equals: exepectedNumberOfTest
]

{ #category : 'tests' }
NewTestRunnerTest >> testSuitesFromANonTestCaseIsEmpty [

	runner testSuiteFromClass: Integer.
	self deny: runner hasTestsToRun
]

{ #category : 'tests' }
NewTestRunnerTest >> testSuitesFromAnAbstractTestCaseIsEmpty [

	self classForRunner runClass: AthensCairoExportSurfaceTest.
	self deny: runner hasTestsToRun
]

{ #category : 'tests' }
NewTestRunnerTest >> testTryingToRunFailuresAfterRunningMultipleTestCaseClass [

	runner
		accumulateResult;
		runClasses: { 
				FailThenPassTest.
				YellowTest }.

	self assert: runner runCount equals: 4.
	self assert: runner failureCount equals: 2.

	runner runFailures.
	self assert: runner runCount equals: 4.
	self assert: runner failureCount equals: 1
]
