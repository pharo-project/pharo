"
I am a more advenced TestRunner. I can run several tests (from a collection, from a giving package). I can accumulate or not tests result when executing test class separatly.

# Public API
- run:
- runAll:
- runPackage:
- runPackages:
- switchToDebugMode
- switchToRunMode
- accumulateResult
- doNotAccumulateResult
- runFailures
"
Class {
	#name : #TestRunner2,
	#superclass : #Object,
	#instVars : [
		'suites',
		'result',
		'suitePreparator',
		'shouldAccumulate',
		'runner',
		'currentTest',
		'currentResult'
	],
	#category : #'TestRunner-Core-Core'
}

{ #category : #running }
TestRunner2 class >> example [
	<script>
	self new 
	suitePreparator: ShuffleSuitePreparator;
	run: DependencyTest;
	inspect
]

{ #category : #running }
TestRunner2 class >> run: aTestCase [
	^ self new
		run: aTestCase
]

{ #category : #processing }
TestRunner2 >> accumulateResult [

	"activate the results accumulation behavior"

	self shouldAccumulate: true
]

{ #category : #accessing }
TestRunner2 >> debugFailure: failure [

	self runner debugFailure: failure
]

{ #category : #accessing }
TestRunner2 >> defaultExecutionEnvironmentClass [

	^ self runner defaultExecutionEnvironmentClass
]

{ #category : #accessing }
TestRunner2 >> defaultRunner [

	^ BasicTestRunner new suitePreparator: self suitePreparator
]

{ #category : #'as yet unclassified' }
TestRunner2 >> defaultSuitePreparator [

	^ AnyEffectSuitePreparator
]

{ #category : #running }
TestRunner2 >> defaultTestResultClass [

	^ TestResult
]

{ #category : #accessing }
TestRunner2 >> doNotAccumulateResult [

	"activate the results accumulation behavior"

	self shouldAccumulate: false
]

{ #category : #accessing }
TestRunner2 >> errorCount [

	^ result errorCount
]

{ #category : #accessing }
TestRunner2 >> failureCount [

	^ self result failureCount
]

{ #category : #testing }
TestRunner2 >> hasTestsToRun [

	"check either the runner has test to run or not.
	If any test has not been added, there nothing to run"

	"Example of usages:"

	"TestRunnerDecorator new hasTestsToRun >>> false
	TestRunnerDecorator new testSuiteFromClass: GreenTest; hasTestsToRun >>> true"

	^ self suites isNotEmpty
]

{ #category : #initialization }
TestRunner2 >> initialize [

	super initialize.
	self doNotAccumulateResult
]

{ #category : #accessing }
TestRunner2 >> passedCount [

	^ self result passedCount
]

{ #category : #running }
TestRunner2 >> resetResult [

	"reset the runner result"

	result := self defaultTestResultClass new
]

{ #category : #accessing }
TestRunner2 >> result [

	^ result ifNil: [ result := self defaultTestResultClass new]
]

{ #category : #getter }
TestRunner2 >> resultDescription [

	^ result asString
]

{ #category : #running }
TestRunner2 >> run: aClass [

	currentTest := aClass.
	self shouldAccumulate ifFalse: [ self resetResult ].
	currentResult := self runner run: aClass.

	^ result := self result + currentResult
]

{ #category : #running }
TestRunner2 >> runAll: aCollectionOfTestCases [

	self accumulateResult.
	aCollectionOfTestCases do: [ :class | 
		class isAbstract ifFalse: [ self run: class ] ].
	^ result
]

{ #category : #accessing }
TestRunner2 >> runCount [

	^ self result runCount
]

{ #category : #running }
TestRunner2 >> runFailure: aTestCase [

	| rerunningResult |
	rerunningResult := self runner runCase: aTestCase.
	rerunningResult hasPassed ifTrue: [ result removeFailure: aTestCase ].
	self updateResultWith: rerunningResult
]

{ #category : #running }
TestRunner2 >> runFailures [

	self result failures do: [ :testCase | self runFailure: testCase ]
]

{ #category : #runninh }
TestRunner2 >> runPackage: aRPackage [

	self runAll:
		(aRPackage definedClasses reject: [ :class | class isAbstract ])
]

{ #category : #running }
TestRunner2 >> runPackages: aCollection [

	aCollection do: [ :package | self runPackage: package ]
]

{ #category : #running }
TestRunner2 >> runSuite: aTestSuite [

	self runner runSuite: aTestSuite
]

{ #category : #accessing }
TestRunner2 >> runner [
	 ^ runner ifNil: [ runner := self defaultRunner ]
]

{ #category : #accessing }
TestRunner2 >> runner: aRunner [

	runner := aRunner
]

{ #category : #processing }
TestRunner2 >> setUp [

	"we would like to process suites before execution (e.g shufling the tests, ...). It can be useful 
	for tests dependancy detection"

	self suitePreparator applyOnSuites: suites
]

{ #category : #tests }
TestRunner2 >> shouldAccumulate [

	^ shouldAccumulate
]

{ #category : #asserting }
TestRunner2 >> shouldAccumulate: aBoolean [

	shouldAccumulate := aBoolean
]

{ #category : #accessing }
TestRunner2 >> someTestCausesError [

	^ result hasErrors
]

{ #category : #accessing }
TestRunner2 >> someTestFailed [

	^ self result hasFailures
]

{ #category : #accessing }
TestRunner2 >> suitePreparator [

	^ suitePreparator ifNil: [ 
		  suitePreparator := self defaultSuitePreparator ]
]

{ #category : #adding }
TestRunner2 >> suitePreparator: aSuitePreparator [

	suitePreparator := aSuitePreparator.
	self runner suitePreparator: aSuitePreparator
]

{ #category : #'instance creation' }
TestRunner2 >> suites [

	^ suites ifNil: [ suites := OrderedCollection new ]
]

{ #category : #counting }
TestRunner2 >> suitesTotal [

	^ suites size
]

{ #category : #running }
TestRunner2 >> switchToDebugMode [

	self runner switchToDebugMode
]

{ #category : #running }
TestRunner2 >> switchToRunMode [

	self runner switchToRunMode
]

{ #category : #running }
TestRunner2 >> tearDown [

	self resetSuites
]

{ #category : #tests }
TestRunner2 >> testSuiteFromClass: aClass [

	"build a suite from the giving class then add it to the internal collections of suites. 
	The giving class should be a test case"

	(aClass isTestCase and: aClass isAbstract not) ifTrue: [ 
		self suites add: aClass suite ]
]

{ #category : #tests }
TestRunner2 >> testSuitesFromClasses: aColOfTestCase [

	"build a collection of TestSuite and save it"

	aColOfTestCase do: [ :class | self testSuiteFromClass: class ]
]

{ #category : #accessing }
TestRunner2 >> testsCount [

	"return the number of all tests methods from suites"

	^ self result tests size
]

{ #category : #tests }
TestRunner2 >> updateResultWith: testResult [

	"updtade the current test result then return it"

	self result + testResult.
	^ result
]
