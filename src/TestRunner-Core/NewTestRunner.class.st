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
	#name : #NewTestRunner,
	#superclass : #Object,
	#instVars : [
		'suite',
		'result',
		'suitePreparator',
		'shouldAccumulate',
		'executionMode'
	],
	#category : #'TestRunner-Core-Core'
}

{ #category : #running }
NewTestRunner class >> example [
	<script>
	self new 
	suitePreparator: ShuffleSuitePreparator;
	run: DependencyTest;
	inspect
]

{ #category : #tests }
NewTestRunner class >> isValidClass: aClass [

	| criterias |
	criterias := { 
		             [ :class | class isTestCase ].
		             [ :class | class isAbstract not ] }.

	^ criterias allSatisfy: [ :criteria | criteria value: aClass ]
]

{ #category : #running }
NewTestRunner class >> runCase: aTestCase [

	"return the result of the execution of the giving testCase"

	^ self new
		  runCase: aTestCase;
		  result
]

{ #category : #running }
NewTestRunner class >> runClass: aTestClass [

	^ self new runClass: aTestClass
]

{ #category : #running }
NewTestRunner class >> runPackage: aRPackage [

	"return the result of the execution of the giving package"

	^ self new
		  runPackage: aRPackage;
		  result
]

{ #category : #running }
NewTestRunner class >> runSuite: aTestSuite [

	"return the result of the execution of the giving testSuite"

	^ self new
		  runSuite: aTestSuite;
		  result
]

{ #category : #processing }
NewTestRunner >> accumulateResult [

	"activate the results accumulation behavior"

	self shouldAccumulate: true
]

{ #category : #tests }
NewTestRunner >> addToSuite: aTestSuite [

	suite := self suite , aTestSuite
]

{ #category : #accessing }
NewTestRunner >> debugFailure: failure [

	self executionMode debugFailure: failure
]

{ #category : #accessing }
NewTestRunner >> defaultExecutionEnvironmentClass [

	^ self defaultExecutionEnvironmentClass
]

{ #category : #'default-objects' }
NewTestRunner >> defaultExecutionModeClass [

	^ RunExecutionMode
]

{ #category : #processing }
NewTestRunner >> defaultSuitePreparator [

	^ AnyEffectSuitePreparator
]

{ #category : #running }
NewTestRunner >> defaultTestResultClass [

	^ TestResult
]

{ #category : #processing }
NewTestRunner >> doNotAccumulateResult [

	"activate the results accumulation behavior"

	self shouldAccumulate: false
]

{ #category : #accessing }
NewTestRunner >> errorCount [

	^ result errorCount
]

{ #category : #internal }
NewTestRunner >> executionMode [

	^ executionMode ifNil: [ self defaultExecutionModeClass ]
]

{ #category : #running }
NewTestRunner >> executionMode: anExecutionMode [

	executionMode := anExecutionMode
]

{ #category : #accessing }
NewTestRunner >> failureCount [

	^ self result failureCount
]

{ #category : #testing }
NewTestRunner >> hasTestsToRun [

	"check either the runner has test to run or not.
	If any test has not been added, there nothing to run"

	^ self suite tests isNotEmpty
]

{ #category : #initialization }
NewTestRunner >> initialize [

	super initialize.
	self doNotAccumulateResult
]

{ #category : #accessing }
NewTestRunner >> passedCount [

	^ self result passedCount
]

{ #category : #running }
NewTestRunner >> resetResult [

	"reset the runner result"

	result := self defaultTestResultClass new
]

{ #category : #internal }
NewTestRunner >> resetSuite [

	suite := nil
]

{ #category : #accessing }
NewTestRunner >> result [

	^ result ifNil: [ result := self defaultTestResultClass new]
]

{ #category : #getter }
NewTestRunner >> resultDescription [

	^ result asString
]

{ #category : #running }
NewTestRunner >> runAll [

	[ 
	self setUp.
	self runSuite ] ensure: [ self tearDown ]
]

{ #category : #running }
NewTestRunner >> runAll: aCollectionOfTestCases [

	self accumulateResult.
	aCollectionOfTestCases do: [ :class | 
		class isAbstract ifFalse: [ self runClass: class ] ].
	^ result
]

{ #category : #running }
NewTestRunner >> runCase: aTestCase [
	
	| executionResult |
	executionResult := self executionMode executeCase: aTestCase.
	self result + executionResult.
	^ executionResult
]

{ #category : #running }
NewTestRunner >> runClass: aClass [

	self shouldAccumulate ifFalse: [ self resetResult ].
	self testSuiteFromClass: aClass.
	self runAll.

	^ result
]

{ #category : #accessing }
NewTestRunner >> runCount [

	^ self result runCount
]

{ #category : #running }
NewTestRunner >> runError: aTestCase [

	self notYetImplemented
]

{ #category : #running }
NewTestRunner >> runErrors [

	self result errors do: [ :testCase | self runError: testCase ]
]

{ #category : #running }
NewTestRunner >> runFailure: aTestCase [

	| rerunningResult |
	rerunningResult := self
		                   runCase: aTestCase.
	rerunningResult hasPassed ifTrue: [ result removeFailure: aTestCase ].
	self updateResultWith: rerunningResult
]

{ #category : #running }
NewTestRunner >> runFailures [

	self result failures do: [ :testCase | self runFailure: testCase ]
]

{ #category : #runninh }
NewTestRunner >> runPackage: aRPackage [

	self shouldAccumulate ifFalse: [ self resetResult ].
	self testSuiteFromPackage: aRPackage.
	self runAll
]

{ #category : #running }
NewTestRunner >> runPackages: aCollection [

	self accumulateResult.
	aCollection do: [ :package | self runPackage: package ]
]

{ #category : #running }
NewTestRunner >> runSuite [

	self suite tests do: [ :aTestCase | self runCase: aTestCase ]
]

{ #category : #running }
NewTestRunner >> runSuite: aTestSuite [

	self suite: aTestSuite.
	self runAll
]

{ #category : #processing }
NewTestRunner >> setUp [

	"we would like to process suites before execution (e.g shufling the tests, ...). It can be useful 
	for tests dependancy detection"

	self suitePreparator applyOn: self suite
]

{ #category : #tests }
NewTestRunner >> shouldAccumulate [

	^ shouldAccumulate
]

{ #category : #asserting }
NewTestRunner >> shouldAccumulate: aBoolean [

	shouldAccumulate := aBoolean
]

{ #category : #accessing }
NewTestRunner >> someTestCausesError [

	^ result hasErrors
]

{ #category : #accessing }
NewTestRunner >> someTestFailed [

	^ self result hasFailures
]

{ #category : #'instance creation' }
NewTestRunner >> suite [

	^ suite ifNil: [ suite := TestSuite new ]
]

{ #category : #'suite-building' }
NewTestRunner >> suite: aTestSuite [

	suite := aTestSuite
]

{ #category : #accessing }
NewTestRunner >> suitePreparator [

	^ suitePreparator ifNil: [ 
		  suitePreparator := self defaultSuitePreparator ]
]

{ #category : #adding }
NewTestRunner >> suitePreparator: aSuitePreparator [

	suitePreparator := aSuitePreparator
]

{ #category : #running }
NewTestRunner >> switchToDebugMode [

	self executionMode: DebugExecutionMode
]

{ #category : #running }
NewTestRunner >> switchToRunMode [

	self executionMode: RunExecutionMode
]

{ #category : #running }
NewTestRunner >> tearDown [

	self resetSuite
]

{ #category : #tests }
NewTestRunner >> testSuiteFromClass: aClass [

	"build a suite from the giving class then add it to the internal collections of suites. 
	The giving class should be a test case"

	(self class isValidClass: aClass) ifTrue: [ 
		self addToSuite: aClass suite ]
]

{ #category : #'suite-building' }
NewTestRunner >> testSuiteFromPackage: aRPackage [

	aRPackage definedClasses do: [ :c | self testSuiteFromClass: c ].
	self suite name: aRPackage name
]

{ #category : #'suite-building' }
NewTestRunner >> testSuitesFromClasses: aCollectionOfClass [

	aCollectionOfClass do: [ :class | 
		self suite addTests: class suite tests ]
]

{ #category : #accessing }
NewTestRunner >> testsCount [

	"return the number of all tests methods from suites"

	^ self result tests size
]

{ #category : #tests }
NewTestRunner >> updateResultWith: testResult [

	"updtade the current test result then return it"

	self result + testResult.
	^ result
]
