"
I am a TestRunner. I run tests and return the result from:
	- TestClass
	- TestCase
	- TestSuite
	- aPackage.
	- Collection of Tests

You can simply use my method (NewTestRunner class run: aRunnable) to run all the previous elements.
```smalltalk
NewTestRunner run: GreenTest
```

# Public API
## Running
- run:
- runFailures
- runErrors
- result

## Change behaviour
- switchToDebugMode
- switchToRunMode
- accumulateResult
- doNotAccumulateResult
"
Class {
	#name : 'NewTestRunner',
	#superclass : 'Object',
	#instVars : [
		'suite',
		'result',
		'suitePreparator',
		'shouldAccumulate',
		'executionMode'
	],
	#category : 'TestRunner-Core-Core',
	#package : 'TestRunner-Core',
	#tag : 'Core'
}

{ #category : 'tests' }
NewTestRunner class >> isValidClass: aClass [

	^ aClass isTestCase and: [ aClass isAbstract not ]
]

{ #category : 'running' }
NewTestRunner class >> run: aRunnable [

	^ self new
		  run: aRunnable;
		  result
]

{ #category : 'running' }
NewTestRunner class >> runCase: aTestCase [

	"return the result of the execution of the giving testCase"

	^ self new
		  runCase: aTestCase
]

{ #category : 'running' }
NewTestRunner class >> runClass: aTestClass [

	^ self new runClass: aTestClass
]

{ #category : 'running' }
NewTestRunner class >> runPackage: aPackage [

	"return the result of the execution of the giving package"

	^ self new
		  runPackage: aPackage;
		  result
]

{ #category : 'running' }
NewTestRunner class >> runSequenceableCollection: aCollection [
	^ self new
		  runSequenceableCollection: aCollection;
		  result
]

{ #category : 'running' }
NewTestRunner class >> runSuite: aTestSuite [

	"return the result of the execution of the giving testSuite"

	^ self new
		  runSuite: aTestSuite;
		  result
]

{ #category : 'API' }
NewTestRunner >> accumulateResult [
	"activate the results accumulation behavior"

	self shouldAccumulate: true
]

{ #category : 'private' }
NewTestRunner >> addError: aTestError [
	"store the giving testCase as Error in result"

	self result addError: aTestError
]

{ #category : 'private' }
NewTestRunner >> addFailure: aTestFailure [
	"store the giving testCase as Failure in result"

	self result addFailure: aTestFailure
]

{ #category : 'private' }
NewTestRunner >> addPass: aPassedTest [
	"store the giving testCase as passed test in result"

	self result addPass: aPassedTest
]

{ #category : 'private' }
NewTestRunner >> addToSuite: aTestSuite [
	"add the giving testsuite to the current suite. Notice that only the tests are added"

	self suite addTests: aTestSuite tests
]

{ #category : 'private' }
NewTestRunner >> classForTestResult [

	^ TestRunnerResult 
]

{ #category : 'running' }
NewTestRunner >> debugFailure: failure [
	"debug the giving failure"

	self flag: #TODO. "method not defined in execution mode interface"
	self executionMode debugFailure: failure
]

{ #category : 'API' }
NewTestRunner >> doNotAccumulateResult [
	"disable the results accumulation behavior"

	self shouldAccumulate: false
]

{ #category : 'accessing' }
NewTestRunner >> errorCount [
	"returns the number of testcase which failed du to error during the execution"

	^ self result errorCount
]

{ #category : 'accessing' }
NewTestRunner >> executionMode [ 

	^ executionMode ifNil: [ executionMode := self executionModeClass runTestsMode ]
]

{ #category : 'setter' }
NewTestRunner >> executionMode: anExecutionMode [

	executionMode := anExecutionMode
]

{ #category : 'private' }
NewTestRunner >> executionModeClass [

	^ ExecutionMode
]

{ #category : 'accessing' }
NewTestRunner >> failureCount [
	"returns the number of testcase which failed du to assertion error during the execution"

	^ self result failureCount
]

{ #category : 'testing' }
NewTestRunner >> hasTestsToRun [
	"check either the runner has test to run or not.
	If any test has not been collected, there nothing to run"

	^ self suite tests isNotEmpty
]

{ #category : 'accessing' }
NewTestRunner >> passedCount [
	"return the number of passed testcase"

	^ self result passedCount
]

{ #category : 'processing' }
NewTestRunner >> prepareSuite [
	"we would like to process suites before execution (e.g shufling the tests for exemple).
	see SuitePreparator"

	self suitePreparator applyOn: self suite
]

{ #category : 'private' }
NewTestRunner >> resetResult [
	"reset the runner result"

	result := self classForTestResult new
]

{ #category : 'private' }
NewTestRunner >> resetSuite [

	suite := nil
]

{ #category : 'accessing' }
NewTestRunner >> result [

	^ result ifNil: [ result := self classForTestResult new]
]

{ #category : 'accessing' }
NewTestRunner >> resultDescription [
	"return the description of test case"

	^ result asString
]

{ #category : 'API' }
NewTestRunner >> run: aRunnable [

	aRunnable runBy: self.
	^ self result
]

{ #category : 'running' }
NewTestRunner >> runAll [
	"run the current collected suite"

	[ 
	self prepareSuite.
	self runSuite ] ensure: [ self tearDown ]
]

{ #category : 'running' }
NewTestRunner >> runCase: aTestCase [
	"run and return the result of the fiving testCase"

	| testResult |
	testResult :=  self executionMode executeCase: aTestCase.
	self shouldAccumulate ifTrue: [ 
		self result mergeWith: testResult ].
	^ testResult 
]

{ #category : 'running' }
NewTestRunner >> runClass: aClass [
	"run and return the result of the giving class"

	self shouldAccumulate ifFalse: [ self resetResult ].
	self testSuiteFromClass: aClass.
	self suite name: aClass name.

	self runAll.

	^ self result
]

{ #category : 'running' }
NewTestRunner >> runClasses: aCollectionOfTestCases [
	"run and return the result of the giving collection of classes"

	self accumulateResult.
	aCollectionOfTestCases reject: #isAbstract thenDo: [ :class | self runClass: class ]  .
	^ self result
]

{ #category : 'accessing' }
NewTestRunner >> runCount [
	"returns the number of executed testcase"

	^ self result runCount
]

{ #category : 'running' }
NewTestRunner >> runError: aTestCase [
	"run the giving testCase and update the result. 
	Usefull if we want only to rerun the failure after some execution. See self >> runErrors"

	| rerunningResult |
	rerunningResult := aTestCase run.
	rerunningResult hasPassed ifTrue: [ result removeError: aTestCase ].
	self updateResultWith: rerunningResult
]

{ #category : 'running' }
NewTestRunner >> runErrors [
	"rerun the failed testcases du to error"

	self result errors do: [ :testCase | self runError: testCase ]
]

{ #category : 'running' }
NewTestRunner >> runFailure: aTestCase [
	"rerun the giving failure then update the result"

	| rerunningResult |
	rerunningResult := self executionMode executeCase: aTestCase .

	rerunningResult hasPassed ifTrue: [ result removeFailure: aTestCase ].
	self updateResultWith: rerunningResult
]

{ #category : 'running' }
NewTestRunner >> runFailures [
	"rerun the failed testcases du to assertion failure"

	self result failures do: [ :testCase | self runFailure: testCase ]
]

{ #category : 'running' }
NewTestRunner >> runPackage: aPackage [
	"run the tests from the giving package"

	self shouldAccumulate ifFalse: [ self resetResult ].
	self testSuiteFromPackage: aPackage.
	self runAll
]

{ #category : 'running' }
NewTestRunner >> runPackages: aCollection [
	"run tests from the giving collections of package"

	self accumulateResult.
	aCollection do: [ :package | self runPackage: package ]
]

{ #category : 'running' }
NewTestRunner >> runSequenceableCollection: aCollection [
	
	self accumulateResult.
	aCollection do: [ :test | self run: test ].
	^ self result
]

{ #category : 'running' }
NewTestRunner >> runSuite [
	"run the tests from the current suite and collect the results"

	self suite tests 
		collect: [ :aTestCase | self runCase: aTestCase ] 
		thenDo: [ :aResult | self result mergeWith: aResult ].
	^ self result 
]

{ #category : 'running' }
NewTestRunner >> runSuite: aTestSuite [
	"run tests from the giving testsuite"

	self suite: aTestSuite.
	self runAll
]

{ #category : 'testing' }
NewTestRunner >> shouldAccumulate [

	^ shouldAccumulate ifNil: [ shouldAccumulate := false ]
]

{ #category : 'setter' }
NewTestRunner >> shouldAccumulate: aBoolean [

	shouldAccumulate := aBoolean
]

{ #category : 'testing' }
NewTestRunner >> someTestCausesError [

	^ result hasErrors
]

{ #category : 'testing' }
NewTestRunner >> someTestFailed [
	"Tell if some test failed during execution"

	^ self result hasFailures
]

{ #category : 'accessing' }
NewTestRunner >> suite [

	^ suite ifNil: [ suite := TestSuite new ]
]

{ #category : 'setter' }
NewTestRunner >> suite: aTestSuite [

	suite := aTestSuite
]

{ #category : 'private' }
NewTestRunner >> suitePreparator [

	^ suitePreparator ifNil: [ suitePreparator := self suitePreparatorClass new ]
]

{ #category : 'setter' }
NewTestRunner >> suitePreparator: aSuitePreparator [

	suitePreparator := aSuitePreparator
]

{ #category : 'private' }
NewTestRunner >> suitePreparatorClass [

	^ SuitePreparator 
]

{ #category : 'API' }
NewTestRunner >> switchToDebugMode [

	self executionMode: self executionModeClass debugMode
]

{ #category : 'API' }
NewTestRunner >> switchToRunMode [

	self executionMode: self executionModeClass runTestsMode
]

{ #category : 'processing' }
NewTestRunner >> tearDown [
	"after the execution of the collected suite. We put here the instruction to execute"

	self resetSuite
]

{ #category : 'private' }
NewTestRunner >> testSuiteFromClass: aClass [
	"build a suite from the giving class then add it to the internal collections of suites. 
	If it doesn't respect some criteria, nothing won't be collected. (See self class >> isValidClass)"

	(self class isValidClass: aClass) ifTrue: [ 
		self addToSuite: aClass suite ]
]

{ #category : 'private' }
NewTestRunner >> testSuiteFromPackage: aPackage [
	"Build testsuite from the giving Package"

	aPackage definedClasses do: [ :c | self testSuiteFromClass: c ].
	self suite name: aPackage name
]

{ #category : 'private' }
NewTestRunner >> testSuitesFromClasses: aCollectionOfClass [
	"Build testsuite from the giving collection of class"

	aCollectionOfClass do: [ :class | 
		self suite addTests: class suite tests ]
]

{ #category : 'accessing' }
NewTestRunner >> testsCount [
	"return the number of collected testCase"

	^ self result tests size
]

{ #category : 'private' }
NewTestRunner >> updateResultWith: testResult [
	"Merge the current result with the giving one then return it"

	^ self result mergeWith: testResult; yourself
]
