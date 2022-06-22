"
If you giving a TestCase (MyCounterTest), i can execute then and give you the execution result.
I only can run one test a time. (see TestRunnerDecorator to execute several tests)

## Public API
- run:
- runSuite:
- runCase:
- runFailures
- switchToDebugMode
- switchToRunMode
"
Class {
	#name : #BasicTestRunner,
	#superclass : #Object,
	#instVars : [
		'result',
		'suitePreparator',
		'executionMode',
		'suite'
	],
	#category : #'TestRunner-Core-Core'
}

{ #category : #running }
BasicTestRunner class >> run: aClass [

	^ self new run: aClass
]

{ #category : #getter }
BasicTestRunner >> classForTestSuite [

	^ TestSuite
]

{ #category : #running }
BasicTestRunner >> debugFailure: failure [

	"rerun the giving failure"

	"remove it, run it, the update the result"

	| debugResult |
	debugResult := failure debug.
	debugResult ifNotNil: [ result removeFailure: failure ].
	self updateResultWith: debugResult
]

{ #category : #'default values' }
BasicTestRunner >> defaultExecutionEnvironmentClass [

	^ RunExecutionMode
]

{ #category : #'default-values' }
BasicTestRunner >> defaultExecutionModeClass [

	^ RunExecutionMode
]

{ #category : #accessing }
BasicTestRunner >> defaultResultClass [

	^ TestResult
]

{ #category : #accessing }
BasicTestRunner >> defaultSuitePreparatorClass [

	^ AnyEffectSuitePreparator
]

{ #category : #counting }
BasicTestRunner >> errorCount [

	^ self result errorCount
]

{ #category : #accessing }
BasicTestRunner >> executionMode [

	^ executionMode ifNil: [ 
		  executionMode := self defaultExecutionModeClass ]
]

{ #category : #accessing }
BasicTestRunner >> executionMode: anObject [

	executionMode := anObject
]

{ #category : #counting }
BasicTestRunner >> failureCount [

	^ self result failureCount
]

{ #category : #testing }
BasicTestRunner >> hasTestsToRun [

	^ self suite tests isNotEmpty
]

{ #category : #counting }
BasicTestRunner >> passedCount [

	^ self result passedCount
]

{ #category : #running }
BasicTestRunner >> resetResult [

	result := nil
]

{ #category : #running }
BasicTestRunner >> resetSuite [

	suite := nil
]

{ #category : #accessing }
BasicTestRunner >> result [

	^ result ifNil: [ result := self defaultResultClass new ]
]

{ #category : #accessing }
BasicTestRunner >> result: anObject [

	result := anObject
]

{ #category : #running }
BasicTestRunner >> run [

	"run all the suite then return the result"

	self setUp.
	[ self runAll ] ensure: [ self tearDown ]
]

{ #category : #running }
BasicTestRunner >> run: aClass [

	self testSuiteFromClass: aClass.
	self run.
	^ self result
]

{ #category : #running }
BasicTestRunner >> runAll [

	CurrentExecutionEnvironment runTestsBy: [ 
		suite tests do: [ :aTestCase | 
			self updateResultWith: (self runCase: aTestCase) ] ]
]

{ #category : #accessing }
BasicTestRunner >> runCase: aTestCase [

	^ self executionMode executeCase: aTestCase
]

{ #category : #counting }
BasicTestRunner >> runCount [

	^ self result runCount
]

{ #category : #accessing }
BasicTestRunner >> runFailure: failure [

	"rerun the giving failure"

	"remove it, run it, the update the result"

	| aResult |
	aResult := self runCase: failure.
	aResult hasFailures ifTrue: [ ^ self ].
	self result removeFailure: failure.
	self updateResultWith: aResult
]

{ #category : #accessing }
BasicTestRunner >> runFailures [

	"rerun all the failed testcases"

	result failures do: [ :failure | self runFailure: failure ].
	^ result
]

{ #category : #running }
BasicTestRunner >> runSuite: aTestSuite [

	suite := aTestSuite.
	self runAll.
	^ result
]

{ #category : #processing }
BasicTestRunner >> setUp [

	"we would like to process suites before 
	the execution (e.g shufling the tests, ...). 
	It can be useful 
	for tests dependancy detection"

	self resetResult.
	self suitePreparator applyOn: self suite
]

{ #category : #getter }
BasicTestRunner >> suite [
	^ suite ifNil: [ suite := self classForTestSuite new ]
]

{ #category : #accessing }
BasicTestRunner >> suitePreparator [

	^ suitePreparator ifNil: [ 
		  suitePreparator := self defaultSuitePreparatorClass ]
]

{ #category : #accessing }
BasicTestRunner >> suitePreparator: anObject [

	suitePreparator := anObject
]

{ #category : #processing }
BasicTestRunner >> switchToDebugMode [

	self executionMode: DebugExecutionMode
]

{ #category : #processing }
BasicTestRunner >> switchToRunMode [

	self executionMode: RunExecutionMode
]

{ #category : #running }
BasicTestRunner >> tearDown [

	self resetSuite.
]

{ #category : #tests }
BasicTestRunner >> testSuiteFromClass: aClass [

	aClass isTestCase ifFalse: [ ^ self ].
	aClass isAbstract ifTrue: [ ^ self ].
	suite := aClass suite
]

{ #category : #counting }
BasicTestRunner >> testsCount [

	^ self result tests size
]

{ #category : #accessing }
BasicTestRunner >> updateResultWith: testResult [

	"updtade the current test result then return it"

	self result + testResult.
	^ result
]
