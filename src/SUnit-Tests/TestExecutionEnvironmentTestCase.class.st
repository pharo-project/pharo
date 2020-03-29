Class {
	#name : #TestExecutionEnvironmentTestCase,
	#superclass : #TestCase,
	#instVars : [
		'testService',
		'executionEnvironment',
		'processes'
	],
	#category : #'SUnit-Tests-Core'
}

{ #category : #testing }
TestExecutionEnvironmentTestCase class >> isAbstract [ 
	^self = TestExecutionEnvironmentTestCase 
]

{ #category : #running }
TestExecutionEnvironmentTestCase >> createTestService [

	self subclassResponsibility 
]

{ #category : #running }
TestExecutionEnvironmentTestCase >> newProcess: aName toImmediatelyExecute: aBlock [
	| newProcess |
	newProcess := aBlock newProcess.
	newProcess name: aName, (processes size + 1) asString.
	newProcess priority: Processor activePriority + 1.
	processes add: newProcess.
	^newProcess
]

{ #category : #running }
TestExecutionEnvironmentTestCase >> runCaseManaged [
	"Here we are testing the test environment logic.
	So we should disable it for ourselves"
	
	^DefaultExecutionEnvironment beActiveDuring: [ self runCase]
]

{ #category : #running }
TestExecutionEnvironmentTestCase >> runWithNoHandlers: aBlock [
	"Executing the given block directly would go through all handlers of SUnit machinery.
	Here we simulate the clean environment with no outer handlers for possible block errors"
	| newProcess synchSemaphore result |
	synchSemaphore := Semaphore new. 
	newProcess := self 
		newProcess: 'Test process with no error handlers' 
		toImmediatelyExecute: [result := aBlock ensure: [synchSemaphore signal]].
	newProcess resume.
	synchSemaphore wait.
	^result
]

{ #category : #running }
TestExecutionEnvironmentTestCase >> setUp [
	super setUp.
	processes := OrderedCollection new.
	
	testService := self createTestService.	
	executionEnvironment := TestExecutionEnvironment new.
	executionEnvironment registerService: testService.

]

{ #category : #running }
TestExecutionEnvironmentTestCase >> tearDown [
	
	executionEnvironment deactivated. "for the case if we activated environment during test"
	processes do: [ :each | each terminate ].
	
	super tearDown.
]
