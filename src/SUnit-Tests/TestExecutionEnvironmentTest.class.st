Class {
	#name : #TestExecutionEnvironmentTest,
	#superclass : #TestCase,
	#instVars : [
		'executionEnvironment',
		'exampleTestBlock',
		'testService1',
		'testService2',
		'ranTest'
	],
	#category : #'SUnit-Tests-Core'
}

{ #category : #accessing }
TestExecutionEnvironmentTest class >> defaultTimeLimit [
	^9.1 seconds
]

{ #category : #helpers }
TestExecutionEnvironmentTest >> exampleTest [
	"It is a test case to be used for testing how testCase is running by environment"

	exampleTestBlock cull: self
]

{ #category : #accessing }
TestExecutionEnvironmentTest >> exampleTestBlock: anObject [
	exampleTestBlock := anObject
]

{ #category : #running }
TestExecutionEnvironmentTest >> runCaseManaged [
	"Here we are testing the test environment logic.
	So we should disable it for ourselves"
	
	^DefaultExecutionEnvironment beActiveDuring: [ self runCase]
]

{ #category : #helpers }
TestExecutionEnvironmentTest >> runTestWith: aBlock [

	ranTest := self class selector: #exampleTest.
	executionEnvironment activated.
	
	ranTest exampleTestBlock: aBlock.
	executionEnvironment runTestCase: ranTest
]

{ #category : #running }
TestExecutionEnvironmentTest >> setUp [
	super setUp.
	
	executionEnvironment := TestExecutionEnvironment new.
	testService1 := TestServiceExample new.
	testService2 := TestServiceExample new.
	executionEnvironment registerService: testService1; registerService: testService2
]

{ #category : #running }
TestExecutionEnvironmentTest >> tearDown [
	
	executionEnvironment deactivated. "for the case if we activated environment during test"
	
	super tearDown.
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testActivation [

	executionEnvironment activated.
	
	self assert: executionEnvironment testProcess equals: Processor activeProcess.
	self assert: executionEnvironment watchDogProcess isTerminated not
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testDeactivation [

	executionEnvironment activated.
	self deny: executionEnvironment watchDogProcess isTerminated.
	
	executionEnvironment deactivated.
	self assert: executionEnvironment watchDogProcess isTerminated.
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testHandleForkedProcessesByAllServices [

	| process |
	executionEnvironment activated.
	
	process := [  ] newProcess name: 'test process'.
	executionEnvironment prepareForNewProcess: process.
	
	self assert: (testService1 forkedProcesses includes: process).
	self assert: (testService2 forkedProcesses includes: process)
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testHasEmptyServicesByDefault [

	executionEnvironment := TestExecutionEnvironment new.
	
	self assert: executionEnvironment services isEmpty
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testIgnoreLongTestWhenItIsSuspendedAsUnderDebug [
	| timeOutSignaled testProcess |
	timeOutSignaled := false.
	testProcess := [
		executionEnvironment activated.		
		[ self runTestWith: [ 
				executionEnvironment maxTimeForTest: 10 milliSeconds.
				Processor activeProcess suspend "it simulates the under debugger condition"]
		] on: TestTookTooMuchTime do: [ :err | timeOutSignaled := true ]		
	] forkAt: Processor activePriority + 1.

	200 milliSeconds wait.
	testProcess resume.
	self deny: timeOutSignaled
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testIgnoreWatchDogProcessAsForkedOne [
	
	executionEnvironment activated.
	executionEnvironment prepareForNewProcess: executionEnvironment watchDogProcess.
	
	self deny: (testService1 forkedProcesses includes: executionEnvironment watchDogProcess).
	self deny: (testService2 forkedProcesses includes: executionEnvironment watchDogProcess)
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testNotifyAllTestServicesAboutCompleteTests [

	self runTestWith: [
		self assert: testService1 completedTests isEmpty].

	self assert: (testService1 completedTests includes: ranTest).
	self assert: (testService2 completedTests includes: ranTest)
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testNotifyAllTestServicesAboutErrorInTest [
	| errorPassed error|
	errorPassed := false.
	error := Error new messageText: 'test error'.
	[self runTestWith: [ error signal]] on: Error do: [:err | 
		errorPassed := true.
		self assert: err identicalTo: error.
		self assert: (testService1 failedTests includes: ranTest).
		self assert: (testService2 failedTests includes: ranTest).
	].

	self assert: errorPassed
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testNotifyAllTestServicesAboutHaltInTest [
	<haltOrBreakpointForTesting>
	| haltPassed |
	haltPassed := false.
	
	[self runTestWith: [ self halt]] on: Halt do: [:halt | 
		haltPassed := true.
		self assert: (testService1 haltedTests includes: ranTest).
		self assert: (testService2 haltedTests includes: ranTest).
	].

	self assert: haltPassed
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testNotifyAllTestServicesAboutTerminatedTests [

	[self runTestWith: [ Processor terminateActive]] forkAt: Processor activePriority + 1.

	self assert: (testService1 terminatedTests includes: ranTest).
	self assert: (testService2 terminatedTests includes: ranTest).
	self deny: (testService1 completedTests includes: ranTest).
	self deny: (testService2 completedTests includes: ranTest)
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testResetWatchDogBetweenTests [

	self runTestWith: [ executionEnvironment maxTimeForTest: 10 milliSeconds].
	
	self runTestWith: [ 
		self assert: executionEnvironment maxTimeForTest equals: self defaultTimeLimit]
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testRunningTest [
	| executed |
	executed := false.
	
	self runTestWith: [ 
		self assert: executionEnvironment testCase identicalTo: ranTest.
		executed := true
	].

	self assert: executed
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testRunningTestShouldSetDefaultTimeLimit [

	self runTestWith: [ 
		self assert: executionEnvironment maxTimeForTest equals: self defaultTimeLimit
	]
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testRunningTooLongTest [

	self 
		should: [
			self runTestWith: [ 
				executionEnvironment maxTimeForTest: 10 milliSeconds.
				20 milliSeconds wait]]
		raise: TestTookTooMuchTime 
]
