Class {
	#name : #TestExecutionEnvironmentTest,
	#superclass : #TestExecutionEnvironmentTestCase,
	#instVars : [
		'exampleTestBlock',
		'ranTest'
	],
	#category : #'SUnit-Tests-Core'
}

{ #category : #accessing }
TestExecutionEnvironmentTest class >> defaultTimeLimit [
	^9.1 seconds
]

{ #category : #running }
TestExecutionEnvironmentTest >> createTestService [
	^TestServiceExample new
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

{ #category : #helpers }
TestExecutionEnvironmentTest >> runTestWith: aBlock [

	ranTest := self class selector: #exampleTest.
	executionEnvironment activated.
	
	ranTest exampleTestBlock: aBlock.
	executionEnvironment runTestCase: ranTest
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testActivation [

	executionEnvironment activated.
	
	self assert: executionEnvironment mainTestProcess equals: Processor activeProcess.
	self assert: executionEnvironment watchDogProcess isTerminated not
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testCleanUpTestServicesAfterFailedTest [

	[ 
		self runTestWith: [ self error: 'test error'] 
	] ifError: [].

	self assert: testService isCleanUpDone
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testCleanUpTestServicesAfterSucceedTest [

	self runTestWith: [ #success].

	self assert: testService isCleanUpDone
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testCleanUpTestServicesAfterTerminatedTest [

	self runWithNoHandlers: [
		self runTestWith: [ Processor terminateActive]].

	self assert: testService isCleanUpDone 
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testDeactivation [

	executionEnvironment activated.
	self deny: executionEnvironment watchDogProcess isTerminated.
	
	executionEnvironment deactivated.
	self assert: executionEnvironment watchDogProcess isTerminated.
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testDoesNotCleanUpTestServicesUntilTestErrorIsNotHandled [

	self runWithNoHandlers: [ 
		[ self runTestWith: [ self error: 'test error'] ]
			on: UnhandledError do: [:exc | 
				self deny: testService isCleanUpDone.
	]].

	self assert: testService isCleanUpDone
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testHandleForkedProcessesByAllServices [

	| process |
	executionEnvironment activated.
	
	process := [  ] newProcess name: 'test process'.
	executionEnvironment prepareForNewProcess: process.
	
	self assert: (testService forkedProcesses includes: process)
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
	
	self deny: (testService forkedProcesses includes: executionEnvironment watchDogProcess)
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testIsMainTestProcess [

	executionEnvironment activated.
	
	self assert: (executionEnvironment isMainTestProcess: Processor activeProcess).
	
	self deny: (executionEnvironment isMainTestProcess: [] newProcess).
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testIsMainTestProcessActive [

	executionEnvironment activated.
	
	self assert: (executionEnvironment isMainTestProcessActive).
	
	self runWithNoHandlers: [ 
		self deny: (executionEnvironment isMainTestProcessActive) ]
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testNotifyTestServicesAboutCompletedFailedTest [

	[
		self runTestWith: [ self error: 'test error' ]
	] ifError: [ self deny: testService isCompletedTestHandled ].

	self assert: testService isCompletedTestHandled
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testNotifyTestServicesAboutCompletedTest [

	self runTestWith: [
		self deny: testService isCompletedTestHandled].

	self assert: testService isCompletedTestHandled
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testNotifyTestServicesAboutHalt [
	| errorPassed expectedException|
	errorPassed := false.
	expectedException := Halt new messageText: 'test halt'.
	self runWithNoHandlers: [	
		[self runTestWith: [ expectedException signal]] on: Halt do: [:actualException | 
			errorPassed := true.
			self assert: actualException equals: expectedException.
			self assert: (testService unhandledExceptions includes: actualException)].
	].

	self assert: errorPassed
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testNotifyTestServicesAboutUnhandledError [
	| errorPassed expectedException|
	errorPassed := false.
	expectedException := Error new messageText: 'test error'.
	self runWithNoHandlers: [	
		[self runTestWith: [ expectedException signal]] on: UnhandledError do: [:actualException | 
			errorPassed := true.
			self assert: actualException exception equals: expectedException.
			self assert: (testService unhandledExceptions includes: actualException)].
	].

	self assert: errorPassed
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testNotifyTestServicesAboutWarning [
	| errorPassed expectedException|
	errorPassed := false.
	expectedException := Warning new messageText: 'test halt'.
	self runWithNoHandlers: [	
		[self runTestWith: [ expectedException signal]] on: Warning do: [:actualException | 
			errorPassed := true.
			self assert: actualException equals: expectedException.
			self assert: (testService unhandledExceptions includes: actualException)].
	].

	self assert: errorPassed
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testRegisteredServiceShouldBeBoundToEnvironment [

	self assert: testService executionEnvironment equals: executionEnvironment
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testResetWatchDogTimeoutBetweenFailedTests [

	[self runTestWith: [ 
		executionEnvironment maxTimeForTest: self defaultTimeLimit + 10 seconds. 
		self error: 'test error']
	]	ifError: [].
	
	self runTestWith: [ 
		self assert: executionEnvironment maxTimeForTest equals: self defaultTimeLimit]
]

{ #category : #tests }
TestExecutionEnvironmentTest >> testResetWatchDogTimeoutBetweenTests [

	self runTestWith: [ executionEnvironment maxTimeForTest: self defaultTimeLimit + 10 seconds].
	
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
