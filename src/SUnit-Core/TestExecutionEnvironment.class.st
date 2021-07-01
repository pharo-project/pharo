"
I am a special execution environment to manage the execution of tests.	
I am installed when test is running (or test suite) by

	CurrentExecutionEnvironment runTestCase: aTestCase
		
My current instance for tests can be accessed with handy method directly from the test case: 

	self executionEnvironment 
	
It simply returns the value of CurrentExecutionEnvironment.
	
Some tests requires completely clean and simplest environment. To disable me override #runCaseManaged on your test class: 

	runCaseManaged 
		^DefaultExecutionEnvironment beActiveDuring: [ self runCase]

1) Test watchdog

I implement a watchdog to ensure that tests complete in finite time and never hanged.
I give them 10 seconds by default which can be overriden by test class using method #defaultTimeLimit.
Or it can be specified directly in the test method:
	self timeLimit: 10 seconds
It can be changed at any time during the test.

To implement this logic I maintain special watchdog process which controls the execution time of tests. It is a single process for overall test suite.

2) Test services 

In addition I provide an extandable service infrastructure to monitor the test execution.
During the test I track all signaled exceptions from the test process and all forked processes.
I notify registered services about these events:

- #handleException:, it is executed for every exception signaled from the test 
- #handleCompletedTest, it is executed when test is completes (successfully or due to the error)
- #handleNewProcess:, it is executed for every process created during the test
- #cleanUpAfterTest:, it is executed as final action when test is done and the environment needs to be prepared for the next test run.

My #services are subclasses of TestExecutionService (see its comment for details).
	
I automatically register all service classes enabled by default (#registerDefaultServices). But users can enable required services manually for given tests:

	self executionEnvironment enableService: TestServiceExample

Or with configuration block: 

	self executionEnvironment enableService: TestServiceExample using: [:service | ]
	
I lookup given service class in my registered services and use the found one if it exists. Otherwise I create and register new service instance with enabled state. 	
At the end of test I disable services which were registered manually registered and which are disabled by default. So no other tests are affected by them.
	
I provide handy method to access well known service ProcessMonitorTestService:

	self executionEnvironment processMonitor 
	
Or simply:

	self executionProcessMonitor
		 
Internal Representation and Key Implementation Points.

    Instance Variables
	services:		<OrderedCollection<TestExecutionService>>
	mainTestProcess:		<Process>
	maxTimeForTest:		<Duration>
	testCase:		<TestCase>
	watchDogProcess:		<Process>
	watchDogSemaphore:		<Semaphore>
	testCompleted:		<Boolean>
"
Class {
	#name : #TestExecutionEnvironment,
	#superclass : #ExecutionEnvironment,
	#instVars : [
		'watchDogProcess',
		'watchDogSemaphore',
		'testCase',
		'maxTimeForTest',
		'testCompleted',
		'services',
		'mainTestProcess'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #'fuel support' }
TestExecutionEnvironment class >> fuelIgnoredInstanceVariableNames [
    ^#('watchDogProcess' 'watchDogSemaphore' 'mainTestProcess')
]

{ #category : #settings }
TestExecutionEnvironment class >> settingsOn: aBuilder [
	<systemsettings>	

	TestExecutionService defaultServiceClasses do: [ :each | 
		each settingsOn: aBuilder]
]

{ #category : #controlling }
TestExecutionEnvironment >> activated [

	mainTestProcess := Processor activeProcess.
	self registerDefaultServices.
	self startWatchDog
]

{ #category : #accessing }
TestExecutionEnvironment >> backgroundFailures [
	^self processMonitor testBackgroundFailures
]

{ #category : #controlling }
TestExecutionEnvironment >> cleanUpAfterTest [
	"Cleanup is performed over all services (enabled and disabled) 
	because service can change its state during test execution
	(user can disable it in the middle of test)"
	
	services do: [ :each | each cleanUpAfterTest]
]

{ #category : #controlling }
TestExecutionEnvironment >> deactivated [

	watchDogProcess ifNotNil: [watchDogProcess terminate]
]

{ #category : #private }
TestExecutionEnvironment >> disableService: aTestExecutionServiceClass [

	| service |
	service := self findService: aTestExecutionServiceClass ifAbsent: [^self].
	service disable 
]

{ #category : #private }
TestExecutionEnvironment >> enableService: aTestExecutionServiceClass [

	| service |
	service := self findService: aTestExecutionServiceClass ifAbsent: [
		self registerService: aTestExecutionServiceClass new].
	service enable.
	^service
]

{ #category : #private }
TestExecutionEnvironment >> enableService: aTestExecutionServiceClass using: aBlock [

	| service |
	service := self enableService: aTestExecutionServiceClass.
	aBlock value: service.
	^service
]

{ #category : #accessing }
TestExecutionEnvironment >> enabledServicesDo: aBlock [
	
	services select: [ :each | each isEnabled ] thenDo: aBlock
]

{ #category : #accessing }
TestExecutionEnvironment >> failures [
	^self processMonitor testFailures
]

{ #category : #accessing }
TestExecutionEnvironment >> findService: aTestExecutionServiceClass [
	^services detect: [:each | each isKindOf: aTestExecutionServiceClass]
]

{ #category : #accessing }
TestExecutionEnvironment >> findService: aTestExecutionServiceClass ifAbsent: absentBlock [
	^services detect: [:each | each isKindOf: aTestExecutionServiceClass] ifNone: absentBlock
]

{ #category : #accessing }
TestExecutionEnvironment >> forkedProcesses [
	^ self processMonitor forkedProcesses
]

{ #category : #controlling }
TestExecutionEnvironment >> handleCompletedTest [
	
	self enabledServicesDo: [ :each | each handleCompletedTest].
]

{ #category : #controlling }
TestExecutionEnvironment >> handleException: anException [

	self enabledServicesDo: [ :each | each handleException: anException].
	
	anException pass
]

{ #category : #controlling }
TestExecutionEnvironment >> handleNewProcess: aProcess [

	self enabledServicesDo: [ :each | each handleNewProcess: aProcess ].
]

{ #category : #initialization }
TestExecutionEnvironment >> initialize [
	super initialize.
	services := OrderedCollection new.
	testCompleted := false
]

{ #category : #testing }
TestExecutionEnvironment >> isMainTestProcess: aProcess [
	^mainTestProcess = aProcess
]

{ #category : #testing }
TestExecutionEnvironment >> isMainTestProcessActive [
	^self isMainTestProcess: Processor activeProcess
]

{ #category : #testing }
TestExecutionEnvironment >> isMainTestProcessFailed [
	^self processMonitor isMainTestProcessFailed
]

{ #category : #testing }
TestExecutionEnvironment >> isTest [
	^true
]

{ #category : #accessing }
TestExecutionEnvironment >> mainTestProcess [
	^ mainTestProcess
]

{ #category : #accessing }
TestExecutionEnvironment >> maxTimeForTest [
	^ maxTimeForTest
]

{ #category : #accessing }
TestExecutionEnvironment >> maxTimeForTest: aDuration [
	maxTimeForTest := aDuration.
	watchDogSemaphore ifNotNil: [ 
		"we need restart watch dog timer for new timeout"
		watchDogSemaphore signal ]. 
]

{ #category : #controlling }
TestExecutionEnvironment >> prepareForNewProcess: aProcess [
	| processBlock |
	watchDogProcess ifNil: [ ^self ]. "we should not catch watchDogProcess which is always the first one"
	aProcess suspendedContext sender ifNotNil: [ ^self ]. "Some existing tests in system create processes on arbitrary block and then check suspendedContext state. Without this 'if' all these tests will fail"
	processBlock := aProcess suspendedContext receiver.
	processBlock isClosure ifFalse: [ ^self ]. "same case as in previous comment"
	
	self handleNewProcess: aProcess
]

{ #category : #accessing }
TestExecutionEnvironment >> processMonitor [
	^self findService: ProcessMonitorTestService
]

{ #category : #controlling }
TestExecutionEnvironment >> registerDefaultServices [

	TestExecutionService enabledServiceClasses do: [ :each |
		self registerService: each new	
	 ]
]

{ #category : #private }
TestExecutionEnvironment >> registerService: aTestExecutionService [

	aTestExecutionService executionEnvironment: self.
	services add: aTestExecutionService.
	^aTestExecutionService 
]

{ #category : #accessing }
TestExecutionEnvironment >> removeAllServices [

	services removeAll
]

{ #category : #controlling }
TestExecutionEnvironment >> runTestCase: aTestCase [
	testCase := aTestCase.
	maxTimeForTest := testCase defaultTimeLimit.
	testCompleted := false.
	watchDogSemaphore signal. "signal about new test case"	
	[self runTestCaseUnderWatchdog: aTestCase] ensure: [
		testCompleted := true.
		watchDogSemaphore signal.  "signal that test case is completed"	
		self cleanUpAfterTest	].
]

{ #category : #controlling }
TestExecutionEnvironment >> runTestCaseUnderWatchdog: aTestCase [
	
	[
		[aTestCase runCase] ensure: [
			"Terminated test is not considered as completed (user just closed a debugger for example)"
			mainTestProcess isTerminating ifFalse: [ 
				self handleCompletedTest ]]
	] on: Exception do: [ :err | 
			self handleException: err 
	]
	
]

{ #category : #controlling }
TestExecutionEnvironment >> runTestsBy: aBlock [

	aBlock value
]

{ #category : #accessing }
TestExecutionEnvironment >> services [
	^ services
]

{ #category : #accessing }
TestExecutionEnvironment >> services: anObject [
	services := anObject
]

{ #category : #controlling }
TestExecutionEnvironment >> startWatchDog [
	
	watchDogSemaphore := Semaphore new.
	watchDogProcess := [self watchDogLoop] newProcess.
	"Watchdog needs to run at high priority to do its job (but not at timing priority)"
	watchDogProcess 
		name: 'Tests execution watch dog';
		priority: Processor timingPriority-1;
		resume
]

{ #category : #accessing }
TestExecutionEnvironment >> testCase [
	^ testCase
]

{ #category : #accessing }
TestExecutionEnvironment >> testCase: anObject [
	testCase := anObject
]

{ #category : #controlling }
TestExecutionEnvironment >> watchDogLoop [

	| timeIsGone |
	[	"waiting new test case" 
		watchDogSemaphore wait. 
		"waiting while test completes"
		[timeIsGone := watchDogSemaphore wait: maxTimeForTest.
		testCompleted] whileFalse: [ 
			"this subloop allows to dynamically change time limit and restart watch dog"
			timeIsGone ifTrue: [
				"The main purpose of following condition is to ignore timeout when test is under debug.
				Test process is suspended only when it is debugged"
				mainTestProcess isSuspended ifFalse: [
					mainTestProcess signalException: TestTookTooMuchTime new]]	].
	] repeat


]

{ #category : #accessing }
TestExecutionEnvironment >> watchDogProcess [
	^ watchDogProcess
]
