"
I am special environment to manage test execution. I address three problems:

1) Tests should never hang. They should be executed with time limit. 
I give them 500 milliseconds by default. It could be overriden by TestCase method #defaultTimeLimit.
Or it could be specified directly in test method by 
	self timeLimit: 10 seconds
It could be changed at any time of test execution.

To implement this logic I maintain special watch dog process which control execution time of tests. It is single for all test suite.

2) When test completes I terminate all running processes which were forked during execution. 

3) I manage all failures from forked processes by preventing spawning debuggers. I mark such tests as failed by signalling TestForkedFailedProcess error.
When failure is signelled from forked process I suspend it and collect them together inside failedProcesses dictionary.
TestForkedFailedProcess signal is resumable to allow debug suspended failures. When you debug test and saw this problem you can press Proceed to debug actual failures.

I am installed when test is running (or test suite) by
	CurrentExecutionEnvironment runTestCase: aTestCase
	
Internal Representation and Key Implementation Points.

    Instance Variables
	failedProcesses:		<Dictionary of<Process->Error>>
	forkedProcesses:		<OrderedCollection of<Process>>
	maxTimeForTest:		<Duration>
	testCase:		<TestCase>
	watchDogProcess:		<Process>
	watchDogSemaphore:		<Semaphore>
"
Class {
	#name : #TestExecutionEnvironment,
	#superclass : #ExecutionEnvironment,
	#instVars : [
		'testProcess',
		'watchDogProcess',
		'watchDogSemaphore',
		'testCase',
		'maxTimeForTest',
		'testCompleted',
		'services'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #controlling }
TestExecutionEnvironment class >> currentFailures [

	^CurrentExecutionEnvironment value failures
]

{ #category : #'fuel support' }
TestExecutionEnvironment class >> fuelIgnoredInstanceVariableNames [
    ^#('watchDogProcess' 'watchDogSemaphore' 'forkedProcesses' 'failedProcesses')
]

{ #category : #controlling }
TestExecutionEnvironment class >> maxTimeForTest: aDuration [

	CurrentExecutionEnvironment value maxTimeForTest: aDuration
]

{ #category : #controlling }
TestExecutionEnvironment class >> resetFailures [

	^CurrentExecutionEnvironment value resetFailures
]

{ #category : #controlling }
TestExecutionEnvironment >> activated [

	testProcess := Processor activeProcess.
	watchDogSemaphore := Semaphore new.
	watchDogProcess := [self watchDogLoop] newProcess.
	"Watchdog needs to run at high priority to do its job (but not at timing priority)"
	watchDogProcess 
		name: 'Tests execution watch dog';
		priority: Processor timingPriority-1;
		resume
]

{ #category : #controlling }
TestExecutionEnvironment >> cleanUpAfterTest [
	
	services do: [ :each | each cleanUpAfterTest]
]

{ #category : #controlling }
TestExecutionEnvironment >> deactivated [

	watchDogProcess ifNotNil: [watchDogProcess terminate]
]

{ #category : #accessing }
TestExecutionEnvironment >> failures [
	self flag: #todo.
	
	^services first suspendedBackgroundFailures values
]

{ #category : #accessing }
TestExecutionEnvironment >> forkedProcesses [
	self flag: #todo.
	
	^ services first forkedProcesses
]

{ #category : #controlling }
TestExecutionEnvironment >> handleCompletedTest [
	
	services do: [ :each | each handleCompletedTest].
]

{ #category : #controlling }
TestExecutionEnvironment >> handleNewProcess: aProcess [

	services do: [ :each | each handleNewProcess: aProcess ].
]

{ #category : #controlling }
TestExecutionEnvironment >> handleUnhandledException: anUnhandledException [

	services do: [ :each | each handleUnhandledException: anUnhandledException].
	
	anUnhandledException pass
]

{ #category : #initialization }
TestExecutionEnvironment >> initialize [
	super initialize.
	services := OrderedCollection new.
	testCompleted := false
]

{ #category : #testing }
TestExecutionEnvironment >> isTest [
	^true
]

{ #category : #testing }
TestExecutionEnvironment >> isTestProcess: aProcess [
	^testProcess = aProcess
]

{ #category : #testing }
TestExecutionEnvironment >> isTestProcessActive [
	^self isTestProcess: Processor activeProcess
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
TestExecutionEnvironment >> registerService: aTestExecutionService [

	aTestExecutionService executionEnvironment: self.
	services add: aTestExecutionService
]

{ #category : #controlling }
TestExecutionEnvironment >> resetFailures [
	services first resetFailures
]

{ #category : #controlling }
TestExecutionEnvironment >> runTestCase: aTestCase [
	testCase := aTestCase.
	maxTimeForTest := testCase defaultTimeLimit.
	testCompleted := false.
	watchDogSemaphore signal. "signal about new test case"
	
	[self runTestCaseSafelly: aTestCase] ensure: [
		testCompleted := true.
		watchDogSemaphore signal.  "signal that test case is completed"	
		self cleanUpAfterTest	].
]

{ #category : #controlling }
TestExecutionEnvironment >> runTestCaseSafelly: aTestCase [
	
	[
		[aTestCase runCase] on: UnhandledError, Halt, Warning do: [ :err | 
			self handleUnhandledException: err ]
	] ensure: [self handleCompletedTest]
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

{ #category : #accessing }
TestExecutionEnvironment >> testCase [
	^ testCase
]

{ #category : #accessing }
TestExecutionEnvironment >> testCase: anObject [
	testCase := anObject
]

{ #category : #accessing }
TestExecutionEnvironment >> testProcess [
	^ testProcess
]

{ #category : #accessing }
TestExecutionEnvironment >> testProcess: anObject [
	testProcess := anObject
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
				testProcess isSuspended ifFalse: [
					testProcess signalException: TestTookTooMuchTime new]]	].
	] repeat


]

{ #category : #accessing }
TestExecutionEnvironment >> watchDogProcess [
	^ watchDogProcess
]
