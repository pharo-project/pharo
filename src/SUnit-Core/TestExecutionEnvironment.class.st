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
		'watchDogProcess',
		'watchDogSemaphore',
		'testCase',
		'maxTimeForTest',
		'forkedProcesses',
		'failedProcesses',
		'testCompleted'
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

	| testProcess |
	testProcess := Processor activeProcess.
	watchDogSemaphore := Semaphore new.
	watchDogProcess := [self watchDogLoopFor: testProcess] newProcess.
	"Watchdog needs to run at high priority to do its job (but not at timing priority)"
	watchDogProcess 
		name: 'Tests execution watch dog';
		priority: Processor timingPriority-1;
		resume
]

{ #category : #controlling }
TestExecutionEnvironment >> checkForkedProcesses [

	forkedProcesses reject: [:each | failedProcesses includesKey: each ] thenDo: #terminate.
	failedProcesses ifEmpty: [ ^self].	
	"Following curtailed logic ensures that we will terminate all processes only 
	if we will not debug them"
	[TestFailedByForkedProcess signal] ifCurtailed: [
		failedProcesses keysDo: #terminate]. 
	"We will be here only if user press Proceed in debugger. 
	In that case we will allow him to debug failed processes"
	failedProcesses keys reject: #isTerminated thenDo: #resume
]

{ #category : #controlling }
TestExecutionEnvironment >> deactivated [

	watchDogProcess terminate
]

{ #category : #accessing }
TestExecutionEnvironment >> failures [
	^failedProcesses values
]

{ #category : #accessing }
TestExecutionEnvironment >> forkedProcesses [
	^ forkedProcesses
]

{ #category : #accessing }
TestExecutionEnvironment >> forkedProcesses: anObject [
	forkedProcesses := anObject
]

{ #category : #initialization }
TestExecutionEnvironment >> initialize [
	super initialize.
	
	forkedProcesses := WeakSet new.
	failedProcesses := OrderedIdentityDictionary new.
	testCompleted := false
]

{ #category : #testing }
TestExecutionEnvironment >> isTest [
	^true
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
	watchDogProcess ifNil: [ ^self ].  "we should not catch watchDogProcess"
	forkedProcesses add: aProcess.
	aProcess suspendedContext sender ifNotNil: [ ^self ]. "Some existing tests in system create processes on arbitrary block and then check suspendedContext state. Without this 'if' all this tests will fail"
	processBlock := aProcess suspendedContext receiver.
	processBlock isClosure ifFalse: [ ^self ]. "same case as in previous comment"
	
	aProcess on: Error do: [ :err | | activeProcess |
		"It is possible to fork process with copy of stack from another process
		It means that aProcess could be not actual active process inside this block handler.
		So we should not reference temp aProcess and instead we should work with actual active process"
		activeProcess := Processor activeProcess. 
		failedProcesses at: activeProcess put: err.
		activeProcess isTerminating ifFalse: [activeProcess suspend].
		err pass]
]

{ #category : #controlling }
TestExecutionEnvironment >> resetFailures [
	failedProcesses removeAll
]

{ #category : #controlling }
TestExecutionEnvironment >> runTestCase: aTestCase [
	testCase := aTestCase.
	maxTimeForTest := testCase defaultTimeLimit.
	testCompleted := false.
	watchDogSemaphore signal. "signal about new test case"

	[[self runTestCaseSafelly: aTestCase] ensure: [
		testCompleted := true.
		watchDogSemaphore signal]. "signal that test case completes"
	
	self checkForkedProcesses] ifCurtailed: [
		forkedProcesses removeAll.
		failedProcesses removeAll]
]

{ #category : #controlling }
TestExecutionEnvironment >> runTestCaseSafelly: aTestCase [
	[
		[aTestCase runCase] on: Halt do: [ :halt | 
			"if test was halted we should resume all background failures 
			to debug all of them together with test process"
			failedProcesses keysDo: #resume. halt pass ]
	] on: Error, TestFailure do: [ :err | 
		"error here means that test is failed. So we should check forked processes 
		and be able to debug all background failures together with original error"
		self checkForkedProcesses. 
		err pass  ]
]

{ #category : #controlling }
TestExecutionEnvironment >> runTestsBy: aBlock [

	aBlock value
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
TestExecutionEnvironment >> watchDogLoopFor: testProcess [

	| timeIsGone |
	[	"waiting new test case" 
		watchDogSemaphore wait. 
		"waiting while test completes"
		[timeIsGone := watchDogSemaphore waitTimeoutMSecs: maxTimeForTest asMilliSeconds.
		testCompleted] whileFalse: [ 
			"this subloop allows to dynamically change time limit and restart watch dog"
			timeIsGone ifTrue: [
				"The main purpose of following condition is to ignore timeout when test is under debug.
				Test process is suspended only when it is debugged"
				testProcess isSuspended ifFalse: [
					testProcess signalException: TestTookTooMuchTime new]]	].
	] repeat


]
