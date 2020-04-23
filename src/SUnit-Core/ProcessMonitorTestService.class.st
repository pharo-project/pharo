"
I monitor all processes forked during tests and track all exceptions signaled from them.

The main goal is to ensure that if there was an error from the background process the test will never be green. 
For example I make following test fail:

	testExample
		[ 1/0 ] fork.
		self assert: true.

TestFailedByForkedProcess will be signaled at the end of such test. And if user will debug this test then two debuggers will be opened for each error (for main and background processes).
		
The backgrond process can be more tricky and it could fail after a long time when another arbitrary test is currently executed. Generally background processes left by tests make the system dirty. It can lead to intermittent failures and flaky tests which is very hard to debug.
And my second goal is to fail the test when it left running processes. I make following test fail: 

	testExample
		[ 10 seconds wait ] fork.
		self assert: true.

TestLeftRunningProcess will be signaled at the end of test.
Currently this feature is disabled by default. And this test would not fail. But it can be configured:	 

	aTestCase testProcessMonitor failTestLeavingProcesses.

Or enable it with: 

	aTestCase testProcessMonitor allowTestToLeaveProcesses.

This configuration can be added to #setUp or there is global settings (also available in settings browser/SUnit):

	ProcessMonitorTestService shouldFailTestLeavingProcesses: true

Also I can be completely disabled for the test: 

	aTestCase testProcessMonitor disable 
	 
Or I can be disabled globaly (also available in settings browser/SUnit): 

	ProcessMonitorTestService isEnabled: false.

Finally at the end of every test I perform a clean up (#cleanUpAfterTest). Here I restore my default state and terminate all left running processes. So the next test will be started on clean environment.

Implementation details:

1. Background failures

I am a kind of TestExecutionService and therefore I intercept all processes forked during the test (#handleNewProcess:).
I set up default exception handler for them to catch all unhandled exceptions. UnhandledException's are supposed to open a debugger at the end of processing. I postpone this logic by suspending the failing process. So when error is signaled from the background process no debugger will be opened immediately.
At the end of test (#handleCompletedTest) I check all suspended failures and if they exist I fail the test with TestFailedByForkedProcess signal.
In interactive mode when user debugs the test I pass all suspended failures together with this signal. So multiple debuggers will be opened to show each error.

The interactive scenario is detected using UnhandledException logic: unhandled error from the main test process or halt from anywhere mean the ""debugger fallback"" and therefore it is a good time to show the rest of failures from other processes. So in that case I pass all suspended failures to debug all of them together.

2. Failing strategy for left processes

I collect all forked processes (#forkedProcesses) and at the end of tests I have an option (#shouldFailTestLeavingProcesses) to fail the test if those processes are still running. TestLeftRunningProcess would be a final error for the test in that case.
I implement a little logic to make this failing strategy more usable and not break the test in trivial cases. For eaxmple following pattern is quite common in tests: 

	testExample 
		[ Processor yield ] fork.
		self assert: true.

""Yield"" could always happen as a side affect of domain code or other image processes. And a simple #fork in tests like this one will not be able to terminate.
The method #allowRunningProcessesToFinish performs own ""Processor yield"" logic to allow background processes take control and finish execution. 

3. One more detail: 
In uninteractive mode I signal those special errors even if there was a domain error from the test. So two exceptions can be signaled for single test run.
It allows to log all errors signaled during the test: one from the main process and others from the background processes.

Internal Representation and Key Implementation Points.

    Instance Variables
	forkedProcesses:		<OrderedCollection<Process>>
	testFailures:		<OrderedIdentityDictionary<Process, Exception>>
	shouldFailTestLeavingProcesses:		<Boolean>
	shouldSuspendBackgroundFailures:		<Boolean>
"
Class {
	#name : #ProcessMonitorTestService,
	#superclass : #TestExecutionService,
	#instVars : [
		'forkedProcesses',
		'testFailures',
		'shouldSuspendBackgroundFailures',
		'shouldFailTestLeavingProcesses'
	],
	#classInstVars : [
		'shouldFailTestLeavingProcesses'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #settings }
ProcessMonitorTestService class >> descriptionForSettingsBrowser [

	^'I monitor all processes forked during the test and manage all exception:
	- when I catch exception from background process I suspend it.
	- at the end of test I signal TestFailedByForkedProcess if there were any background failures
	- if test was failed and the error was unhandled (and debugger was shown) I pass all suspended errors. So when you debug the test you will see all background erros as usualy.
	- if halt happens anywere I pass all suspended errors and do not suspend new one anymore
	- I can force test to not left running processes. See the next setting for it.
	- When all checks and errors were done I clean the system: I terminate all processes forked during the test'
]

{ #category : #'fuel support' }
ProcessMonitorTestService class >> fuelIgnoredInstanceVariableNames [
    ^#('forkedProcesses' 'suspendedBackgroundFailures')
]

{ #category : #settings }
ProcessMonitorTestService class >> settingsOn: aBuilder [
	super settingsOn: aBuilder.
	
	(aBuilder setting: #shouldFailTestLeavingProcesses)
		target: self;
		parent: self name ;
		default: false;
		label: 'Fail tests which left running processes' ;
		description: 'When test forks new process it is a good practice to take care about it at the end ot test. 
This setting allows SUnit to track dirty state after the test when it does not perform proper cleanup and left running processes'
]

{ #category : #accessing }
ProcessMonitorTestService class >> shouldFailTestLeavingProcesses [
	^shouldFailTestLeavingProcesses ifNil: [ shouldFailTestLeavingProcesses := false ]
]

{ #category : #accessing }
ProcessMonitorTestService class >> shouldFailTestLeavingProcesses: aBoolean [
	shouldFailTestLeavingProcesses := aBoolean
]

{ #category : #controlling }
ProcessMonitorTestService >> allowRunningProcessesToFinish [
	"The idea here is to allow most trivial processes to finish themselves.
	So they would not be a garbage left by test.
	For example following test would left one process at the end:
		>>testExample
			[ Processor yield ] fork 
	But if we would give it a chance to finish it would do this. 
	So in this method we give running processes a chance to finish:
		- yield execution until number of rest processes are reduced"
	| runningProcesses restProcesses |
	runningProcesses := self runningProcesses.
	runningProcesses ifEmpty: [ ^self ].
	(runningProcesses allSatisfy: [ :each | each priority = Processor activePriority ]) ifFalse: [ 
			"If there is any process with different priority than active one 
			we can't do anything to allow finish all of them"
			^self ].
	
	[Processor yield.
	restProcesses := runningProcesses reject: [ :each | each isTerminated ].
	restProcesses size = runningProcesses size or: [ restProcesses isEmpty ]] 
		whileFalse: [ runningProcesses := restProcesses ]
]

{ #category : #accessing }
ProcessMonitorTestService >> allowTestToLeaveProcesses [

	shouldFailTestLeavingProcesses := false

]

{ #category : #controlling }
ProcessMonitorTestService >> cleanUpAfterTest [
	super cleanUpAfterTest.
	
	forkedProcesses do: [:each | each terminate].
	forkedProcesses removeAll.
	testFailures removeAll.
	self enableBackgroudFailuresSuspension.
	self useDefaultFailingStrategyForRunningProcesses 
]

{ #category : #accessing }
ProcessMonitorTestService >> disableBackgroudFailuresSuspension [

	shouldSuspendBackgroundFailures := false
]

{ #category : #accessing }
ProcessMonitorTestService >> enableBackgroudFailuresSuspension [

	shouldSuspendBackgroundFailures := true
]

{ #category : #controlling }
ProcessMonitorTestService >> ensureNoBackgroundFailures [
	shouldSuspendBackgroundFailures ifFalse: [ 
		"We don't care about background failures if process monitor pass all of them"
		^self ].

	self suspendedBackgroundFailures ifEmpty: [ ^self ].

	[TestFailedByForkedProcess signalFrom: executionEnvironment] 
		on: UnhandledError do: [ :e | 
			self passBackgroundFailures.
			e pass]
]

{ #category : #controlling }
ProcessMonitorTestService >> ensureNoRunningProcesses [

	self allowRunningProcessesToFinish.	
	self runningProcesses ifEmpty: [ ^self ].
	
	shouldFailTestLeavingProcesses ifTrue: [ 	
		TestLeftRunningProcess signalFrom: executionEnvironment]
]

{ #category : #accessing }
ProcessMonitorTestService >> failTestLeavingProcesses [

	shouldFailTestLeavingProcesses := true
]

{ #category : #accessing }
ProcessMonitorTestService >> forkedProcesses [
	^ forkedProcesses
]

{ #category : #accessing }
ProcessMonitorTestService >> forkedProcesses: anObject [
	forkedProcesses := anObject
]

{ #category : #controlling }
ProcessMonitorTestService >> handleBackgroundException: anUnhandledException [
	
	self handleException: anUnhandledException.
	
	anUnhandledException pass
]

{ #category : #controlling }
ProcessMonitorTestService >> handleCompletedTest [
	super handleCompletedTest.
	
	self ensureNoBackgroundFailures.
	self ensureNoRunningProcesses
]

{ #category : #controlling }
ProcessMonitorTestService >> handleException: anException [
	super handleException: anException.
	
	anException manageTestProcessBy: self
]

{ #category : #controlling }
ProcessMonitorTestService >> handleNewProcess: aProcess [
	super handleNewProcess: aProcess.
	
	forkedProcesses add: aProcess.
	
	aProcess on: UnhandledException do: [ :err |
		self handleBackgroundException: err]
]

{ #category : #controlling }
ProcessMonitorTestService >> handleUnhandledException: anUnhandledException [
	self recordTestFailure: anUnhandledException.
	
	executionEnvironment isMainTestProcessActive ifTrue: [ ^self passBackgroundFailures ].
	
	shouldSuspendBackgroundFailures ifTrue: [
		self suspendBackgroundFailure: anUnhandledException]
]

{ #category : #initialization }
ProcessMonitorTestService >> initialize [
	super initialize.
	
	self enableBackgroudFailuresSuspension.
	self useDefaultFailingStrategyForRunningProcesses.
	forkedProcesses := WeakSet new.
	testFailures := OrderedIdentityDictionary new.
	 
]

{ #category : #testing }
ProcessMonitorTestService >> isTestProcessFailed: aProcess [

	^testFailures at: aProcess ifPresent: [ true ] ifAbsent: [ false ]
]

{ #category : #controlling }
ProcessMonitorTestService >> passBackgroundFailures [	
	self disableBackgroudFailuresSuspension.
		
	testFailures keys 
		select: [:each | each isSuspended ] 
		thenDo: [:each | each resume ]
]

{ #category : #controlling }
ProcessMonitorTestService >> recordTestFailure: anException [

	| activeProcess |
	activeProcess := Processor activeProcess.
	activeProcess isTerminating ifTrue: [ 
		"Do nothing for exceptions during process termination"
		^self ]. 
	
	testFailures at: activeProcess put: anException
]

{ #category : #initialization }
ProcessMonitorTestService >> resetFailures [
	testFailures removeAll
]

{ #category : #accessing }
ProcessMonitorTestService >> runningProcesses [
	"Suspended processes are not scheduled and therefore they are not considered as running"
	^ forkedProcesses reject: [ :each | each isTerminated or: [ each isSuspended ]]
]

{ #category : #accessing }
ProcessMonitorTestService >> shouldFailTestLeavingProcesses [
	^ shouldFailTestLeavingProcesses
]

{ #category : #accessing }
ProcessMonitorTestService >> shouldFailTestLeavingProcesses: anObject [
	shouldFailTestLeavingProcesses := anObject
]

{ #category : #accessing }
ProcessMonitorTestService >> shouldSuspendBackgroundFailures [
	^ shouldSuspendBackgroundFailures
]

{ #category : #accessing }
ProcessMonitorTestService >> shouldSuspendBackgroundFailures: anObject [
	shouldSuspendBackgroundFailures := anObject
]

{ #category : #controlling }
ProcessMonitorTestService >> suspendBackgroundFailure: anException [

	| activeProcess |
	activeProcess := Processor activeProcess.
	activeProcess isTerminating ifTrue: [ 
		"Do nothing if process is under termination"
		^self ]. 
	
	self recordTestFailure: anException.
	activeProcess suspend
]

{ #category : #accessing }
ProcessMonitorTestService >> suspendedBackgroundFailures [
	^ testFailures associationsSelect: [ :each | each key isSuspended ]
]

{ #category : #accessing }
ProcessMonitorTestService >> testFailures [
	^ testFailures
]

{ #category : #initialization }
ProcessMonitorTestService >> useDefaultFailingStrategyForRunningProcesses [

	shouldFailTestLeavingProcesses := self class shouldFailTestLeavingProcesses.

]
