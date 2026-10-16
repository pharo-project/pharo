"
I monitor all processes forked during tests and track all exceptions signaled from them.

1) The main goal is to ensure that if there was an error from the background process the test will never be green. 
For example I make following test fail:

	testExample
		[ 1/0 ] fork.
		self assert: true.

TestFailedByForkedProcess will be signaled at the end of such test. And if user will debug this test then two debuggers will be opened for each error (for main and background processes).
		
2) The second my purpose is to ensure that no running processes will be left after the test.
The background process can be more tricky than example and it can be forked by domain code.
Out of the test such processes could affect the overall system behavior. 
They could affect other tests results. If they fail during other tests it will hide the fact that the issue is related to the original test which will make debugging hard.

So at the end of every test I perform a cleanup (#cleanUpAfterTest) where I terminate all running processes. This behavior can be disabled globaly in settings (System/SUnit) or in test method and setUp:

	self executionProcessMonitor disableProcessTermination 
	
To enable it use: 

	self executionProcessMonitor terminateProcessesAfterTest

In addition I implement special logic to fail the test which left running processes after run.
So the process termination is not hidden. 
For example following test will fail:

	testExample
		[ 10 seconds wait ] fork.
		self assert: true.

TestLeftRunningProcess will be signaled at the end of test.
For compatibility this feature is disabled by default. And this test would not fail. But it can be enabled globaly in settings (System/SUnit) or in test and setUp:	 

	self executionProcessMonitor failTestLeavingProcesses.

And to disable it use: 

	self executionProcessMonitor allowTestToLeaveProcesses.

3) I can be completely disabled globaly in settings (System/SUnit) or in test and setUp:

	self executionProcessMonitor disable 
	 
Finally at the end of every test I perform a cleanup (#cleanUpAfterTest) where I restore all my default settings. So the next test will be started with default behaviour.

Implementation details:

1. Background failures

I am a kind of TestExecutionService and therefore I intercept all processes forked during the test (#handleNewProcess:).
I set up default exception handler for them to catch all unhandled exceptions. UnhandledException's are supposed to open a debugger at the end of processing. I postpone this logic by suspending the failing process. So when error is signaled from the background process no debugger will be opened immediately.
At the end of test (#handleCompletedTest) I check all suspended failures and if they exist I fail the test with TestFailedByForkedProcess signal.
In interactive mode when user debugs the test I pass all suspended failures together with this signal. So multiple debuggers will be opened to show each error.

The interactive scenario is detected using UnhandledException logic: unhandled error from the main test process or halt from anywhere mean the ""debugger fallback"" and therefore it is a good time to show the rest of failures from other processes. So in that case I pass all suspended failures to debug all of them together.

2. Failing strategy for left processes

I collect all forked processes (#forkedProcesses) and at the end of tests I have an option (#shouldFailTestLeavingProcesses) to fail the test if these processes are still running. TestLeftRunningProcess would be a final error for the test in that case.
I implement a little logic to make this failing strategy more usable and not break the test in trivial cases. For eaxmple following pattern is quite common in tests: 

	testExample 
		[ ""some work here"". Processor yield ] fork.
		Processor yield.
		self assert: true.

The #yield's here are to describe what could happen in real world: processes can be preemted at any time due to various activities from other image processes (with different priorities). And a simple #fork in tests like this one will not be able to terminate at the end of test while it is ""almost done"".
The method #allowRunningProcessesToFinish is to solve this issue. It performs own ""Processor yield"" logic to allow such background processes to take a control and finish the execution. 

3. One more detail: 
In uninteractive mode I signal TestFailedByForkedProcess even if there was a domain error from the test. So two exceptions can be signaled for single test run.
It allows to log all errors signaled during the test: one from the main process and others from all background processes using this signal (notice that I suspend all background failures and they do not log themselves as a default feedback).

Internal Representation and Key Implementation Points.

    Instance Variables
	forkedProcesses:		<OrderedCollection<Process>>
	testFailures:		<OrderedIdentityDictionary<Process, Exception>>
	shouldFailTestLeavingProcesses:		<Boolean>
	shouldSuspendBackgroundFailures:		<Boolean>
"
Class {
	#name : 'ProcessMonitorTestService',
	#superclass : 'TestExecutionService',
	#instVars : [
		'forkedProcesses',
		'testFailures',
		'shouldSuspendBackgroundFailures',
		'shouldFailTestLeavingProcesses',
		'shouldTerminateProcesses'
	],
	#classInstVars : [
		'shouldFailTestLeavingProcesses',
		'shouldTerminateProcesses'
	],
	#category : 'SUnit-Core-Kernel',
	#package : 'SUnit-Core',
	#tag : 'Kernel'
}

{ #category : 'fuel support' }
ProcessMonitorTestService class >> fuelIgnoredInstanceVariableNames [
    ^#('forkedProcesses' 'suspendedBackgroundFailures')
]

{ #category : 'settings' }
ProcessMonitorTestService class >> settingsForFailingStrategyOn: aBuilder [

	(aBuilder setting: #shouldFailTestLeavingProcesses)
		target: self;
		parent: self name ;
		default: false;
		label: 'Fail tests which left running processes' ;
		description: 'The test will be failed if it left running processes after the run.
For example following test will fail:
	testExample
		[ 10 seconds wait ] fork.
		self assert: true.
Tests should not leave the system dirty.
And this setting is to facilitate such rule relatively to background processes forked during the test.
See ProcessMonitorTestService comment for more details'
]

{ #category : 'settings' }
ProcessMonitorTestService class >> settingsForTerminationStrategyOn: aBuilder [

	(aBuilder setting: #shouldTerminateProcesses)
		target: self;
		parent: self name ;
		default: true;
		label: 'Terminate all processes after the test' ;
		description: 'At the end of each test all forked processes will be terminated except when we are running in interractive mode under debugger.
When debugger opens all control is up to the user.
But when we are running the test suite to get results it is a good practice to remove all garbage after the test.
This setting is about doing it in automatic way.
See ProcessMonitorTestService comment for more details'
]

{ #category : 'settings' }
ProcessMonitorTestService class >> settingsOn: aBuilder [
	super settingsOn: aBuilder.

	self
		settingsForFailingStrategyOn: aBuilder;
		settingsForTerminationStrategyOn: aBuilder
]

{ #category : 'accessing' }
ProcessMonitorTestService class >> shouldFailTestLeavingProcesses [
	^shouldFailTestLeavingProcesses ifNil: [ shouldFailTestLeavingProcesses := false ]
]

{ #category : 'accessing' }
ProcessMonitorTestService class >> shouldFailTestLeavingProcesses: aBoolean [
	shouldFailTestLeavingProcesses := aBoolean
]

{ #category : 'accessing' }
ProcessMonitorTestService class >> shouldTerminateProcesses [
	^ shouldTerminateProcesses ifNil: [ shouldTerminateProcesses := true ]
]

{ #category : 'accessing' }
ProcessMonitorTestService class >> shouldTerminateProcesses: anObject [
	shouldTerminateProcesses := anObject
]

{ #category : 'controlling' }
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

{ #category : 'accessing' }
ProcessMonitorTestService >> allowTestToLeaveProcesses [

	shouldFailTestLeavingProcesses := false
]

{ #category : 'controlling' }
ProcessMonitorTestService >> cleanUpAfterTest [
	super cleanUpAfterTest.

	shouldTerminateProcesses ifTrue: [
		self terminateRunningProcesses].
	forkedProcesses removeAll.
	testFailures removeAll.
	self enableBackgroudFailuresSuspension.
	self useDefaultFailingStrategyForRunningProcesses.
	self useDefaultTerminationStrategyForRunningProcesses
]

{ #category : 'accessing' }
ProcessMonitorTestService >> disableBackgroudFailuresSuspension [

	shouldSuspendBackgroundFailures := false
]

{ #category : 'accessing' }
ProcessMonitorTestService >> disableProcessesTermination [

	shouldTerminateProcesses := false
]

{ #category : 'accessing' }
ProcessMonitorTestService >> enableBackgroudFailuresSuspension [

	shouldSuspendBackgroundFailures := true
]

{ #category : 'controlling' }
ProcessMonitorTestService >> ensureNoBackgroundFailures [
	self isMainTestProcessFailed & self shouldPassBackgroundFailures ifTrue: [
		"We don't need extra error about failed process when all errors are shown to the user:
			- if they were not suspended and were passed
			- if main test process is also fail (and therefore test fails anyway)"
		^self ].
	self suspendedBackgroundFailures ifEmpty: [ ^self ].

	"COMMENT FOR DEBUGGER STOPPED HERE:
	TestFailedByForkedProcess notifies about background failures.
	Test failed because forked process failed (even if main test process was completed without errors).
	See ProcessMonitorTestService comment for more details"
	[TestFailedByForkedProcess signalFrom: executionEnvironment]
		on: UnhandledError do: [ :e |
			self passBackgroundFailures.
			e pass]
]

{ #category : 'controlling' }
ProcessMonitorTestService >> ensureNoRunningProcesses [
	"If test was already failed due to an error in the main test process
	we do not need an extra failure about left processes"
	self isMainTestProcessFailed ifTrue: [ ^self ].
	self runningProcesses ifEmpty: [ ^self ].
	shouldFailTestLeavingProcesses ifFalse: [ ^self ].

	"COMMENT FOR DEBUGGER STOPPED HERE:
	TestLeftRunningProcess notifies that forked processes are still running when test completes.
	Test failed because test left the system in dirty state.
	Left running processes can affect the result of other tests
	and even the general system behavior after test run.
	This protection can be disabled globaly in settings or in test method and setUp:
		self executionProcessMonitor allowTestToLeaveProcesses
	See ProcessMonitorTestService comment for more details"
	TestLeftRunningProcess signalFrom: executionEnvironment
]

{ #category : 'accessing' }
ProcessMonitorTestService >> failTestLeavingProcesses [

	shouldFailTestLeavingProcesses := true
]

{ #category : 'accessing' }
ProcessMonitorTestService >> forkedProcesses [
	^ forkedProcesses
]

{ #category : 'accessing' }
ProcessMonitorTestService >> forkedProcesses: anObject [
	forkedProcesses := anObject
]

{ #category : 'controlling' }
ProcessMonitorTestService >> handleBackgroundException: anUnhandledException [

	self handleException: anUnhandledException.

	self shouldPassBackgroundFailures ifTrue: [ anUnhandledException pass ]
]

{ #category : 'controlling' }
ProcessMonitorTestService >> handleCompletedTest [
	super handleCompletedTest.

	self allowRunningProcessesToFinish.
	self ensureNoBackgroundFailures.
	self ensureNoRunningProcesses
]

{ #category : 'controlling' }
ProcessMonitorTestService >> handleException: anException [
	super handleException: anException.

	anException manageTestProcessBy: self
]

{ #category : 'controlling' }
ProcessMonitorTestService >> handleNewProcess: aProcess [
	super handleNewProcess: aProcess.

	forkedProcesses add: aProcess.

	aProcess on: UnhandledException do: [ :err |
		self handleBackgroundException: err]
]

{ #category : 'controlling' }
ProcessMonitorTestService >> handleUnhandledException: anUnhandledException [
	self recordTestFailure: anUnhandledException.

	executionEnvironment isMainTestProcessActive ifTrue: [ ^self passBackgroundFailures ].

	shouldSuspendBackgroundFailures ifTrue: [
		self suspendBackgroundFailure: anUnhandledException]
]

{ #category : 'initialization' }
ProcessMonitorTestService >> initialize [
	super initialize.

	self enableBackgroudFailuresSuspension.
	self useDefaultFailingStrategyForRunningProcesses.
	self useDefaultTerminationStrategyForRunningProcesses.
	forkedProcesses := WeakSet new.
	testFailures := OrderedIdentityDictionary new
]

{ #category : 'testing' }
ProcessMonitorTestService >> isMainTestProcessFailed [
	^self isTestProcessFailed: executionEnvironment mainTestProcess
]

{ #category : 'testing' }
ProcessMonitorTestService >> isTestProcessFailed: aProcess [

	^testFailures at: aProcess ifPresent: [ true ] ifAbsent: [ false ]
]

{ #category : 'controlling' }
ProcessMonitorTestService >> passBackgroundFailures [
	self disableBackgroudFailuresSuspension.
	self disableProcessesTermination.

	testFailures keys
		select: [:each | each isSuspended ]
		thenDo: [:each | each resume ]
]

{ #category : 'controlling' }
ProcessMonitorTestService >> recordTestFailure: anException [

	| activeProcess |
	activeProcess := Processor activeProcess.
	activeProcess isTerminating ifTrue: [
		"Do nothing for exceptions during process termination"
		^self ].

	testFailures at: activeProcess put: anException
]

{ #category : 'accessing' }
ProcessMonitorTestService >> runningProcesses [
	"Suspended processes are not scheduled and therefore they are not considered as running"
	^ forkedProcesses reject: [ :each | each isTerminated or: [ each isSuspended ]]
]

{ #category : 'accessing' }
ProcessMonitorTestService >> shouldFailTestLeavingProcesses [
	^ shouldFailTestLeavingProcesses
]

{ #category : 'accessing' }
ProcessMonitorTestService >> shouldFailTestLeavingProcesses: anObject [
	shouldFailTestLeavingProcesses := anObject
]

{ #category : 'testing' }
ProcessMonitorTestService >> shouldPassBackgroundFailures [
	^shouldSuspendBackgroundFailures not
]

{ #category : 'accessing' }
ProcessMonitorTestService >> shouldSuspendBackgroundFailures [
	^ shouldSuspendBackgroundFailures
]

{ #category : 'accessing' }
ProcessMonitorTestService >> shouldSuspendBackgroundFailures: anObject [
	shouldSuspendBackgroundFailures := anObject
]

{ #category : 'accessing' }
ProcessMonitorTestService >> shouldTerminateProcesses [
	^ shouldTerminateProcesses
]

{ #category : 'accessing' }
ProcessMonitorTestService >> shouldTerminateProcesses: anObject [
	shouldTerminateProcesses := anObject
]

{ #category : 'controlling' }
ProcessMonitorTestService >> suspendBackgroundFailure: anException [

	| activeProcess |
	activeProcess := Processor activeProcess.
	activeProcess isTerminating ifTrue: [
		"Do nothing if process is under termination"
		^self ].

	self recordTestFailure: anException.
	activeProcess suspend
]

{ #category : 'accessing' }
ProcessMonitorTestService >> suspendedBackgroundFailures [
	^ testFailures associationsSelect: [ :each | each key isSuspended ]
]

{ #category : 'accessing' }
ProcessMonitorTestService >> terminateProcessesAfterTest [

	shouldTerminateProcesses := true
]

{ #category : 'controlling' }
ProcessMonitorTestService >> terminateRunningProcesses [
	forkedProcesses do: [:each | each terminate]
]

{ #category : 'accessing' }
ProcessMonitorTestService >> testBackgroundFailures [
	^ testFailures copy
		removeKey: executionEnvironment mainTestProcess ifAbsent: [ ];
		yourself
]

{ #category : 'accessing' }
ProcessMonitorTestService >> testFailures [
	^ testFailures
]

{ #category : 'initialization' }
ProcessMonitorTestService >> useDefaultFailingStrategyForRunningProcesses [

	shouldFailTestLeavingProcesses := self class shouldFailTestLeavingProcesses
]

{ #category : 'initialization' }
ProcessMonitorTestService >> useDefaultTerminationStrategyForRunningProcesses [
	shouldTerminateProcesses := self class shouldTerminateProcesses
]
