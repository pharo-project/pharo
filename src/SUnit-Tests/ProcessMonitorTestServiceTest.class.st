Class {
	#name : #ProcessMonitorTestServiceTest,
	#superclass : #TestExecutionEnvironmentTestCase,
	#category : #'SUnit-Tests-Core'
}

{ #category : #running }
ProcessMonitorTestServiceTest >> createTestService [

	^ProcessMonitorTestService new
]

{ #category : #running }
ProcessMonitorTestServiceTest >> fork: aBlock [
	"Here we simulate fork under TestExecutionEnvironment 
	which passes new process to all test services"
	| newProcess |
	newProcess := self newProcess: 'Test process' toImmediatelyExecute: aBlock.
	testService handleNewProcess: newProcess.
	newProcess resume.
	^newProcess
]

{ #category : #running }
ProcessMonitorTestServiceTest >> setUp [ 
	super setUp.
	
	executionEnvironment activated 
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testAlwaysPassBackgroundHalt [
	| process haltWasPassed halt |
	haltWasPassed := false.
	halt := Halt new.
	process := self fork: [ Processor activeProcess suspend. halt signal ].
	testService handleNewProcess: process.
	
	process on: Halt do: [ :actual | self assert: actual equals: halt. haltWasPassed := true ].
	process resume.
	
	self assert: process isTerminated.
	self assert: haltWasPassed.
	self deny: (testService suspendedBackgroundFailures includesKey: process)
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testCleanForkedProcessesWhenTestIsCompletedAndBackgroundFailuresAreTerminated [
	| failedProcess |
	failedProcess := self fork: [ 10 seconds wait ].
	testService suspendedBackgroundFailures at: failedProcess put: Error new.
	
	[ [testService handleCompletedTest] 
		on: TestFailedByForkedProcess do: [:err | Processor activeProcess terminate]
	] forkAt: Processor activePriority + 1.

	self assert: failedProcess isTerminated.
	self assert: testService forkedProcesses isEmpty.
	self assert: testService suspendedBackgroundFailures isEmpty
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testCleanForkedProcessesWhenTestIsCompletedAndFailureIsResumed [
	| failedProcess |
	failedProcess := self fork: [ 10 seconds wait ].
	testService suspendedBackgroundFailures at: failedProcess put: Error new.
	
	[testService handleCompletedTest] 
		on: TestFailedByForkedProcess do: [ :err | 	err resume]. 
			
	self assert: testService forkedProcesses isEmpty.
	self assert: testService suspendedBackgroundFailures isEmpty
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testFailTestWhenItIsCompletedWithBackgroundFailures [

	| process |
	process := self fork: [ 10 seconds wait ].
	testService suspendedBackgroundFailures at: process put: Error new.
	
	self should: [testService handleCompletedTest] raise: TestFailedByForkedProcess
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testFailTestWhenItIsCompletedWithBackgroundFailuresAndRunningProcesses [

	| failedProcess |
	failedProcess := self fork: [ 10 seconds wait ].
	testService suspendedBackgroundFailures at: failedProcess put: Error new.
	self fork: [ 10 seconds wait ].
	
	self 
		should: [
			[testService handleCompletedTest] 
				on: TestFailedByForkedProcess do: [ :err | err resume ]]
		raise: TestLeftRunningProcess
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testFailTestWhenItIsCompletedWithRunningProcesses [

	self fork: [ 10 seconds wait ].
	
	self should: [testService handleCompletedTest] raise: TestLeftRunningProcess
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testHasEmptyForkedProcessesByDefault [

	self assert: testService forkedProcesses isEmpty
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testHasEmptySuspendedBackgroundFailuresByDefault [

	self assert: testService suspendedBackgroundFailures isEmpty
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testPassBackgroundFailuresWhenSuspensionLogicIsDisabled [

	| process error errorWasPassed |
	errorWasPassed := false.
	error := Error new messageText: 'test error'.
	process := self fork: [ Processor activeProcess suspend. error signal ].	
	
	testService disableBackgroudFailuresSuspension.
	self deny: testService shouldSuspendBackgroundFailures.
	
	process on: UnhandledError do: [ :err | errorWasPassed := true ].
	process resume.
	
	self assert: process isTerminated.
	self assert: errorWasPassed
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testResumeFailedProcessWhenItFailsTestWithUnhandledError [
	"Test with background failures is failing with TestFailedByForkedProcess.
	In case when it is unhandled (when we debug the test) it should resume all background failures"
	| executed failedProcess |
	executed := false.
	failedProcess := self fork: [testService suspendBackgroundFailure: Error new. executed := true ].
	self assert: failedProcess isSuspended.
	
	self runWithNoHandlers: [
		[testService handleCompletedTest] on: UnhandledError do: [ :err |
			self deny: failedProcess isSuspended]
	].

	self assert: executed 
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testResumeFailedProcessesWhenHaltIsSignaled [
	| executed |
	executed := false.
	self fork: [ testService suspendBackgroundFailure: Error new. executed := true ].

	testService handleUnhandledException: Halt new. 
	
	self assert: executed.
	self deny: testService shouldSuspendBackgroundFailures
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testResumeFailedProcessesWhenHaltIsSignaledInBackground [
	| processWithHalt executed |
	executed := false.
	self fork: [ testService suspendBackgroundFailure: Error new. executed := true ].
	processWithHalt := self fork: [ Processor activeProcess suspend. Halt new signal ].
	
	processWithHalt on: Halt do: [ :actual | ].
	processWithHalt resume.
	
	self assert: executed.
	self deny: testService shouldSuspendBackgroundFailures
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testResumeFailedProcessesWhenTestFailureIsUnhandled [
	| executed |
	executed := false.
	self fork: [ testService suspendBackgroundFailure: Error new. executed := true ].
	self deny: executed.	

	testService handleUnhandledException: UnhandledError new.
	self assert: executed.
	"Unhandled exception opens debugger and therefore any new errors should not be suspended"
	self deny: testService shouldSuspendBackgroundFailures 
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testShouldSuspendBackgroundFailuresByDefault [

	self assert: testService shouldSuspendBackgroundFailures 
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testStoreAllForkedProcesses [

	| process |
	process := self fork: [ ].
	
	self assert: (testService forkedProcesses includes: process)
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testSuspendBackgroundError [

	| process error suspendedError |
	error := Error new messageText: 'test error'.
	
	process := self fork: [ error signal ].
	
	self assert: process isSuspended.
	suspendedError := testService suspendedBackgroundFailures at: process.
	self assert: suspendedError class equals: UnhandledError. 
	self assert: suspendedError exception identicalTo: error.
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testSuspendBackgroundWarning [
	
	| process |
	process := self fork: [ Warning signal ].
	
	self assert: process isSuspended.
	self assert: (testService suspendedBackgroundFailures at: process) class equals: Warning
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testTerminateAllFailedProcessesWhenTestIsTerminated [
	| process  |
	process := self fork: [ 10 seconds wait ].
	testService suspendedBackgroundFailures at: process put: Error new.
	
	testService handleTerminatedTest: #someTestCase.
	
	self assert: process isTerminated.		
	self assert: testService forkedProcesses isEmpty.
	self assert: testService suspendedBackgroundFailures isEmpty
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testTerminateAllForkedProcessesWhenTestIsTerminated [
	| process  |
	process := self fork: [ 10 seconds wait ].
	
	testService handleTerminatedTest: #someTestCase.
	
	self assert: process isTerminated.		
	self assert: testService forkedProcesses isEmpty.
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testTerminateFailedProcessWhenTestFailureIsHandled [
	| process |
	process := self fork: [ 10 seconds wait ].
	testService suspendedBackgroundFailures at: process put: Error new.
	
	[testService handleCompletedTest.
	self deny: process isTerminated] 
		on: TestFailedByForkedProcess 
		do: [ :err | "here we handle test failure with no action" ].
		
	self assert: process isTerminated
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testTerminateLeftRunningProcessWhenTestFailureIsHandled [

	| process |
	process := self fork: [ 10 seconds wait ].
	
	[testService handleCompletedTest.
	self deny: process isTerminated] 
		on: TestLeftRunningProcess 
		do: [ :err | "here we handle test failure with no action" ].
		
	self assert: process isTerminated
]
