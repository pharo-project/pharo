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
	| newProcess |
	newProcess := self newProcess: 'Test process' toImmediatelyExecute: aBlock.
	testService handleNewProcess: newProcess.
	newProcess resume.
	^newProcess
]

{ #category : #running }
ProcessMonitorTestServiceTest >> setUp [
	super setUp.
	
	processes := OrderedCollection new
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testAlwaysPassBackgroundHalt [
	<haltOrBreakpointForTesting>
	| process haltWasPassed |
	haltWasPassed := false.
	process := [ self halt ] newProcess name: 'test process'; priority: Processor activePriority + 1.
	testService handleNewProcess: process.
	
	process on: Halt do: [ :err | haltWasPassed := true ].
	process resume.
	
	self assert: process isTerminated.
	self assert: haltWasPassed.
	self assert: (testService forkedProcesses includes: process).
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
	process := [ error signal ] newProcess name: 'test process'; priority: Processor activePriority + 1.
	testService handleNewProcess: process.
	
	testService disableBackgroudFailuresSuspension.
	self deny: testService shouldSuspendBackgroundFailures.
	process on: UnhandledError do: [ :err | errorWasPassed := true ].
	process resume.
	
	self assert: process isTerminated.
	self assert: errorWasPassed
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testResumeFailedProcessWhenItFailsTestWithUnhandledError [
	| failedProcess executed |
	executed := false.
	failedProcess := self fork: [ Processor activeProcess suspend. executed := true ].
	testService suspendedBackgroundFailures at: failedProcess put: Error new.
	
	self runWithNoHandlers: [
		[testService handleCompletedTest] on: UnhandledError do: [ :err | ]
	].

	self assert: executed 
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testResumeFailedProcessesWhenTestFailureIsUnhandled [
	| failedProcess executed |
	executed := false.
	failedProcess := self fork: [ Processor activeProcess suspend. executed := true ].
	testService suspendedBackgroundFailures at: failedProcess put: Error new.
	self deny: executed.	

	testService handleUnhandledException: Error new.
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
