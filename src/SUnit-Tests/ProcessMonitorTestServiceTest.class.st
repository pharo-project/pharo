Class {
	#name : 'ProcessMonitorTestServiceTest',
	#superclass : 'TestExecutionEnvironmentTestCase',
	#category : 'SUnit-Tests-Core',
	#package : 'SUnit-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
ProcessMonitorTestServiceTest >> createTestService [

	^ProcessMonitorTestService new
]

{ #category : 'running' }
ProcessMonitorTestServiceTest >> fork: aBlock [
	"Here we simulate fork under TestExecutionEnvironment
	which passes new process to all test services"
	| newProcess |
	newProcess := self newProcess: 'Test process' toImmediatelyExecute: aBlock.
	testService handleNewProcess: newProcess.
	newProcess resume.
	^newProcess
]

{ #category : 'running' }
ProcessMonitorTestServiceTest >> isNextAddGrowing: aWeakSet [

	| probe |
	probe := aWeakSet copy.
	probe add: Object new.
	^ (probe instVarNamed: #array) size > (aWeakSet instVarNamed: #array) size
]

{ #category : 'running' }
ProcessMonitorTestServiceTest >> setUp [
	super setUp.

	"Following settings ensure that we are testing full behavior independently from default settings"
	testService failTestLeavingProcesses.
	testService terminateProcessesAfterTest.
	executionEnvironment activated
]

{ #category : 'running' }
ProcessMonitorTestServiceTest >> step: aProcess until: aBlock [
	"Step aProcess until aBlock answers true.
	Answer false if it does not happen within a bounded number of steps"

	10000 timesRepeat: [
		aBlock value ifTrue: [ ^ true ].
		aProcess isTerminated ifTrue: [ ^ false ].
		aProcess step ].
	^ aBlock value
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testAllowRunningBackgroundProcessesToFinish [

	| semaphore processIsDone process |
	semaphore := Semaphore new.
	processIsDone := false.
	process := self fork: [ semaphore wait. processIsDone := true].
	process priority: Processor activePriority.
	semaphore signal.
	self deny: processIsDone.

	testService handleCompletedTest.

	self assert: processIsDone
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testAllowRunningBackgroundProcessesToFinishButFailTestIfItCant [

	| semaphore process wasResumed |
	semaphore := Semaphore new.
	wasResumed := false.
	process := self fork: [ semaphore wait. wasResumed := true. 10 seconds wait].
	process priority: Processor activePriority.
	semaphore signal.
	self deny: wasResumed.

	self should: [testService handleCompletedTest] raise: TestLeftRunningProcess.
	self assert: wasResumed
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testCleanUpShouldNotTerminateAllFailedProcessesWhenSuchTerminationIsDisabled [
	| process  |
	process := self fork: [ testService suspendBackgroundFailure: Error new ].
	testService disableProcessesTermination.

	testService cleanUpAfterTest.

	self deny: process isTerminated.
	self assert: testService suspendedBackgroundFailures isEmpty.
	self assert: testService forkedProcesses isEmpty
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testCleanUpShouldNotTerminateAllRunningProcessesWhenSuchTerminationIsDisabled [
	| process  |
	process := self fork: [ 10 seconds wait ].
	testService disableProcessesTermination.

	testService cleanUpAfterTest.

	self deny: process isTerminated.
	self assert: testService forkedProcesses isEmpty
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testCleanUpShouldRestoreDefaultFailingLogicForRunningProcessesCase [

	testService shouldFailTestLeavingProcesses: ProcessMonitorTestService shouldFailTestLeavingProcesses not.

	testService cleanUpAfterTest.

	self assert: testService shouldFailTestLeavingProcesses equals: ProcessMonitorTestService shouldFailTestLeavingProcesses
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testCleanUpShouldRestoreProcessTerminationLogic [

	testService shouldTerminateProcesses: ProcessMonitorTestService shouldTerminateProcesses not.

	testService cleanUpAfterTest.

	self assert: testService shouldTerminateProcesses equals: ProcessMonitorTestService shouldTerminateProcesses
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testCleanUpShouldRestoreSuspensionLogic [

	testService passBackgroundFailures.
	self deny: testService shouldSuspendBackgroundFailures.

	testService cleanUpAfterTest.
	self assert: testService shouldSuspendBackgroundFailures
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testCleanUpShouldTerminateAllFailedProcesses [
	| process  |
	process := self fork: [ testService suspendBackgroundFailure: Error new ].

	testService cleanUpAfterTest.

	self assert: process isTerminated.
	self assert: testService suspendedBackgroundFailures isEmpty.
	self assert: testService forkedProcesses isEmpty
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testCleanUpShouldTerminateAllRunningProcesses [
	| process  |
	process := self fork: [ 10 seconds wait ].

	testService cleanUpAfterTest.

	self assert: process isTerminated.
	self assert: testService forkedProcesses isEmpty
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testCleanUpShouldWaitForProcessBeingAdded [
	"#handleNewProcess: runs in the forking process, which can be preempted by the test process running #cleanUpAfterTest.
	Here the forking process is stopped inside WeakSet>>#growTo: of forkedProcesses,
	after the new array was created but before it was stored, and the clean-up runs in a higher-priority process.
	The clean-up should wait until the process is added, and forkedProcesses should stay consistent"

	| forkedProcesses keptProcesses adderErrors adder cleanUpDone |
	forkedProcesses := testService forkedProcesses.
	keptProcesses := OrderedCollection new. "forkedProcesses is a weak set"
	[ self isNextAddGrowing: forkedProcesses ] whileFalse: [
		testService handleNewProcess: (keptProcesses add: [ ] newProcess) ].
	adderErrors := OrderedCollection new.
	adder := self
		newProcess: 'Adder'
		toImmediatelyExecute: [
			[ testService handleNewProcess: (keptProcesses add: [ ] newProcess) ]
				on: Error
				do: [ :error | adderErrors add: error ] ].
	self assert: (self step: adder until: [
			| context |
			context := adder suspendedContext.
			context method == (WeakSet >> #growTo:)
				and: [ context receiver == forkedProcesses
				and: [ context stackPtr > context numTemps
				and: [ (context top isKindOf: WeakArray)
				and: [ context top ~~ (forkedProcesses instVarNamed: #array) ] ] ] ] ]).

	cleanUpDone := Semaphore new.
	(self
		newProcess: 'Clean-up'
		toImmediatelyExecute: [ [ testService cleanUpAfterTest ] ensure: [ cleanUpDone signal ] ]) resume.
	self assert: (self step: adder until: [ adder isTerminated ]).
	self deny: (cleanUpDone waitTimeoutMilliseconds: 1000).

	self assert: adderErrors isEmpty description: [ 'Adding a process failed: ' , adderErrors first printString ].
	self assert: forkedProcesses isEmpty.
	self shouldnt: [ testService runningProcesses ] raise: Error
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testDisableRunningProcessesCleanupWhenPassBackgroundFailures [

	testService passBackgroundFailures.

	self deny: testService shouldTerminateProcesses
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testDoesNotRaiseForkedProcessFailureWhenFailuresWerePassedAndMainProcessAlsoFails [

	self fork: [ testService suspendBackgroundFailure: Error new.
		Processor activeProcess suspend "To emulate process under debugger" ].

	testService passBackgroundFailures.
	testService recordTestFailure: Error new.

	self shouldnt: [testService handleCompletedTest] raise: TestFailedByForkedProcess
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testDoesNotRaiseForkedProcessFailureWhenFailuresWerePassedAndProcessCompletes [

	| process |
	process := self fork: [ testService suspendBackgroundFailure: Error new ].

	testService passBackgroundFailures.

	self assert: process isTerminated description: 'process should be terminated now'.
	self shouldnt: [testService handleCompletedTest] raise: TestFailedByForkedProcess
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testDoesNotRaiseForkedProcessFailureWhenThereWasOnlyMainProcessFailure [

	testService handleException: Error new.

	self shouldnt: [testService handleCompletedTest] raise: TestFailedByForkedProcess
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testDoesNotRaiseLeftRunningProcessWhenItAllowsThemToBe [

	| process |
	process := self fork: [ 10 seconds wait ].
	self deny: process isTerminated.

	testService allowTestToLeaveProcesses.
	self shouldnt: [testService handleCompletedTest] raise: TestLeftRunningProcess.
	self deny: process isTerminated
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testDoesNotRaiseLeftRunningProcessWhenProcessWasOnlyCreated [

	testService handleNewProcess: [  ] newProcess.

	self shouldnt: [testService handleCompletedTest] raise: TestLeftRunningProcess
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testDoesNotRaiseLeftRunningProcessWhenThereWasMainProcessFailure [

	self fork: [ 10 seconds wait].

	testService handleException: Error new.

	self shouldnt: [testService handleCompletedTest] raise: TestLeftRunningProcess
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testFailTestWhenBackgroundFailureWasPassedButMainProcessCompletesSuccessfully [

	self fork: [ testService suspendBackgroundFailure: Error new.
		Processor activeProcess suspend "To emulate process under debugger" ].

	testService passBackgroundFailures.

	self should: [testService handleCompletedTest] raise: TestFailedByForkedProcess
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testFailTestWhenBackgroundProcessWasFailedDuringFinalTryToFinishItAtTestCompletionTime [
	"ProcessMonitor tries to allow the left running processes to terminate during the test completion.
	Such running processes can fail at this stage and this test is to cover this scenario.
	The complex assertion logic here is to reliably simulate this scenario.
	The error processing envolves many message sends and it increases the chances for the process to be preempted.
	Therefore a single iteration is not always enough to get the expected corner case.
	Thus the function under the test is repeated in the loop"

	| semaphore processIsDone process |
	semaphore := Semaphore new.
	processIsDone := false.
	process := self fork: [
		           semaphore wait.
		           processIsDone := true.
		           1 / 0 ].
	process priority: Processor activePriority.
	semaphore signal.
	self deny: processIsDone.

	[
	[ testService handleCompletedTest ]
		on: TestLeftRunningProcess
		do: [ :err | err retry ].
	self assert: false description: 'should fail with error' ]
		on: TestFailedByForkedProcess
		do: [ :err |  ].
	self assert: processIsDone
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testFailTestWhenItIsCompletedWithBackgroundFailures [

	self fork: [ testService suspendBackgroundFailure: Error new ].

	self should: [testService handleCompletedTest] raise: TestFailedByForkedProcess
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testFailTestWhenItIsCompletedWithBackgroundFailuresAndRunningProcesses [

	self fork: [ testService suspendBackgroundFailure: Error new ].
	self fork: [ 10 seconds wait ].

	self
		should: [
			[testService handleCompletedTest]
				on: TestFailedByForkedProcess do: [ :err | err resumeUnchecked: true ]]
		raise: TestLeftRunningProcess
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testFailTestWhenItIsCompletedWithRunningProcesses [

	self fork: [ 10 seconds wait ].

	self should: [testService handleCompletedTest] raise: TestLeftRunningProcess
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testGettingBackgroundFailuresFromEnvironment [
	| mainError backgroundError |
	mainError := Error new messageText: 'mainProcessError'.
	backgroundError := Error new messageText: 'backgroundError'.
	self fork: [ testService recordTestFailure: backgroundError ].
	testService recordTestFailure: mainError.

	self assert: executionEnvironment backgroundFailures equals: testService testBackgroundFailures.
	self assertCollection: executionEnvironment backgroundFailures asSet hasSameElements: {backgroundError}
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testGettingFailuresFromEnvironment [
	| mainError backgroundError |
	mainError := Error new messageText: 'mainProcessError'.
	backgroundError := Error new messageText: 'backgroundError'.
	self fork: [ testService recordTestFailure: backgroundError ].
	testService recordTestFailure: mainError.

	self assert: executionEnvironment failures equals: testService testFailures.
	self assertCollection: executionEnvironment failures asSet hasSameElements: {mainError. backgroundError}
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testGettingForkedProcessesFromEnvironment [

	| process |
	process := self fork: [ ].

	self assert: executionEnvironment forkedProcesses equals: testService forkedProcesses.
	self assert: executionEnvironment forkedProcesses asArray equals: {process}
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testGettingServiceFromEnvironment [

	self assert: executionEnvironment processMonitor identicalTo: testService
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testGettingServiceFromTestCase [

	| actual |
	executionEnvironment beActiveDuring: [
		actual := self executionProcessMonitor
	].

	self assert: actual identicalTo: testService
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testHasDefaultFailingStrategyForRunningProcesses [

	testService := ProcessMonitorTestService new.

	self assert: testService shouldFailTestLeavingProcesses isNotNil.
	self assert: testService shouldFailTestLeavingProcesses equals: ProcessMonitorTestService shouldFailTestLeavingProcesses
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testHasDefaultTerminationStrategyForRunningProcesses [

	testService := ProcessMonitorTestService new.

	self assert: testService shouldTerminateProcesses isNotNil.
	self assert: testService shouldTerminateProcesses equals: ProcessMonitorTestService shouldTerminateProcesses
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testHasEmptyForkedProcessesByDefault [

	self assert: testService forkedProcesses isEmpty
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testHasEmptySuspendedBackgroundFailuresByDefault [

	self assert: testService suspendedBackgroundFailures isEmpty
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testIgnoreDirtyTestErrors [

	testService handleException: TestLeftRunningProcess new.
	self assert: testService testFailures isEmpty.

	testService handleException: TestFailedByForkedProcess new.
	self assert: testService testFailures isEmpty
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testIsMainTestProcessFailed [

	executionEnvironment activated.

	self deny: testService isMainTestProcessFailed.

	testService recordTestFailure: Error new.
	self assert: executionEnvironment isMainTestProcessFailed
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testIsTestProcessFailed [

	| process |
	self deny: (testService isTestProcessFailed: #process).

	process := self fork: [testService recordTestFailure: Error new].
	self assert: (testService isTestProcessFailed: process).
	self deny: (testService isTestProcessFailed: #anotherProcess)
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testLockAllowsCallingServiceMethods [
	"The mutex is re-entrant: service methods called inside #lock: do not wait for the lock.
	The block runs in a helper process, so that a deadlock would fail the test instead of blocking it"

	| process done runningProcesses error |
	process := [ ] newProcess.
	done := Semaphore new.
	(self
		newProcess: 'Lock owner'
		toImmediatelyExecute: [
			[ runningProcesses := testService lock: [ :aService |
				aService handleNewProcess: process.
				aService isMainTestProcessFailed.
				aService runningProcesses ] ]
				on: Error
				do: [ :anError | error := anError ].
			done signal ]) resume.

	self deny: (done waitTimeoutMilliseconds: 1000).
	self assert: error isNil description: [ 'Lock owner failed: ' , error printString ].
	self assert: (testService forkedProcesses includes: process).
	self assert: runningProcesses isEmpty
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testLockAnswersBlockValueWithServiceAsArgument [

	| argument |
	self
		assert: (testService lock: [ :aService |
			argument := aService.
			#result ])
		equals: #result.
	self assert: argument identicalTo: testService
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testLockMakesOtherProcessesWait [
	"A higher-priority process registering a process while the test process holds the lock waits until the lock is released"

	| process adder added |
	process := [ ] newProcess.
	added := Semaphore new.
	testService lock: [ :aService |
		adder := self
			newProcess: 'Adder'
			toImmediatelyExecute: [
				aService handleNewProcess: process.
				added signal ].
		adder resume.
		self assert: adder isWaiting.
		self deny: (aService forkedProcesses includes: process) ].

	self deny: (added waitTimeoutMilliseconds: 1000).
	self assert: (testService forkedProcesses includes: process)
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testPassBackgroundFailuresWhenSuspensionLogicIsDisabled [
	| process error errorWasPassed |
	errorWasPassed := false.
	error := Error new messageText: 'test error'.
	process := self fork: [ Processor activeProcess suspend. error signal ].

	testService disableBackgroudFailuresSuspension.
	self deny: testService shouldSuspendBackgroundFailures.

	process on: UnhandledError do: [ :err | errorWasPassed := true ].
	process resume.

	self assert: errorWasPassed.
	self assert: process isTerminated description: 'process should be terminated now'
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testRecordMainTestProcessError [

	| error |
	error := Error new.

	testService handleException: error.

	self
		assert: (testService testFailures at: Processor activeProcess)
		equals: error
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testRecordMainTestProcessUnhandledError [

	| error |
	error := UnhandledError new.

	testService handleException: error.

	self
		assert: (testService testFailures at: Processor activeProcess)
		equals: error
]

{ #category : 'tests' }
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

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testResumeFailedProcessesWhenTestFailureIsUnhandled [
	| executed |
	executed := false.
	self fork: [ testService suspendBackgroundFailure: Error new. executed := true ].
	self deny: executed.

	testService handleException: Error new. "UnhandledError always started with original error signal"
	testService handleException: UnhandledError new.
	self assert: executed.
	"Unhandled exception opens debugger and therefore any new errors should not be suspended"
	self deny: testService shouldSuspendBackgroundFailures
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testShouldSuspendBackgroundFailuresByDefault [

	self assert: testService shouldSuspendBackgroundFailures
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testStoreAllForkedProcesses [

	| process |
	process := self fork: [ ].

	self assert: (testService forkedProcesses includes: process)
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testSuspendBackgroundError [

	| process error suspendedError |
	error := Error new messageText: 'test error'.

	process := self fork: [ error signal ].

	self assert: process isSuspended.
	suspendedError := testService suspendedBackgroundFailures at: process.
	self assert: suspendedError class equals: UnhandledError.
	self assert: suspendedError exception identicalTo: error
]

{ #category : 'tests' }
ProcessMonitorTestServiceTest >> testSuspendBackgroundWarning [

	| process warning suspendedError |
	warning := Warning new.
	process := self fork: [ warning signal ].

	self assert: process isSuspended.
	suspendedError := testService suspendedBackgroundFailures at: process.
	self assert: suspendedError class equals: UnhandledError.
	self assert: suspendedError exception identicalTo: warning
]
