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

	"Following settings ensure that we are testing full behavior independently from default settings"
	testService failTestLeavingProcesses.
	testService terminateProcessesAfterTest.
	executionEnvironment activated
]

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
ProcessMonitorTestServiceTest >> testAlwaysPassBackgroundHalt [
	| process haltWasPassed halt |
	self flag: 'it fails runKernelTests on bootstrap image due to Process bug #6745'.
	self skip.

	haltWasPassed := false.
	halt := Halt new.
	process := self fork: [ Processor activeProcess suspend. halt signal ].
	testService handleNewProcess: process.

	process on: Halt do: [ :actual |
		actual == halt ifFalse: [ actual pass ]. "it allows to debug if it works wrongly"
		self assert: actual equals: halt. haltWasPassed := true ].
	process resume.

	self assert: haltWasPassed.
	self assert: process isTerminated description: 'process should be terminated now'.
	self deny: (testService suspendedBackgroundFailures includesKey: process)
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testCleanUpShouldNotTerminateAllFailedProcessesWhenSuchTerminationIsDisabled [
	| process  |
	process := self fork: [ testService suspendBackgroundFailure: Error new ].
	testService disableProcessesTermination.

	testService cleanUpAfterTest.

	self deny: process isTerminated.
	self assert: testService suspendedBackgroundFailures isEmpty.
	self assert: testService forkedProcesses isEmpty
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testCleanUpShouldNotTerminateAllRunningProcessesWhenSuchTerminationIsDisabled [
	| process  |
	process := self fork: [ 10 seconds wait ].
	testService disableProcessesTermination.

	testService cleanUpAfterTest.

	self deny: process isTerminated.
	self assert: testService forkedProcesses isEmpty
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testCleanUpShouldRestoreDefaultFailingLogicForRunningProcessesCase [

	testService shouldFailTestLeavingProcesses: ProcessMonitorTestService shouldFailTestLeavingProcesses not.

	testService cleanUpAfterTest.

	self assert: testService shouldFailTestLeavingProcesses equals: ProcessMonitorTestService shouldFailTestLeavingProcesses
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testCleanUpShouldRestoreProcessTerminationLogic [

	testService shouldTerminateProcesses: ProcessMonitorTestService shouldTerminateProcesses not.

	testService cleanUpAfterTest.

	self assert: testService shouldTerminateProcesses equals: ProcessMonitorTestService shouldTerminateProcesses
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testCleanUpShouldRestoreSuspensionLogic [

	testService passBackgroundFailures.
	self deny: testService shouldSuspendBackgroundFailures.

	testService cleanUpAfterTest.
	self assert: testService shouldSuspendBackgroundFailures
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testCleanUpShouldTerminateAllFailedProcesses [
	| process  |
	process := self fork: [ testService suspendBackgroundFailure: Error new ].

	testService cleanUpAfterTest.

	self assert: process isTerminated.
	self assert: testService suspendedBackgroundFailures isEmpty.
	self assert: testService forkedProcesses isEmpty
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testCleanUpShouldTerminateAllRunningProcesses [
	| process  |
	process := self fork: [ 10 seconds wait ].

	testService cleanUpAfterTest.

	self assert: process isTerminated.
	self assert: testService forkedProcesses isEmpty
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testDisableRunningProcessesCleanupWhenPassBackgroundFailures [

	testService passBackgroundFailures.

	self deny: testService shouldTerminateProcesses
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testDoesNotRaiseForkedProcessFailureWhenFailuresWerePassedAndMainProcessAlsoFails [

	self fork: [ testService suspendBackgroundFailure: Error new.
		Processor activeProcess suspend "To emulate process under debugger" ].

	testService passBackgroundFailures.
	testService recordTestFailure: Error new.

	self shouldnt: [testService handleCompletedTest] raise: TestFailedByForkedProcess
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testDoesNotRaiseForkedProcessFailureWhenFailuresWerePassedAndProcessCompletes [

	| process |
	self flag: 'it fails runKernelTests on bootstrap image due to Process bug #6745'.
	self skip.
	process := self fork: [ testService suspendBackgroundFailure: Error new ].

	testService passBackgroundFailures.

	self assert: process isTerminated description: 'process should be terminated now'.
	self shouldnt: [testService handleCompletedTest] raise: TestFailedByForkedProcess
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testDoesNotRaiseForkedProcessFailureWhenThereWasOnlyMainProcessFailure [

	testService handleException: Error new.

	self shouldnt: [testService handleCompletedTest] raise: TestFailedByForkedProcess
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testDoesNotRaiseLeftRunningProcessWhenItAllowsThemToBe [

	| process |
	process := self fork: [ 10 seconds wait ].
	self deny: process isTerminated.

	testService allowTestToLeaveProcesses.
	self shouldnt: [testService handleCompletedTest] raise: TestLeftRunningProcess.
	self deny: process isTerminated
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testDoesNotRaiseLeftRunningProcessWhenProcessWasOnlyCreated [

	testService handleNewProcess: [  ] newProcess.

	self shouldnt: [testService handleCompletedTest] raise: TestLeftRunningProcess
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testDoesNotRaiseLeftRunningProcessWhenThereWasMainProcessFailure [

	self fork: [ 10 seconds wait].

	testService handleException: Error new.

	self shouldnt: [testService handleCompletedTest] raise: TestLeftRunningProcess
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testFailTestWhenBackgroundFailureWasPassedButMainProcessCompletesSuccessfully [

	self fork: [ testService suspendBackgroundFailure: Error new.
		Processor activeProcess suspend "To emulate process under debugger" ].

	testService passBackgroundFailures.

	self should: [testService handleCompletedTest] raise: TestFailedByForkedProcess
]

{ #category : #tests }
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
	process := self fork: [ semaphore wait. processIsDone := true. 1/0 ].
	process priority: Processor activePriority.
	semaphore signal.
	self deny: processIsDone.

	self should: [
		[
			[testService handleCompletedTest] on: TestLeftRunningProcess do: [ :err | err retry ].
			self assert: false description: 'should fail with error'.
		] on: TestFailedByForkedProcess do: [:err | ]
	] notTakeMoreThan: 2 seconds.
	self assert: processIsDone
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testFailTestWhenItIsCompletedWithBackgroundFailures [

	self fork: [ testService suspendBackgroundFailure: Error new ].

	self should: [testService handleCompletedTest] raise: TestFailedByForkedProcess
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testFailTestWhenItIsCompletedWithBackgroundFailuresAndRunningProcesses [

	self fork: [ testService suspendBackgroundFailure: Error new ].
	self fork: [ 10 seconds wait ].

	self
		should: [
			[testService handleCompletedTest]
				on: TestFailedByForkedProcess do: [ :err | err resumeUnchecked: true ]]
		raise: TestLeftRunningProcess
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testFailTestWhenItIsCompletedWithRunningProcesses [

	self fork: [ 10 seconds wait ].

	self should: [testService handleCompletedTest] raise: TestLeftRunningProcess
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testGettingBackgroundFailuresFromEnvironment [
	| mainError backgroundError |
	mainError := Error new messageText: 'mainProcessError'.
	backgroundError := Error new messageText: 'backgroundError'.
	self fork: [ testService recordTestFailure: backgroundError ].
	testService recordTestFailure: mainError.

	self assert: executionEnvironment backgroundFailures equals: testService testBackgroundFailures.
	self assertCollection: executionEnvironment backgroundFailures asSet hasSameElements: {backgroundError}
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testGettingFailuresFromEnvironment [
	| mainError backgroundError |
	mainError := Error new messageText: 'mainProcessError'.
	backgroundError := Error new messageText: 'backgroundError'.
	self fork: [ testService recordTestFailure: backgroundError ].
	testService recordTestFailure: mainError.

	self assert: executionEnvironment failures equals: testService testFailures.
	self assertCollection: executionEnvironment failures asSet hasSameElements: {mainError. backgroundError}
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testGettingForkedProcessesFromEnvironment [

	| process |
	process := self fork: [ ].

	self assert: executionEnvironment forkedProcesses equals: testService forkedProcesses.
	self assert: executionEnvironment forkedProcesses asArray equals: {process}
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testGettingServiceFromEnvironment [

	self assert: executionEnvironment processMonitor identicalTo: testService
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testGettingServiceFromTestCase [

	| actual |
	executionEnvironment beActiveDuring: [
		actual := self executionProcessMonitor
	].

	self assert: actual identicalTo: testService
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testHasDefaultFailingStrategyForRunningProcesses [

	testService := ProcessMonitorTestService new.

	self assert: testService shouldFailTestLeavingProcesses notNil.
	self assert: testService shouldFailTestLeavingProcesses equals: ProcessMonitorTestService shouldFailTestLeavingProcesses
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testHasDefaultTerminationStrategyForRunningProcesses [

	testService := ProcessMonitorTestService new.

	self assert: testService shouldTerminateProcesses notNil.
	self assert: testService shouldTerminateProcesses equals: ProcessMonitorTestService shouldTerminateProcesses
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
ProcessMonitorTestServiceTest >> testIgnoreDirtyTestErrors [

	testService handleException: TestLeftRunningProcess new.
	self assert: testService testFailures isEmpty.

	testService handleException: TestFailedByForkedProcess new.
	self assert: testService testFailures isEmpty
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testIsMainTestProcessFailed [

	executionEnvironment activated.

	self deny: testService isMainTestProcessFailed.

	testService recordTestFailure: Error new.
	self assert: executionEnvironment isMainTestProcessFailed
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testIsTestProcessFailed [

	| process |
	self deny: (testService isTestProcessFailed: #process).

	process := self fork: [testService recordTestFailure: Error new].
	self assert: (testService isTestProcessFailed: process).
	self deny: (testService isTestProcessFailed: #anotherProcess)
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testPassBackgroundFailuresWhenSuspensionLogicIsDisabled [
	| process error errorWasPassed |
	self flag: 'it fails runKernelTests on bootstrap image due to Process bug #6745'.
	self skip.
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

{ #category : #tests }
ProcessMonitorTestServiceTest >> testRecordMainTestProcessError [

	| error |
	error := Error new.

	testService handleException: error.

	self
		assert: (testService testFailures at: Processor activeProcess)
		equals: error
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testRecordMainTestProcessUnhandledError [

	| error |
	error := UnhandledError new.

	testService handleException: error.

	self
		assert: (testService testFailures at: Processor activeProcess)
		equals: error
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

	testService handleException: Halt new.

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

	testService handleException: Error new. "UnhandledError always started with original error signal"
	testService handleException: UnhandledError new.
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
	self assert: suspendedError exception identicalTo: error
]

{ #category : #tests }
ProcessMonitorTestServiceTest >> testSuspendBackgroundWarning [

	| process warning suspendedError |
	warning := Warning new.
	process := self fork: [ warning signal ].

	self assert: process isSuspended.
	suspendedError := testService suspendedBackgroundFailures at: process.
	self assert: suspendedError class equals: UnhandledError.
	self assert: suspendedError exception identicalTo: warning
]
