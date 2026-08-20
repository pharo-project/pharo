"
SUnit tests for monitors
"
Class {
	#name : 'MonitorTest',
	#superclass : 'TestCase',
	#instVars : [
		'monitor',
		'forkedProcesses'
	],
	#category : 'Kernel-Tests-Processes',
	#package : 'Kernel-Tests',
	#tag : 'Processes'
}

{ #category : 'running - helpers' }
MonitorTest >> fork: aBlock [
	| newProcess |
	newProcess := aBlock forkNamed: testSelector.
	forkedProcesses add: newProcess.
	^newProcess
]

{ #category : 'running - helpers' }
MonitorTest >> fork: aBlock at: priority [

	| newProcess |
	newProcess := aBlock forkAt: priority named: testSelector.
	forkedProcesses add: newProcess.
	^newProcess
]

{ #category : 'running' }
MonitorTest >> setUp [
	super setUp.

	forkedProcesses := OrderedCollection new.
	monitor := Monitor new
]

{ #category : 'running' }
MonitorTest >> tearDown [
	forkedProcesses do: #terminate.

	super tearDown
]

{ #category : 'tests - examples' }
MonitorTest >> testExample1 [
	| producer1 producer2 monitor goal work counter goalReached finished |
	goal := (1 to: 1000) asOrderedCollection.
	work := OrderedCollection new.
	counter := 0.
	goalReached := false.
	finished := Semaphore new.
	monitor := Monitor new.

	producer1 := [ [ monitor
		critical: [ monitor waitUntil: [ counter \\ 5 = 0 ].
			goalReached or: [ work add: (counter := counter + 1) ].
			goalReached := counter >= goal size.
			monitor signal ].
	goalReached ] whileFalse.
	finished signal ].

	producer2 := [ [ monitor
		critical: [ monitor waitWhile: [ counter \\ 5 = 0 ].
			goalReached or: [ work add: (counter := counter + 1) ].
			goalReached := counter >= goal size.
			monitor signal ].
	goalReached ] whileFalse.
	finished signal ].

	producer1 forkAt: Processor userBackgroundPriority.
	producer2 forkAt: Processor userBackgroundPriority.

	finished
		wait;
		wait.
	self assert: goal equals: work
]

{ #category : 'tests - examples' }
MonitorTest >> testExample2 [
	"Here is a second version that does not use a semaphore to inform the
	forking process about termination of both forked processes"

	| producer1 producer2 monitor goal work counter goalReached activeProducers |
	goal := (1 to: 1000) asOrderedCollection.
	work := OrderedCollection new.
	counter := 0.
	goalReached := false.
	activeProducers := 0.
	monitor := Monitor new.

	producer1 := [ monitor critical: [ activeProducers := activeProducers + 1 ].
	[ monitor
		critical: [ monitor waitUntil: [ counter \\ 5 = 0 ].
			goalReached or: [ work add: (counter := counter + 1) ].
			" Transcript show: 'P1  '; show: counter printString; show: '  ';
       show: activeProducers printString; cr."
			goalReached := counter >= goal size.
			monitor signal ].
	goalReached ] whileFalse.
	monitor
		critical: [ activeProducers := activeProducers - 1.
			monitor signal: #finish ] ].

	producer2 := [ monitor critical: [ activeProducers := activeProducers + 1 ].

	[ monitor
		critical: [ monitor waitWhile: [ counter \\ 5 = 0 ].
			goalReached or: [ work add: (counter := counter + 1) ].
			goalReached := counter >= goal size.
			monitor signal ].
	goalReached ] whileFalse.
	monitor
		critical: [ activeProducers := activeProducers - 1.
			monitor signal: #finish ] ].

	producer1 forkAt: Processor userBackgroundPriority.
	producer2 forkAt: Processor userBackgroundPriority.


	monitor critical: [ monitor waitUntil: [ activeProducers = 0 & goalReached ] for: #finish ].

	self assert: goal equals: work
]

{ #category : 'tests' }
MonitorTest >> testExecutionCriticalSection [

	| actual |
	actual := monitor critical: [ #result ].

	self assert: actual equals: #result
]

{ #category : 'tests' }
MonitorTest >> testFailedCriticalSectionShouldUnblockWaitingOne [
	| lastCriticalExecuted semaphoreToHoldMutex |
	lastCriticalExecuted := false.
	semaphoreToHoldMutex := Semaphore new.
	self fork: [
		[
		monitor critical: [
			semaphoreToHoldMutex wait. "here we grab mutex and control it with semaphore"
			self error: 'critical section failed' ] ] onErrorDo: [  ] ].
	self waitLastProcessLock. "wait until first process grabs the mutex"

	self fork: [ monitor critical: [ lastCriticalExecuted := true ] ].
	self waitLastProcessLock.

	semaphoreToHoldMutex signal.
	self waitLastProcessTerminate.
	self assert: lastCriticalExecuted
]

{ #category : 'tests' }
MonitorTest >> testTerminatedCriticalSectionShouldUnblockWaitingOne [
	| lastCriticalExecuted semaphoreToHoldMutex processHoldingMutex |
	lastCriticalExecuted := false.
	semaphoreToHoldMutex := Semaphore new.

	processHoldingMutex := self fork: [
		monitor critical: [ semaphoreToHoldMutex wait. "here we grab mutex and control it with semaphore"
		self error: 'should not happen' ]].
	self waitLastProcessLock.

	self fork: [monitor critical: [ lastCriticalExecuted := true ]].
	self waitLastProcessLock.

	processHoldingMutex terminate.
	self waitLastProcessTerminate.
	self assert: lastCriticalExecuted
]

{ #category : 'tests' }
MonitorTest >> testTerminatingBlockedCriticalSectionShouldNotUnblockAnotherWaitingSection [
	| semaphoreToHoldMutex holdingCriticalExecutedFirst firstWaitingProcess lastCriticalExecuted |
	holdingCriticalExecutedFirst := false.
	semaphoreToHoldMutex := Semaphore new.
	lastCriticalExecuted := false.
	self fork: [
		monitor critical: [ semaphoreToHoldMutex wait. "here we grab mutex and control it with semaphore"
		holdingCriticalExecutedFirst := lastCriticalExecuted not ]].
	self waitLastProcessLock.

	firstWaitingProcess := self fork: [monitor critical: [ self error: 'should not happen' ]].
	self waitLastProcessLock.
	self fork: [monitor critical: [ lastCriticalExecuted := true]].
	self waitLastProcessLock.
	firstWaitingProcess terminate.
	self waitLastProcessLock. "check that last process is still waiting"
	semaphoreToHoldMutex signal. "here we resume first process execution"
	self waitLastProcessTerminate.
	self assert: holdingCriticalExecutedFirst.
	self assert: lastCriticalExecuted
]

{ #category : 'tests' }
MonitorTest >> testTerminatingBlockedCriticalWhichWasSignalledButNotResumedYet [
	| processWaitingForMutex firstCriticalExecuted lastCriticalExecuted semaphoreToHoldMutex |
	firstCriticalExecuted := false.
	lastCriticalExecuted := false.
	semaphoreToHoldMutex := Semaphore new.

	self fork: [
		monitor critical: [ semaphoreToHoldMutex wait.
		firstCriticalExecuted := true ]] at: Processor activeProcess priority + 1.
	self waitLastProcessLock.
	"for second critical we choose small priority. So it can't be resumed automatically by scheduler in our scenario."
	processWaitingForMutex := self fork: [monitor critical: [ self error: 'should not happen' ]] at: Processor activeProcess priority - 1.
	self waitLastProcessLock.
	self deny: firstCriticalExecuted.
	semaphoreToHoldMutex signal.
	self assert: firstCriticalExecuted.
	processWaitingForMutex terminate. "Here the process waits for monitor and being terminated at the point when monitor was already signalled but process was not resumed.
		Correct critical implementation should allow execution of new consequent criticals"
	self fork: [ monitor critical: [ lastCriticalExecuted := true ]].
	self waitLastProcessTerminate.
	self assert: lastCriticalExecuted description: 'consequent last critical should be executed'
]

{ #category : 'tests' }
MonitorTest >> testTwoCriticalsShouldWaitEachOther [

	| lastCriticalExecuted firstCriticalExecutedFirst semaphoreToHoldMutex |
	lastCriticalExecuted := false.
	firstCriticalExecutedFirst := false.
	semaphoreToHoldMutex := Semaphore new.

	self fork: [
		monitor critical: [ semaphoreToHoldMutex wait. "here we grab monitor and control it with semaphore"
		firstCriticalExecutedFirst := lastCriticalExecuted not ]].
	self waitLastProcessLock.

	self fork: [monitor critical: [ lastCriticalExecuted := true ]].
	self waitLastProcessLock.

	semaphoreToHoldMutex signal.
	self waitLastProcessTerminate.
	self assert: lastCriticalExecuted.
	self assert: firstCriticalExecutedFirst
]

{ #category : 'tests' }
MonitorTest >> testTwoRecursiveCriticalsShouldNotWaitEachOther [

	| executed |
	executed := false.

	self fork: [monitor critical: [ monitor critical: [ executed := true ]]].
	self waitLastProcessTerminate.

	self assert: executed
]

{ #category : 'tests' }
MonitorTest >> testWaitMaxMilliseconds [

	monitor := Monitor new.
	monitor critical: [ monitor waitMaxMilliseconds: 10 ]
]

{ #category : 'running - helpers' }
MonitorTest >> waitFor: aBlock [

	[ 10 milliSeconds wait. aBlock value ] whileFalse
]

{ #category : 'running - helpers' }
MonitorTest >> waitLastProcessLock [

	self waitProcessLock: forkedProcesses last
]

{ #category : 'running - helpers' }
MonitorTest >> waitLastProcessSuspend [

	self waitProcessSuspend: forkedProcesses last
]

{ #category : 'running - helpers' }
MonitorTest >> waitLastProcessTerminate [

	self waitProcessTermination: forkedProcesses last
]

{ #category : 'running - helpers' }
MonitorTest >> waitProcessLock: aProcess [

	self waitFor: [ aProcess suspendingList isEmptyOrNil not ]
]

{ #category : 'running - helpers' }
MonitorTest >> waitProcessSuspend: aProcess [

	self waitFor: [ aProcess isSuspended ]
]

{ #category : 'running - helpers' }
MonitorTest >> waitProcessTermination: aProcess [

	self waitFor: [ aProcess isTerminated ]
]
