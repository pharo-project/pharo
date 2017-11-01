"
SUnit tests for class  Mutex
"
Class {
	#name : #MutexTest,
	#superclass : #TestCase,
	#instVars : [
		'mutex',
		'forkedProcesses'
	],
	#category : 'Kernel-Tests-Processes'
}

{ #category : #helpers }
MutexTest >> fork: aBlock [

	| newProcess |
	newProcess := aBlock forkNamed: testSelector.
	forkedProcesses add: newProcess.
	^newProcess
]

{ #category : #helpers }
MutexTest >> fork: aBlock at: priority [

	| newProcess |
	newProcess := aBlock forkAt: priority named: testSelector.
	forkedProcesses add: newProcess.
	^newProcess
]

{ #category : #running }
MutexTest >> setUp [
	super setUp.
	
	forkedProcesses := OrderedCollection new.
	mutex := Mutex new.
	
]

{ #category : #running }
MutexTest >> tearDown [
	forkedProcesses do: #terminate.
	
	super tearDown.
	
	
]

{ #category : #tests }
MutexTest >> testExecutionCriticalSection [

	| actual |
	actual := mutex critical: [ #result ].
	
	self assert: actual equals: #result
]

{ #category : #tests }
MutexTest >> testFailedCriticalSectionShouldUnblockWaitingOne [
	| lastCriticalExecuted semaphoreToHoldMutex processHoldingMutex |
	lastCriticalExecuted := false.
	semaphoreToHoldMutex := Semaphore new.
	
	processHoldingMutex := self fork: [[
		mutex critical: [ semaphoreToHoldMutex wait. "here we grab mutex and control it with semaphore"
		self error: 'critical section failed' ]] ifError: []].
	self waitLastProcessLock. "wait until first process grabs the mutex"

	self fork: [mutex critical: [ lastCriticalExecuted := true ]].
	self waitLastProcessLock.
	
	semaphoreToHoldMutex signal.
	self waitLastProcessTerminate.
	self assert: lastCriticalExecuted
]

{ #category : #tests }
MutexTest >> testTerminatedCriticalSectionShouldUnblockWaitingOne [
	| lastCriticalExecuted semaphoreToHoldMutex processHoldingMutex |
	lastCriticalExecuted := false.
	semaphoreToHoldMutex := Semaphore new.
	
	processHoldingMutex := self fork: [
		mutex critical: [ semaphoreToHoldMutex wait. "here we grab mutex and control it with semaphore"
		self error: 'should not happen' ]].
	self waitLastProcessLock.
	
	self fork: [mutex critical: [ lastCriticalExecuted := true ]].
	self waitLastProcessLock.
	
	processHoldingMutex terminate.
	self waitLastProcessTerminate.
	self assert: lastCriticalExecuted
]

{ #category : #tests }
MutexTest >> testTerminatingBlockedCriticalSectionShouldNotUnblockAnotherWaitingSection [
	| semaphoreToHoldMutex holdingCriticalExecutedFirst firstWaitingProcess lastCriticalExecuted |
	holdingCriticalExecutedFirst := false.
	semaphoreToHoldMutex := Semaphore new.
	lastCriticalExecuted := false.
	
	self fork: [
		mutex critical: [ semaphoreToHoldMutex wait. "here we grab mutex and control it with semaphore"
		holdingCriticalExecutedFirst := lastCriticalExecuted not ]].
	self waitLastProcessLock.
	
	firstWaitingProcess := self fork: [mutex critical: [ self error: 'should not happen' ]].
	self waitLastProcessLock.
	self fork: [mutex critical: [ lastCriticalExecuted := true]].
	self waitLastProcessLock.

	firstWaitingProcess terminate.
	self waitLastProcessLock. "check that last process is still waiting"
	semaphoreToHoldMutex signal. "here we resume first process execution"
	self waitLastProcessTerminate.
	self assert: holdingCriticalExecutedFirst.
	self assert: lastCriticalExecuted.
]

{ #category : #tests }
MutexTest >> testTerminatingBlockedCriticalWhichWasSignalledButNotResumedYet [
	| processWaitingForMutex firstCriticalExecuted lastCriticalExecuted semaphoreToHoldMutex |
	firstCriticalExecuted := false.
	lastCriticalExecuted := false.
	semaphoreToHoldMutex := Semaphore new.
	
	self fork: [ 
		mutex critical: [ semaphoreToHoldMutex wait.
		firstCriticalExecuted := true ]] at: Processor activeProcess priority + 1.	
	self waitLastProcessLock.
	"for second critical we choose small priority. So it can't be resumed automatically by scheduler in our scenario."
	processWaitingForMutex := self fork: [mutex critical: [ self error: 'should not happen' ]] at: Processor activeProcess priority - 1. 
	self waitLastProcessLock.
	self deny: firstCriticalExecuted.
	semaphoreToHoldMutex signal.
	self assert: firstCriticalExecuted.
	processWaitingForMutex terminate. "Here the process waits for mutex and being terminated at the point when mutex was already signalled but process was not resumed.
		Correct critical implementation should allow execution of new consequent criticals"
	self fork: [ mutex critical: [ lastCriticalExecuted := true ]].	
	self waitLastProcessTerminate.
	self assert: lastCriticalExecuted description: 'consequent last critical should be executed'
]

{ #category : #tests }
MutexTest >> testTwoCriticalsShouldWaitEachOther [

	| lastCriticalExecuted firstCriticalExecutedFirst semaphoreToHoldMutex |
	lastCriticalExecuted := false.
	firstCriticalExecutedFirst := false.
	semaphoreToHoldMutex := Semaphore new.
	
	self fork: [
		mutex critical: [ semaphoreToHoldMutex wait. "here we grab mutex and control it with semaphore"
		firstCriticalExecutedFirst := lastCriticalExecuted not ]].
	self waitLastProcessLock.
	
	self fork: [mutex critical: [ lastCriticalExecuted := true ]].
	self waitLastProcessLock.
	
	semaphoreToHoldMutex signal.
	self waitLastProcessTerminate.
	self assert: lastCriticalExecuted.
	self assert: firstCriticalExecutedFirst 
]

{ #category : #tests }
MutexTest >> testTwoRecursiveCriticalsShouldNotWaitEachOther [

	| executed |
	executed := false.
	
	self fork: [mutex critical: [ mutex critical: [ executed := true ]]].
	self waitLastProcessTerminate.
		
	self assert: executed
]

{ #category : #helpers }
MutexTest >> waitFor: aBlock [

	[ 10 milliSeconds wait. aBlock value ] whileFalse
]

{ #category : #helpers }
MutexTest >> waitLastProcessLock [

	self waitProcessLock: forkedProcesses last
]

{ #category : #helpers }
MutexTest >> waitLastProcessSuspend [

	self waitProcessSuspend: forkedProcesses last
]

{ #category : #helpers }
MutexTest >> waitLastProcessTerminate [

	self waitProcessTermination: forkedProcesses last
]

{ #category : #helpers }
MutexTest >> waitProcessLock: aProcess [

	self waitFor: [ aProcess suspendingList notEmpty ]
]

{ #category : #helpers }
MutexTest >> waitProcessSuspend: aProcess [

	self waitFor: [ aProcess isSuspended ]
]

{ #category : #helpers }
MutexTest >> waitProcessTermination: aProcess [

	self waitFor: [ aProcess isTerminated ]
]
