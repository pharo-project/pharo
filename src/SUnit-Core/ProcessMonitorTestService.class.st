Class {
	#name : #ProcessMonitorTestService,
	#superclass : #TestExecutionService,
	#instVars : [
		'forkedProcesses',
		'shouldSuspendBackgroundFailures',
		'testFailures'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #'fuel support' }
ProcessMonitorTestService class >> fuelIgnoredInstanceVariableNames [
    ^#('forkedProcesses' 'suspendedBackgroundFailures' shouldSuspendBackgroundFailures)
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

{ #category : #controlling }
ProcessMonitorTestService >> cleanUpAfterTest [
	super cleanUpAfterTest.
	
	forkedProcesses do: [:each | each terminate].
	forkedProcesses removeAll.
	testFailures removeAll.
	self enableBackgroudFailuresSuspension
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
	
	TestLeftRunningProcess signalFrom: executionEnvironment
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
