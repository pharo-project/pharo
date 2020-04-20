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
	"Suspended processes are not scheduled and therefore they are not considered like a running"
	(forkedProcesses allSatisfy: [ :each | each isTerminated or: [ each isSuspended ] ]) 
		ifTrue: [ ^self ].

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
