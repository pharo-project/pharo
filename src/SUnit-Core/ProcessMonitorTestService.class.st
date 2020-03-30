Class {
	#name : #ProcessMonitorTestService,
	#superclass : #TestExecutionService,
	#instVars : [
		'forkedProcesses',
		'suspendedBackgroundFailures',
		'shouldSuspendBackgroundFailures'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #controlling }
ProcessMonitorTestService >> cleanUpAfterTest [

	forkedProcesses do: [:each | each terminate].
	forkedProcesses removeAll.
	suspendedBackgroundFailures removeAll.
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
	suspendedBackgroundFailures ifEmpty: [ ^self ].

	[TestFailedByForkedProcess signal] on: UnhandledError do: [ :e | 
		self passSuspendedFailures.
		e pass.
	]
]

{ #category : #controlling }
ProcessMonitorTestService >> ensureNoRunningProcesses [
	(forkedProcesses anySatisfy: [ :each | each isTerminated not ]) 
		ifFalse: [ ^self ].

	TestLeftRunningProcess signal
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
ProcessMonitorTestService >> handleCompletedTest [

	self ensureNoBackgroundFailures.
	self ensureNoRunningProcesses
]

{ #category : #controlling }
ProcessMonitorTestService >> handleNewProcess: aProcess [
	forkedProcesses add: aProcess.
	
	aProcess on: UnhandledError, Halt, Warning do: [ :err |
		self handleUnhandledException: err]
]

{ #category : #controlling }
ProcessMonitorTestService >> handleUnhandledException: anUnhandledException [
	| activeProcess |
	activeProcess := Processor activeProcess.
	(executionEnvironment isTestProcess: activeProcess)
		ifTrue: [ self passSuspendedFailures ]
		ifFalse: [ self handleUnhandledException: anUnhandledException fromForkedProcess: activeProcess]
]

{ #category : #controlling }
ProcessMonitorTestService >> handleUnhandledException: anException fromForkedProcess: aProcess [

	shouldSuspendBackgroundFailures ifFalse: [ ^anException pass ].
	(anException isKindOf: Halt) ifTrue: [ 
		self passSuspendedFailures.
		^anException pass ].
	
	suspendedBackgroundFailures at: aProcess put: anException.
	aProcess isTerminating ifFalse: [aProcess suspend].
	^anException pass
]

{ #category : #initialization }
ProcessMonitorTestService >> initialize [
	super initialize.
	
	self enableBackgroudFailuresSuspension.
	forkedProcesses := WeakSet new.
	suspendedBackgroundFailures := OrderedIdentityDictionary new.
	 
]

{ #category : #controlling }
ProcessMonitorTestService >> passSuspendedFailures [	
	self disableBackgroudFailuresSuspension.
		
	suspendedBackgroundFailures keys 
		reject: [:each | each isTerminated ] 
		thenDo: [:each | each resume ]
]

{ #category : #initialization }
ProcessMonitorTestService >> resetFailures [
	suspendedBackgroundFailures removeAll
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
		"Do nothing for exceptions during process termination"
		^self ]. 
	
	suspendedBackgroundFailures at: activeProcess put: anException.
	activeProcess suspend
]

{ #category : #accessing }
ProcessMonitorTestService >> suspendedBackgroundFailures [
	^ suspendedBackgroundFailures
]
