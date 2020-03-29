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
ProcessMonitorTestService >> cleanUpAfterTest: aTestCase [

	forkedProcesses do: [:each | each terminate].
	forkedProcesses removeAll.
	suspendedBackgroundFailures removeAll
]

{ #category : #accessing }
ProcessMonitorTestService >> disableBackgroudFailuresSuspension [

	shouldSuspendBackgroundFailures := false
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

	[self ensureNoBackgroundFailures.
	self ensureNoRunningProcesses]
		ensure: [ 
			"forkedProcesses do: [ :each | each terminate ].
			forkedProcesses removeAll.
			suspendedBackgroundFailures removeAll"]
]

{ #category : #controlling }
ProcessMonitorTestService >> handleNewProcess: aProcess [
	forkedProcesses add: aProcess.
	
	aProcess on: UnhandledError, Halt, Warning do: [ :err |
		"It is possible to fork a process with copy of stack from another process.
		It means that aProcess can be not an actual active process inside this block handler.
		So we should not reference temp aProcess and instead we should work with actual active process"
		self handleUnhandledException: err fromForkedProcess: Processor activeProcess]
]

{ #category : #controlling }
ProcessMonitorTestService >> handleUnhandledException: anError [
	
	self passSuspendedFailures 
]

{ #category : #controlling }
ProcessMonitorTestService >> handleUnhandledException: anException fromForkedProcess: aProcess [

	shouldSuspendBackgroundFailures ifFalse: [ ^anException pass ].
	(anException isKindOf: Halt) ifTrue: [ ^anException pass ].
	
	suspendedBackgroundFailures at: aProcess put: anException.
	aProcess isTerminating ifFalse: [aProcess suspend].
	^anException pass
]

{ #category : #initialization }
ProcessMonitorTestService >> initialize [
	super initialize.
	
	forkedProcesses := WeakSet new.
	suspendedBackgroundFailures := OrderedIdentityDictionary new.
	shouldSuspendBackgroundFailures := true
]

{ #category : #controlling }
ProcessMonitorTestService >> passSuspendedFailures [	
	shouldSuspendBackgroundFailures := false.
	
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

{ #category : #accessing }
ProcessMonitorTestService >> suspendedBackgroundFailures [
	^ suspendedBackgroundFailures
]

{ #category : #accessing }
ProcessMonitorTestService >> suspendedBackgroundFailures: anObject [
	suspendedBackgroundFailures := anObject
]
