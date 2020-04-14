Class {
	#name : #TestServiceExample,
	#superclass : #TestExecutionService,
	#instVars : [
		'forkedProcesses',
		'isCleanUpDone',
		'isCompletedTestHandled',
		'signaledExceptions'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #testing }
TestServiceExample class >> isEnabledByDefault [
	^false
]

{ #category : #controlling }
TestServiceExample >> cleanUpAfterTest [
	super cleanUpAfterTest.
	
	isCleanUpDone := true
]

{ #category : #accessing }
TestServiceExample >> forkedProcesses [
	^ forkedProcesses
]

{ #category : #controlling }
TestServiceExample >> handleCompletedTest [
	super handleCompletedTest.
	
	isCompletedTestHandled := true
]

{ #category : #controlling }
TestServiceExample >> handleException: anUnhandledException [
	super handleException: anUnhandledException.
	
	signaledExceptions add: anUnhandledException 
]

{ #category : #controlling }
TestServiceExample >> handleNewProcess: aProcess [
	super handleNewProcess: aProcess.
	
	forkedProcesses add: aProcess
]

{ #category : #initialization }
TestServiceExample >> initialize [ 
	super initialize.
	
	forkedProcesses := OrderedCollection new.
	signaledExceptions := OrderedCollection new.
	isCleanUpDone := false.
	isCompletedTestHandled := false
]

{ #category : #accessing }
TestServiceExample >> isCleanUpDone [
	^ isCleanUpDone
]

{ #category : #accessing }
TestServiceExample >> isCompletedTestHandled [
	^ isCompletedTestHandled
]

{ #category : #accessing }
TestServiceExample >> signaledExceptions [
	^ signaledExceptions
]
