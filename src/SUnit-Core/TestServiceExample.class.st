Class {
	#name : #TestServiceExample,
	#superclass : #TestExecutionService,
	#instVars : [
		'forkedProcesses',
		'isCleanUpDone',
		'isCompletedTestHandled',
		'unhandledExceptions'
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
TestServiceExample >> handleNewProcess: aProcess [
	super handleNewProcess: aProcess.
	
	forkedProcesses add: aProcess
]

{ #category : #controlling }
TestServiceExample >> handleUnhandledException: anUnhandledException [
	super handleUnhandledException: anUnhandledException.
	
	unhandledExceptions add: anUnhandledException 
]

{ #category : #initialization }
TestServiceExample >> initialize [ 
	super initialize.
	
	forkedProcesses := OrderedCollection new.
	unhandledExceptions := OrderedCollection new.
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
TestServiceExample >> unhandledExceptions [
	^ unhandledExceptions
]
