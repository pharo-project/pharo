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

{ #category : #controlling }
TestServiceExample >> cleanUpAfterTest [

	isCleanUpDone := true
]

{ #category : #accessing }
TestServiceExample >> forkedProcesses [
	^ forkedProcesses
]

{ #category : #controlling }
TestServiceExample >> handleCompletedTest [

	isCompletedTestHandled := true
]

{ #category : #controlling }
TestServiceExample >> handleNewProcess: aProcess [

	forkedProcesses add: aProcess
]

{ #category : #controlling }
TestServiceExample >> handleUnhandledException: anUnhandledException [

	unhandledExceptions add: anUnhandledException 
]

{ #category : #initialization }
TestServiceExample >> initialize [ 
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
