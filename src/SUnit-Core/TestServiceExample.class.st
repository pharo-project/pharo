Class {
	#name : #TestServiceExample,
	#superclass : #TestExecutionService,
	#instVars : [
		'forkedProcesses',
		'handledExceptions',
		'completedTestIsHandled',
		'cleanUpIsDone'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #controlling }
TestServiceExample >> cleanUpAfterTest [

	cleanUpIsDone := true
]

{ #category : #accessing }
TestServiceExample >> cleanUpIsDone [
	^ cleanUpIsDone
]

{ #category : #accessing }
TestServiceExample >> cleanUpIsDone: anObject [
	cleanUpIsDone := anObject
]

{ #category : #accessing }
TestServiceExample >> completedTestIsHandled [
	^ completedTestIsHandled
]

{ #category : #accessing }
TestServiceExample >> completedTestIsHandled: anObject [
	completedTestIsHandled := anObject
]

{ #category : #accessing }
TestServiceExample >> forkedProcesses [
	^ forkedProcesses
]

{ #category : #accessing }
TestServiceExample >> forkedProcesses: anObject [
	forkedProcesses := anObject
]

{ #category : #controlling }
TestServiceExample >> handleCompletedTest [

	completedTestIsHandled := true
]

{ #category : #controlling }
TestServiceExample >> handleNewProcess: aProcess [

	forkedProcesses add: aProcess
]

{ #category : #controlling }
TestServiceExample >> handleUnhandledException: anUnhandledException [

	handledExceptions add: anUnhandledException 
]

{ #category : #accessing }
TestServiceExample >> handledExceptions [
	^ handledExceptions
]

{ #category : #accessing }
TestServiceExample >> handledExceptions: anObject [
	handledExceptions := anObject
]

{ #category : #initialization }
TestServiceExample >> initialize [ 
	forkedProcesses := OrderedCollection new.
	handledExceptions := OrderedCollection new.
	cleanUpIsDone := false.
	completedTestIsHandled := false
]
