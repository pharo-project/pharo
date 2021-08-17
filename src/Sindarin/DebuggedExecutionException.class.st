Class {
	#name : #DebuggedExecutionException,
	#superclass : #Error,
	#category : #'Sindarin-Exceptions'
}

{ #category : #testing }
DebuggedExecutionException >> isExceptionSignalledForDebuggedExecution [
	^ true
]
