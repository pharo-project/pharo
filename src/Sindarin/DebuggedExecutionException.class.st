Class {
	#name : #DebuggedExecutionException,
	#superclass : #Error,
	#category : #Sindarin
}

{ #category : #testing }
DebuggedExecutionException >> isExceptionSignalledForDebuggedExecution [
	^ true
]
