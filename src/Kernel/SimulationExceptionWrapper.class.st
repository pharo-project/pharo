"
Notify about an exception while simulating an execution
"
Class {
	#name : #SimulationExceptionWrapper,
	#superclass : #Exception,
	#instVars : [
		'exception'
	],
	#category : #Kernel-Exceptions
}

{ #category : #signalling }
SimulationExceptionWrapper class >> signalForException: anError [

	^ self new
		exception: anError
]

{ #category : #accessing }
SimulationExceptionWrapper >> exception [

	^ exception
]

{ #category : #accessing }
SimulationExceptionWrapper >> exception: anObject [

	exception := anObject
]
