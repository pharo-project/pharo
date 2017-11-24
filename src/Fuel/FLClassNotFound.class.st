"
I represent an error produced during materialization when a serialized class or trait name doesn't exist.
"
Class {
	#name : #FLClassNotFound,
	#superclass : #FLMaterializationError,
	#category : #'Fuel-Errors'
}

{ #category : #signaling }
FLClassNotFound class >> signalWithName: className [
	^ self signal: 'Class named ', className printString, ' not found.'
]
