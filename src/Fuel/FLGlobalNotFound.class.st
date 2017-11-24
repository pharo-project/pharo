"
I represent an error produced during materialization when a serialized global name doesn't exist (at Smalltalk globals).
"
Class {
	#name : #FLGlobalNotFound,
	#superclass : #FLMaterializationError,
	#category : #'Fuel-Errors'
}

{ #category : #signaling }
FLGlobalNotFound class >> signalWithName: aName [
	^ self signal: 'Global named ', aName printString, ' not found.'
]
