"
I represent an error produced during materialization when a serialized method in a class or trait name doesn't exist (at Smalltalk globals).
"
Class {
	#name : #FLMethodNotFound,
	#superclass : #FLMaterializationError,
	#category : #'Fuel-Core-Errors'
}

{ #category : #signaling }
FLMethodNotFound class >> signalWith: aGlobalName and: aSelector [
	^ self signal: 'Method ', aGlobalName, '>>#', aSelector, ' not found.'
]
