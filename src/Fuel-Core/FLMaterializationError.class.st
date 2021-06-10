"
I represent an error happened during materialization.
"
Class {
	#name : #FLMaterializationError,
	#superclass : #FLError,
	#category : #'Fuel-Core-Errors'
}

{ #category : #exceptioninstantiator }
FLMaterializationError class >> signal: signalerText [

	^ super signal: 'Materialization error. ', signalerText
	
]

{ #category : #testing }
FLMaterializationError >> isResumable [
	"Allow users to ignore errors if they know what they are doing"
	^ true
]
