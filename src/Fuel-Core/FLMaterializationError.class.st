"
I represent an error happened during materialization.
"
Class {
	#name : #FLMaterializationError,
	#superclass : #FLError,
	#category : 'Fuel-Core-Errors'
}

{ #category : #exceptioninstantiator }
FLMaterializationError class >> signal: signalerText [

	^ super signal: 'Materialization error. ', signalerText
	
]
