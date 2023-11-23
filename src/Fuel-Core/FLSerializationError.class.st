"
I represent an error happened during serialization.
"
Class {
	#name : 'FLSerializationError',
	#superclass : 'FLError',
	#category : 'Fuel-Core-Errors',
	#package : 'Fuel-Core',
	#tag : 'Errors'
}

{ #category : 'exceptioninstantiator' }
FLSerializationError class >> signal: signalerText [

	^ super signal: 'Serialization error. ', signalerText
]
