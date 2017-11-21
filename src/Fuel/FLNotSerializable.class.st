"
I represent an error which may happen while tracing in the graph an object that is forbidden of being serialized.
"
Class {
	#name : #FLNotSerializable,
	#superclass : #FLSerializationError,
	#category : #Fuel-Errors
}

{ #category : #signaling }
FLNotSerializable class >> signalWith: anObject [

 	self signal: 'Found a forbidden object in the graph: ', anObject printString.
]
