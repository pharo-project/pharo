"
I am an error produced during serialization, signaled when trying to serialize an obsolete class as global. It is a prevention, because such class is lekely to be absent during materialization.
"
Class {
	#name : #FLObsolete,
	#superclass : #FLSerializationError,
	#category : #'Fuel-Core-Errors'
}

{ #category : #signaling }
FLObsolete class >> signalWithName: classOrTraitName [
	^ self signal: classOrTraitName printString, ' can not be serialized as global because it is obsolete.'
]
