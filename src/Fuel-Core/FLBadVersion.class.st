"
I represent an error produced during materialization when the serialized version doesn't match the materializer's version (accessible via FLMaterializer>>version). A version is encoded in 16 bits and is enconded heading the serialized stream, after the signature.
"
Class {
	#name : #FLBadVersion,
	#superclass : #FLMaterializationError,
	#category : #'Fuel-Core-Errors'
}

{ #category : #signaling }
FLBadVersion class >> signalCurrentVersion: currentVersion streamVersion: streamVersion [

	^ self signal: 'Unexpected stream version ', streamVersion asString, ' where it should be ', currentVersion asString, '.'

]
