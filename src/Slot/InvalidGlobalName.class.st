"
I am a warning signaled when trying to build a class with invalid name.
"
Class {
	#name : #InvalidGlobalName,
	#superclass : #ClassBuilderWarning,
	#category : #Slot-Exception
}

{ #category : #signalling }
InvalidGlobalName class >> signal: aMessage for: aName [
	self signal: aName asString, ': ', aMessage 
]
