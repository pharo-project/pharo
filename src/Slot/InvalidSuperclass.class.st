"
I am a warning signaled when trying to build a class with an invalid superclass.
"
Class {
	#name : #InvalidSuperclass,
	#superclass : #ClassBuilderWarning,
	#category : #Slot-Exception
}

{ #category : #signalling }
InvalidSuperclass class >> signal: aMessage for: anObject [
	self signal: anObject asString, ': ', aMessage 
]
