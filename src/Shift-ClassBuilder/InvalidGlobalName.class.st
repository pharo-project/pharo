"
I am a warning signaled when trying to build a class with invalid name.
"
Class {
	#name : 'InvalidGlobalName',
	#superclass : 'Error',
	#category : 'Shift-ClassBuilder-Exceptions',
	#package : 'Shift-ClassBuilder',
	#tag : 'Exceptions'
}

{ #category : 'signalling' }
InvalidGlobalName class >> signal: aMessage for: aName [
	self signal: aName asString, ': ', aMessage
]
