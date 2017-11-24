"
I am signaled when the name of a Slot is an invalid variable name.
"
Class {
	#name : #InvalidSlotName,
	#superclass : #ClassBuilderWarning,
	#category : #'Slot-Exception'
}

{ #category : #signalling }
InvalidSlotName class >> signalFor: aSymbol [
	self signal: aSymbol asString, ' is an invalid name for a slot'
]
