"
I am signaled when the name of a Slot is an invalid variable name.
"
Class {
	#name : 'InvalidSlotName',
	#superclass : 'Error',
	#category : 'Kernel-CodeModel-Exceptions',
	#package : 'Kernel-CodeModel',
	#tag : 'Exceptions'
}

{ #category : 'signalling' }
InvalidSlotName class >> signalFor: aSymbol [
	self signal: aSymbol asString, ' is an invalid name for a slot'
]
