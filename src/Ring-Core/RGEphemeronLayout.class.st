Class {
	#name : 'RGEphemeronLayout',
	#superclass : 'RGPointerLayout',
	#category : 'Ring-Core-Kernel',
	#package : 'Ring-Core',
	#tag : 'Kernel'
}

{ #category : 'description' }
RGEphemeronLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton."

	^ #ephemeronSubclass:
]

{ #category : 'testing - types' }
RGEphemeronLayout >> isEphemeronLayout [

	^ true
]
