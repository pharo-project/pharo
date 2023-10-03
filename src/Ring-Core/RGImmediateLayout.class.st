Class {
	#name : 'RGImmediateLayout',
	#superclass : 'RGObjectLayout',
	#category : 'Ring-Core-Kernel',
	#package : 'Ring-Core',
	#tag : 'Kernel'
}

{ #category : 'description' }
RGImmediateLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton."

	^ #immediateSubclass:
]

{ #category : 'testing - types' }
RGImmediateLayout >> isImmediateLayout [

	^ true
]
