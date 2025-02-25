Class {
	#name : 'RGByteLayout',
	#superclass : 'RGBitsLayout',
	#category : 'Ring-Core-Kernel',
	#package : 'Ring-Core',
	#tag : 'Kernel'
}

{ #category : 'description' }
RGByteLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton."

	^ #variableByteSubclass:
]

{ #category : 'testing - types' }
RGByteLayout >> isByteLayout [

	^ true
]
