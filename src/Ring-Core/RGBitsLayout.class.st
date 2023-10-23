Class {
	#name : 'RGBitsLayout',
	#superclass : 'RGObjectLayout',
	#category : 'Ring-Core-Kernel',
	#package : 'Ring-Core',
	#tag : 'Kernel'
}

{ #category : 'description' }
RGBitsLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton."

	^ #variableWordSubclass:
]

{ #category : 'testing - types' }
RGBitsLayout >> isBitsLayout [

	^ true
]

{ #category : 'testing - types' }
RGBitsLayout >> isVariableLayout [

	^ true
]
