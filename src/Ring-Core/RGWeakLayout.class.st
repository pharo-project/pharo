Class {
	#name : 'RGWeakLayout',
	#superclass : 'RGPointerLayout',
	#category : 'Ring-Core-Kernel',
	#package : 'Ring-Core',
	#tag : 'Kernel'
}

{ #category : 'description' }
RGWeakLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton."

	^ #weakSubclass:
]

{ #category : 'testing - types' }
RGWeakLayout >> isVariableLayout [

	^ true
]

{ #category : 'testing - types' }
RGWeakLayout >> isWeakLayout [

	^ true
]
