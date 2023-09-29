Class {
	#name : 'RGVariableLayout',
	#superclass : 'RGPointerLayout',
	#category : 'Ring-Core-Kernel',
	#package : 'Ring-Core',
	#tag : 'Kernel'
}

{ #category : 'description' }
RGVariableLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton."

	^ #variableSubclass:
]

{ #category : 'testing - types' }
RGVariableLayout >> isVariableLayout [

	^ true
]
