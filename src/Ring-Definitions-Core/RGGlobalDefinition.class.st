"
An RGGlobalDefinition is an abstract superclass for representing classes, global variables and shared pools

"
Class {
	#name : #RGGlobalDefinition,
	#superclass : #RGDefinition,
	#category : #'Ring-Definitions-Core-Base'
}

{ #category : #comparing }
RGGlobalDefinition >> = aRGGlobalDefinition [
	"This method look for equality of the properties of the receiver"
	"Verifies the class and the name of the receiver"

	^self class = aRGGlobalDefinition class
		and:[ self name == aRGGlobalDefinition name ]
]

{ #category : #comparing }
RGGlobalDefinition >> hash [
	"Hash is re-implemented because #= is re-implemented"
	
	^self class hash bitXor: (self name hash)
]

{ #category : #accessing }
RGGlobalDefinition >> parent [
	"Retrieves the namespace in which the receiver is known"
	
	^self environment
]
