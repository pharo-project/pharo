"
This is an example for a virtual slot that computes its value. 

It stores a block which is evaluated with the object as a parameter to calculate the value
on read. Writing is ignored.

e.g. make a class lile this:

Object subclass: #TT
	slots: { #i => ComputedSlot with: [ :o | o class methods size ] }
	classVariables: {  }
	package: 'TT'
"
Class {
	#name : #ComputedSlot,
	#superclass : #Slot,
	#instVars : [
		'block'
	],
	#category : #'VariablesLibrary-Slots'
}

{ #category : #comparing }
ComputedSlot >> = other [
	^ super = other and: [block = other block]
]

{ #category : #'code generation' }
ComputedSlot >> emitValue: methodBuilder [
	"generate the bytecode for 'block cull: self'"
	methodBuilder
		pushLiteral: block;
		pushReceiver;
		send: #cull:.
]

{ #category : #comparing }
ComputedSlot >> hasSameDefinitionAs: otherSlot [
	"other then #=, we use string comparision for the blocks here"
	^ (super hasSameDefinitionAs: otherSlot) 
		and: [ block printString = otherSlot block printString ]

]

{ #category : #comparing }
ComputedSlot >> hash [
	^super hash bitXor: block hash
]

{ #category : #printing }
ComputedSlot >> printOn: aStream [
	aStream 
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name;
		nextPutAll: ' with: ';
		nextPutAll: block printString
]

{ #category : #'meta-object-protocol' }
ComputedSlot >> read: anObject [
	"we use #cull: to support both 0-arg and 1-arg blocks"
	^block cull: anObject
	
]

{ #category : #'instance creation' }
ComputedSlot >> with: aBlock [
	block := aBlock
]

{ #category : #'meta-object-protocol' }
ComputedSlot >> write: aValue to: anObject [
	"ignored, as the slot returns the computed value on read"
]
