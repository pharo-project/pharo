"
By default each Slot corresponds to an instance variable and vice versa. Hence there is a Slot for each instance variable.


"
Class {
	#name : #IndexedSlot,
	#superclass : #Slot,
	#instVars : [
		'index'
	],
	#category : #'Slot-Core-Variables'
}

{ #category : #comparing }
IndexedSlot >> = other [
	self == other
		ifTrue: [ ^ true ].
	^ ((self species == other species) 
			and: [ index = other index ])
			and: [ name = other name ]
]

{ #category : #comparing }
IndexedSlot >> hasSameDefinitionAs: other [
	"Definiton does not contain index"
	^super = other
]

{ #category : #comparing }
IndexedSlot >> hash [
	^ (self species hash bitXor: self name hash) bitXor: (self index ifNil: [ 0 ])
]

{ #category : #accessing }
IndexedSlot >> index [
	^ index
]

{ #category : #accessing }
IndexedSlot >> index: anIndex [
	index := anIndex
]

{ #category : #testing }
IndexedSlot >> isVirtual [
	"indexed slots are the only not virtual slots for now"
	^false
]

{ #category : #'meta-object-protocol' }
IndexedSlot >> read: anObject [
	^ thisContext object: anObject instVarAt: index
]

{ #category : #'meta-object-protocol' }
IndexedSlot >> write: aValue to: anObject [
	^ thisContext object: anObject instVarAt: index put: aValue
]
