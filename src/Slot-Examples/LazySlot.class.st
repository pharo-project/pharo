"
A slot that is lazy initialized with a default value. 

Lazy initialized instance variables are implemented in Smalltalk by doing a nil check in the read accessor method.

The slot instead does the nil check as part of the slot read operation

"
Class {
	#name : #LazySlot,
	#superclass : #InstanceVariableSlot,
	#instVars : [
		'default'
	],
	#category : #'Slot-Examples-Base'
}

{ #category : #comparing }
LazySlot >> = other [
	^ super = other and: [default = other default]
]

{ #category : #accessing }
LazySlot >> default: anObject [
	default := anObject
]

{ #category : #'code generation' }
LazySlot >> emitValue: aMethodBuilder [
	aMethodBuilder
		pushInstVar: index;
		pushDup;
		pushLiteral: nil;
		send: #==;
		jumpAheadTo: #target if: false;
		popTop;
		pushLiteral: default;
		jumpAheadTarget: #target
]

{ #category : #comparing }
LazySlot >> hasSameDefinitionAs: otherSlot [
	^ (super hasSameDefinitionAs: otherSlot) 
		and: [ default = otherSlot default ]
]

{ #category : #comparing }
LazySlot >> hash [
	^super hash bitXor: default hash
]

{ #category : #printing }
LazySlot >> printOn: aStream [
	aStream 
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name;
		nextPutAll: ' default: ';
		store: default
]

{ #category : #'meta-object-protocol' }
LazySlot >> read: anObject [
	^ (super read: anObject) ifNil: [ default ]
]
