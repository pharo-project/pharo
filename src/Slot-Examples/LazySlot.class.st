"
A slot that is lazy initialized with a default value. 

Lazy initialized instance variables are implemented in Smalltalk by doing a nil check in the read accessor method.

The slot instead does the nil check as part of the slot read operation

"
Class {
	#name : #LazySlot,
	#superclass : #AbstractInitializedSlot,
	#category : #'Slot-Examples-Base'
}

{ #category : #'code generation' }
LazySlot >> emitValue: aMethodBuilder [
	"generate bytecode for '<varname> ifNil: [default]'"
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

{ #category : #'meta-object-protocol' }
LazySlot >> read: anObject [
	^ (super read: anObject) ifNil: [ default ]
]
