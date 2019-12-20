"
A slot that is lazy initialized with a default value. 

Lazy initialized instance variables are implemented in Smalltalk by doing a nil check in the read accessor method.

The slot instead does the nil check as part of the slot read operation.

#name => LazySlot default: 5.

#name => LazySlot defaultBlock: [ 'a bloc' ].

#name => LazySlot defaultBlock: [ :object | 'a bloc, parameter is the instance' ].
"
Class {
	#name : #LazySlot,
	#superclass : #AbstractInitializedSlot,
	#category : #'VariablesLibrary-Slots'
}

{ #category : #'code generation' }
LazySlot >> emitValue: aMethodBuilder [
	"generate bytecode for '<varname> ifNil: [<varname> := default cull: self]'"
	aMethodBuilder
		pushInstVar: index;
		pushDup;
		pushLiteral: nil;
		send: #==;
		jumpAheadTo: #target if: false;
		popTop;
		pushLiteral: default.
	default isBlock ifTrue: [ 
		aMethodBuilder	
			pushReceiver;
			send: #cull:].
	aMethodBuilder
		storeInstVar: index;
		jumpAheadTarget: #target
]

{ #category : #'meta-object-protocol' }
LazySlot >> read: anObject [
	"if the value is nil, we evaluate the default block with the object as a potential parameter"
	^ (super read: anObject) ifNil: [ 
		default isBlock 
			ifTrue: [ self write: (default cull: anObject) to: anObject] 
			ifFalse: [ self write: default to: anObject]]
]
