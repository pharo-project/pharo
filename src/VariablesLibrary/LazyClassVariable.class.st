"
A class variable that is lazy initialized with a default value. 

Lazy initialized variables are implemented in Smalltalk by doing a nil check in the read accessor method.

This class variable instead does the nil check as part of the variable read operation.

#name => LazyClassVariable default: 5.

#name => LazyClassVariable defaultBlock: [ 'a bloc' ].

#name => LazyClassVariable defaultBlock: [ :object | 'a bloc, parameter is the class' ].
"
Class {
	#name : #LazyClassVariable,
	#superclass : #AbstractInitializedClassVariable,
	#category : #'VariablesLibrary-ClassVariables'
}

{ #category : #'code generation' }
LazyClassVariable >> emitValue: aMethodBuilder [
	"generate bytecode for '<varname> ifNil: [<varname> := default cull: self]'"
	aMethodBuilder
		pushLiteralVariable: self;
		pushDup;
		pushLiteral: nil;
		send: #==;
		jumpAheadTo: #target if: false;
		popTop;
		pushLiteral: default.
	default isBlock ifTrue: [ 
		aMethodBuilder	
			pushReceiver;
			send: #definingClass;
			send: #cull:].
	aMethodBuilder
		storeIntoLiteralVariable: self;
		jumpAheadTarget: #target
]

{ #category : #'meta-object-protocol' }
LazyClassVariable >> read [
	"if the value is nil, we evaluate the default block with the object as a potential parameter"
	^ super read ifNil: [ 
		default isBlock 
			ifTrue: [ self write: (default cull: self definingClass) ] 
			ifFalse: [ self write: default ] ]
]
