"
I'm an abstract class to implement the pushing strategy for method arguments. 
My children define different pushing strategies for the valid types or argument (usually an atomic type object, and external object or a ""self"" instruction)
"
Class {
	#name : #FFIFunctionArgumentLoader,
	#superclass : #Object,
	#category : #'UnifiedFFI-Arguments'
}

{ #category : #'private emitting code' }
FFIFunctionArgumentLoader >> addUnpack: aBuilder fromArity: arity [
	aBuilder
		pushTemp: self rolledPointerTempName;
		pushLiteral: arity;
		send: #unpackFromArity:;
		send: #adoptAddress:;
		popTop
]

{ #category : #'emitting code' }
FFIFunctionArgumentLoader >> emitArgument: aBuilder context: aContext [
	self subclassResponsibility
]

{ #category : #'emitting code' }
FFIFunctionArgumentLoader >> emitArgument: aBuilder context: aContext inCallout: aCallout [
	
	self emitArgument: aBuilder context: aContext
]

{ #category : #'emitting code' }
FFIFunctionArgumentLoader >> emitArgument: aBuilder context: aContext objectClass: objectClass instVarName: aName [
	self subclassResponsibility
]

{ #category : #'private emitting code' }
FFIFunctionArgumentLoader >> emitArgumentVariableAccesingPart: aBuilder context: aContext objectClass: objectClass instVarName: aName [
	"Emit access to calling variable (changes depending on the type)."
	self subclassResponsibility
]

{ #category : #'emitting code' }
FFIFunctionArgumentLoader >> emitHandleArityUnpack: aBuilder type: aType context: aContext objectClass: objectClass instVarName: aName [
	"This a special kind of arity-unpacking, that works with FFIConstantHandleType"

	aType needsArityUnpacking ifFalse: [ ^ self ].
	
	"Push accessing variable (this change depending on the argument type)"
	self 
		emitArgumentVariableAccesingPart: aBuilder 
		context: aContext 
		objectClass: objectClass 
		instVarName: aName.
	"Unpack argument"
	aBuilder
		pushTemp: self rolledPointerTempName;
		pushLiteral: aType pointerArity;
		send: #unpackHandleFromArity:.
	"Send assignment"
	aBuilder send: #instVarAt:put:.
	"... and return top"
	aBuilder popTop.
]

{ #category : #'emitting code' }
FFIFunctionArgumentLoader >> emitPointerArityPack: aBuilder context: aContext arity: arity [
	"I take into account complex arities (like **, ***). 
	 I'm called right after argument marshaling which means it is already in top of the stack"
	^ self 
		emitPointerArityPack: aBuilder 
		context: aContext 
		arity: arity
		selector: #packToArity:
]

{ #category : #'emitting code' }
FFIFunctionArgumentLoader >> emitPointerArityPack: aBuilder context: aContext arity: arity selector: aSelector [
	"I take into account complex arities (like **, ***). 
	 I'm called right after argument marshaling which means it is already in top of the stack"
	| ptrName |
	ptrName := self rolledPointerTempName.
	aBuilder 
		addTemp: ptrName;
		pushLiteral: arity;
		send: aSelector;
		storeTemp: ptrName
]

{ #category : #'private emitting code' }
FFIFunctionArgumentLoader >> emitPointerArityUnpack: aBuilder context: aContext arity: arity [
	self emitArgument: aBuilder context: aContext.
	self addUnpack: aBuilder fromArity: arity
]

{ #category : #'private emitting code' }
FFIFunctionArgumentLoader >> emitPointerArityUnpack: aBuilder context: aContext objectClass: objectClass instVarName: aName arity: arity [
	self emitArgument: aBuilder context: aContext objectClass: objectClass instVarName: aName.
	self addUnpack: aBuilder fromArity: arity
]

{ #category : #'emitting code' }
FFIFunctionArgumentLoader >> emitPointerArityUnpack: aBuilder type: aType context: aContext [
	aType needsArityUnpacking ifFalse: [ ^ self ].
	self 
		emitPointerArityUnpack: aBuilder 
		context: aContext 
		arity: aType pointerArity
]

{ #category : #'emitting code' }
FFIFunctionArgumentLoader >> emitPointerArityUnpack: aBuilder type: aType context: aContext objectClass: objectClass instVarName: aName [
	aType needsArityUnpacking ifFalse: [ ^ self ].
	self 
		emitPointerArityUnpack: aBuilder 
		context: aContext 
		objectClass: objectClass 
		instVarName: aName
		arity: aType pointerArity
]

{ #category : #private }
FFIFunctionArgumentLoader >> rolledPointerTempName [
	self subclassResponsibility
]

{ #category : #private }
FFIFunctionArgumentLoader >> rolledPointerTempNameFor: aString [
	^ ('_ptr_', aString) asSymbol
]
