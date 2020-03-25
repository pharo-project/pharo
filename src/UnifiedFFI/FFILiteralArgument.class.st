"
I implement the push strategy for literal objects specified in a callout.
"
Class {
	#name : #FFILiteralArgument,
	#superclass : #FFIFunctionArgumentLoader,
	#instVars : [
		'value',
		'type'
	],
	#category : #'UnifiedFFI-Arguments'
}

{ #category : #'instance creation' }
FFILiteralArgument class >> value: aValue [
	^ self new value: aValue
]

{ #category : #'instance creation' }
FFILiteralArgument class >> value: anObject type: aType [
	^ self new 
		value: anObject;
		type: aType;
		yourself
]

{ #category : #'emitting code' }
FFILiteralArgument >> emitArgument: aBuilder context: aContext [

	aBuilder pushLiteral: self value
]

{ #category : #unpacking }
FFILiteralArgument >> emitPointerArityUnpack: anIRBuilder type: aFFIVoid context: aContext [ 
	
	"Nothing to do here"
]

{ #category : #'emitting code' }
FFILiteralArgument >> emitReturn: aBuilder resultTempVar: resultVar context: aContext [ 
	self error: 'Literals cannot be used as return'
]

{ #category : #'emitting code' }
FFILiteralArgument >> emitReturnArgument: builder context: aContext [
	"Nothing to do here"
]

{ #category : #accessing }
FFILiteralArgument >> externalType [ 

	^ self type externalType
]

{ #category : #accessing }
FFILiteralArgument >> externalTypeAlignment [

	^ self externalType externalTypeAlignment
]

{ #category : #accessing }
FFILiteralArgument >> externalTypeSize [

	^ self externalType byteSize
]

{ #category : #accessing }
FFILiteralArgument >> loader [ 
	^ self
]

{ #category : #accessing }
FFILiteralArgument >> pointerArity: additionalArity [
	additionalArity > 0 ifTrue: [ self error: 'passing pointer to constant' ]
]

{ #category : #'stack parameter classification' }
FFILiteralArgument >> stackValueParameterClass [
	^ value isFloat ifTrue: [#float] ifFalse: [#integer]

]

{ #category : #accessing }
FFILiteralArgument >> type [
	^ type
]

{ #category : #accessing }
FFILiteralArgument >> type: anObject [
	type := anObject
]

{ #category : #accessing }
FFILiteralArgument >> value [
	^ value
]

{ #category : #accessing }
FFILiteralArgument >> value: anObject [
	value := anObject
]
