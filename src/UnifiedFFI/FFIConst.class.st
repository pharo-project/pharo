"
I'm a constant in an ffi call. 
Constants can be used while doing a call, under a very restrictive rule: it has to be possible to parse it to understand which basic type it is.

"
Class {
	#name : #FFIConst,
	#superclass : #FFIExternalType,
	#instVars : [
		'value',
		'type'
	],
	#category : #'UnifiedFFI-Types'
}

{ #category : #'instance creation' }
FFIConst class >> value: aValue [
	^ self new value: aValue
]

{ #category : #'instance creation' }
FFIConst class >> value: anObject type: aType [
	^ self new 
		value: anObject;
		type: aType;
		yourself
]

{ #category : #unpacking }
FFIConst >> emitPointerArityUnpack: anIRBuilder type: aFFIVoid context: aContext [ 
	
	"Nothing to do here"
]

{ #category : #'emitting code' }
FFIConst >> emitReturn: aBuilder resultTempVar: resultVar context: aContext [ 
	self error: 'Literals cannot be used as return'
]

{ #category : #'emitting code' }
FFIConst >> emitReturnArgument: builder context: aContext [
	"Nothing to do here"
]

{ #category : #accessing }
FFIConst >> externalType [ 

	^ self type externalType
]

{ #category : #accessing }
FFIConst >> externalTypeAlignment [

	^ self externalType externalTypeAlignment
]

{ #category : #accessing }
FFIConst >> externalTypeSize [

	^ self externalType byteSize
]

{ #category : #accessing }
FFIConst >> loader [ 
	^ self
]

{ #category : #accessing }
FFIConst >> pointerArity: additionalArity [
	additionalArity > 0 ifTrue: [ self error: 'passing pointer to constant' ]
]

{ #category : #'stack parameter classification' }
FFIConst >> stackValueParameterClass [
	^ value isFloat ifTrue: [#float] ifFalse: [#integer]

]

{ #category : #accessing }
FFIConst >> type [
	^ type
]

{ #category : #accessing }
FFIConst >> type: anObject [
	type := anObject
]

{ #category : #accessing }
FFIConst >> value [
	^ value
]

{ #category : #accessing }
FFIConst >> value: anObject [
	value := anObject
]
