Class {
	#name : #FFIValueArgument,
	#superclass : #FFIFunctionArgument,
	#instVars : [
		'value',
		'resolution',
		'type',
		'resolvedType'
	],
	#category : #'UnifiedFFI-Arguments'
}

{ #category : #converting }
FFIValueArgument >> asOldArraySpec [

	^ { nil . nil . value . 0 }
]

{ #category : #'emitting code' }
FFIValueArgument >> emitArgument: aBuilder context: aContext [
	
	aBuilder pushLiteral: self value
]

{ #category : #'emitting code' }
FFIValueArgument >> emitArgument: aBuilder context: aContext objectClass: objectClass instVarName: aName [
	
	self shouldNotImplement
]

{ #category : #'private emitting code' }
FFIValueArgument >> emitArgumentVariableAccesingPart: aBuilder context: aContext objectClass: objectClass instVarName: aName [

	self shouldNotImplement
]

{ #category : #'emitting code' }
FFIValueArgument >> emitReturnArgument: anIRBuilder context: aContext [

	"Nothing to do here"
]

{ #category : #accessing }
FFIValueArgument >> externalTypeWithArity [
	
	^ self resolvedType externalTypeWithArity
]

{ #category : #resolving }
FFIValueArgument >> initialize [

	super initialize.
	type := FFIUndefinedTypeDeclaration new.
]

{ #category : #accessing }
FFIValueArgument >> loader [
	
	^ resolution
]

{ #category : #resolving }
FFIValueArgument >> resolveUsing: aResolver [

	value = 'false' ifTrue: [ 
		resolution := FFIConst value: 0 type: (aResolver resolveType: #bool).
		resolvedType := type resolveUsing: aResolver forArgument: self.
		^ self ].
	value = 'true' ifTrue: [ 
		resolution := FFIConst value: 1 type: (aResolver resolveType: #bool).
		resolvedType := type resolveUsing: aResolver forArgument: self.
		^ self ].
	(value = 'nil' or: [ value = 'NULL' ]) ifTrue: [ 
		resolution := FFIConst value: ExternalAddress null type: (aResolver resolveType: #'void *').
		resolvedType := type resolveUsing: aResolver forArgument: self.
		^ self ].
	value = 'self' ifTrue: [
		resolution := resolvedType := type resolveUsing: aResolver forArgument: self.
		resolution loader: FFISelfArgument new.
		^ self ].
	resolution := FFIConst value: value.
	resolvedType := type resolveUsing: aResolver forArgument: self.
]

{ #category : #accessing }
FFIValueArgument >> resolvedType [
	
	^ resolvedType
]

{ #category : #accessing }
FFIValueArgument >> resolvedValue [
	self shouldBeImplemented.
]

{ #category : #private }
FFIValueArgument >> rolledPointerTempName [

	self shouldNotImplement
]

{ #category : #accessing }
FFIValueArgument >> type [
	
	^ type
]

{ #category : #accessing }
FFIValueArgument >> value [ 

	^ value
]

{ #category : #accessing }
FFIValueArgument >> value: anObject [
	value := anObject
]
