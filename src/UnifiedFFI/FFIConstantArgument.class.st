Class {
	#name : #FFIConstantArgument,
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
FFIConstantArgument >> asOldArraySpec [

	^ { nil . nil . value . 0 }
]

{ #category : #'emitting code' }
FFIConstantArgument >> emitArgument: aBuilder context: aContext [
	
	self loader emitArgument: aBuilder context: aContext
]

{ #category : #'emitting code' }
FFIConstantArgument >> emitArgument: aBuilder context: aContext objectClass: objectClass instVarName: aName [
	
	self shouldNotImplement
]

{ #category : #'private emitting code' }
FFIConstantArgument >> emitArgumentVariableAccesingPart: aBuilder context: aContext objectClass: objectClass instVarName: aName [

	self shouldNotImplement
]

{ #category : #'emitting code' }
FFIConstantArgument >> emitReturnArgument: anIRBuilder context: aContext [

	"Nothing to do here"
]

{ #category : #accessing }
FFIConstantArgument >> externalTypeWithArity [
	
	^ self resolvedType externalTypeWithArity
]

{ #category : #resolving }
FFIConstantArgument >> initialize [

	super initialize.
	type := FFIUndefinedTypeDeclaration new.
]

{ #category : #accessing }
FFIConstantArgument >> loader [
	
	^ resolution
]

{ #category : #resolving }
FFIConstantArgument >> resolveUsing: aResolver [

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
FFIConstantArgument >> resolvedType [
	
	^ resolvedType
]

{ #category : #accessing }
FFIConstantArgument >> resolvedValue [
	self shouldBeImplemented.
]

{ #category : #private }
FFIConstantArgument >> rolledPointerTempName [

	self shouldNotImplement
]

{ #category : #accessing }
FFIConstantArgument >> type [
	
	^ type
]

{ #category : #accessing }
FFIConstantArgument >> value [ 

	^ value
]

{ #category : #accessing }
FFIConstantArgument >> value: anObject [
	value := anObject
]
