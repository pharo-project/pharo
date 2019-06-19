Class {
	#name : #FFIValueArgument,
	#superclass : #FFIFunctionArgument,
	#instVars : [
		'value'
	],
	#category : #'UnifiedFFI-Arguments'
}

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

{ #category : #private }
FFIValueArgument >> rolledPointerTempName [

	self shouldNotImplement
]

{ #category : #accessing }
FFIValueArgument >> value [ 

	^ value
]

{ #category : #accessing }
FFIValueArgument >> value: anObject [
	value := anObject
]
