Class {
	#name : #FFIValueArgument,
	#superclass : #FFIFunctionArgument,
	#instVars : [
		'value',
		'resolution'
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

{ #category : #accessing }
FFIValueArgument >> loader [
	
	^ resolution
]

{ #category : #resolving }
FFIValueArgument >> resolveUsing: aResolver [
	
	resolution := FFIConst value: value
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
