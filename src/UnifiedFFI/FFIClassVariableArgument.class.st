"
I am a loader for class variables, pushing to the stack the class variable literal
"
Class {
	#name : #FFIClassVariableArgument,
	#superclass : #FFIFunctionArgumentLoader,
	#instVars : [
		'argName'
	],
	#category : #'UnifiedFFI-Arguments'
}

{ #category : #accessing }
FFIClassVariableArgument >> argName [
	^ argName
]

{ #category : #accessing }
FFIClassVariableArgument >> argName: anObject [
	argName := anObject
]

{ #category : #'emitting code' }
FFIClassVariableArgument >> emitArgument: aBuilder context: aContext [
	aBuilder pushLiteralVariable: (aContext methodClass bindingOf: self argName).
]

{ #category : #private }
FFIClassVariableArgument >> rolledPointerTempName [
	^ self rolledPointerTempNameFor: self argName
]
