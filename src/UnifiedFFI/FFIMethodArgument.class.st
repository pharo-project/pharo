"
I implement the push strategy for method arguments who are simple variables.
"
Class {
	#name : #FFIMethodArgument,
	#superclass : #FFIFunctionArgumentLoader,
	#instVars : [
		'index',
		'argName'
	],
	#category : #'UnifiedFFI-Arguments'
}

{ #category : #accessing }
FFIMethodArgument >> argName [
	^ argName
]

{ #category : #accessing }
FFIMethodArgument >> argName: anObject [
	argName := anObject
]

{ #category : #'emitting code' }
FFIMethodArgument >> emitArgument: aBuilder context: aContext [
	aBuilder pushTemp: self argName asSymbol
]

{ #category : #'emitting code' }
FFIMethodArgument >> emitArgument: aBuilder context: aContext objectClass: objectClass instVarName: aName [
	"If argument is an object, I need to send value of instVar aName instead it self"
	aBuilder 
		pushTemp: self argName asSymbol;
		pushLiteral: (objectClass 
			instVarIndexFor: aName 
			ifAbsent: [ self error: 'No ', aName, ' instVar' ]);
		send: #instVarAt:
]

{ #category : #'private emitting code' }
FFIMethodArgument >> emitArgumentVariableAccesingPart: aBuilder context: aContext objectClass: objectClass instVarName: aName [
	aBuilder 
		pushTemp: self argName asSymbol;
		pushLiteral: (objectClass 
			instVarIndexFor: aName 
			ifAbsent: [ self error: 'No ', aName, ' instVar' ]).
]

{ #category : #accessing }
FFIMethodArgument >> index [
	"I am the index in the method. 
	 For example, method: arg1 withArg: arg2 will have arguments at index 1 and at index 2"
	^ index
]

{ #category : #accessing }
FFIMethodArgument >> index: anObject [
	index := anObject
]

{ #category : #private }
FFIMethodArgument >> rolledPointerTempName [
	^ self rolledPointerTempNameFor: self argName
]
