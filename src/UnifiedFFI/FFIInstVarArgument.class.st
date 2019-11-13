"
I implement the push strategy for method arguments who are  instance variables.
"
Class {
	#name : #FFIInstVarArgument,
	#superclass : #FFIFunctionArgumentLoader,
	#instVars : [
		'argName'
	],
	#category : #'UnifiedFFI-Arguments'
}

{ #category : #accessing }
FFIInstVarArgument >> argName [
	^ argName
]

{ #category : #accessing }
FFIInstVarArgument >> argName: anObject [
	argName := anObject
]

{ #category : #'emitting code' }
FFIInstVarArgument >> emitArgument: aBuilder context: aContext [
	aBuilder pushInstVar: (aContext methodClass instVarIndexFor: self argName).
]

{ #category : #'emitting code' }
FFIInstVarArgument >> emitArgument: aBuilder context: aContext objectClass: objectClass instVarName: aName [
	"If argument is an object, I need to send value of instVar aName instead it self"
	aBuilder 
		pushInstVar: (aContext methodClass instVarIndexFor: self argName);
		pushLiteral: (objectClass 
			instVarIndexFor: aName 
			ifAbsent: [ self error: 'No ', aName, ' instVar' ]);
		send: #instVarAt:
]

{ #category : #'private emitting code' }
FFIInstVarArgument >> emitArgumentVariableAccesingPart: aBuilder context: aContext objectClass: objectClass instVarName: aName [
	aBuilder 
		pushInstVar: (aContext methodClass instVarIndexFor: self argName);
		pushLiteral: (objectClass 
			instVarIndexFor: aName 
			ifAbsent: [ self error: 'No ', aName, ' instVar' ]).

]

{ #category : #private }
FFIInstVarArgument >> rolledPointerTempName [
	^ self rolledPointerTempNameFor: self argName
]
