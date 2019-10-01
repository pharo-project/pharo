Class {
	#name : #FFIVariableArgument,
	#superclass : #FFIFunctionArgument,
	#instVars : [
		'name'
	],
	#category : #'UnifiedFFI-Arguments'
}

{ #category : #'instance-creation' }
FFIVariableArgument class >> name: aString typeName: aTypeName arity: anInteger [
	
	^ self new
		name: aString;
		type: (FFITypeDeclaration typeName: aTypeName arity: anInteger);
		yourself
]

{ #category : #converting }
FFIVariableArgument >> asOldArraySpec [

	^ { name . nil . type name . type arity }
]

{ #category : #'emitting code' }
FFIVariableArgument >> emitArgument: anIRBuilder context: aContext [

	self resolvedType emitArgument: anIRBuilder context: aContext.
]

{ #category : #accessing }
FFIVariableArgument >> name [
	^ name
]

{ #category : #accessing }
FFIVariableArgument >> name: anObject [
	name := anObject
]

{ #category : #resolution }
FFIVariableArgument >> resolveUsing: aResolver [

	loader := aResolver loaderForArgNamed: name indirectIndex: nil.
	resolvedType := type resolveUsing: aResolver forArgument: self.	
	resolvedType loader: loader.
]
