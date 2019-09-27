Class {
	#name : #FFIVariableArgument,
	#superclass : #FFIFunctionArgument,
	#instVars : [
		'name',
		'type',
		'loader',
		'resolvedType'
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

{ #category : #'emitting code' }
FFIVariableArgument >> emitReturnArgument: anIRBuilder context: aContext [ 
	
	loader
		emitPointerArityUnpack: anIRBuilder 
		type: self resolvedType
		context: aContext
]

{ #category : #accessing }
FFIVariableArgument >> externalTypeWithArity [
	
	^ resolvedType externalTypeWithArity
]

{ #category : #accessing }
FFIVariableArgument >> loader [
	^ loader
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

{ #category : #accessing }
FFIVariableArgument >> resolvedType [
	
	^ resolvedType
]

{ #category : #accessing }
FFIVariableArgument >> type [
	^ type
]

{ #category : #accessing }
FFIVariableArgument >> type: anObject [
	type := anObject
]
