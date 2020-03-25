"
I am a function argument representing a variable (in contrast to a literal value).
The parser creates me when it finds symbols as arguments.

I am resolved by resolving my type and my name.
The type knows how to resolve itself in a context.
To resolve my name, the resolver performs a semantic analysis in the calling context to define if the variable is an instance variable, a method argument or a class variable. The name resolution is represented by my loader.
"
Class {
	#name : #FFIVariableArgument,
	#superclass : #FFIFunctionArgument,
	#instVars : [
		'name'
	],
	#category : #'UnifiedFFI-Arguments'
}

{ #category : #'instance creation' }
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
