Class {
	#name : #FFIVariableArgument,
	#superclass : #FFIFunctionArgument,
	#instVars : [
		'name',
		'type',
		'loader'
	],
	#category : #'UnifiedFFI-Base'
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
	
	loader := aResolver loaderForArgNamed: name indirectIndex: nil
]

{ #category : #accessing }
FFIVariableArgument >> type [
	^ type
]

{ #category : #accessing }
FFIVariableArgument >> type: anObject [
	type := anObject
]
