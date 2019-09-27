Class {
	#name : #FFITypeDeclaration,
	#superclass : #Object,
	#instVars : [
		'name',
		'arity',
		'resolvedType'
	],
	#category : #'UnifiedFFI-Arguments'
}

{ #category : #'instance creation' }
FFITypeDeclaration class >> typeName: aString arity: anInteger [ 
	
	^ self new
		name: aString;
		arity: anInteger;
		yourself
]

{ #category : #accessing }
FFITypeDeclaration >> arity [
	^ arity
]

{ #category : #accessing }
FFITypeDeclaration >> arity: anObject [
	arity := anObject
]

{ #category : #converting }
FFITypeDeclaration >> asOldArraySpec [
	
	^ { name . arity }
]

{ #category : #'emitting code' }
FFITypeDeclaration >> emitReturn: anIRBuilder resultTempVar: aString context: aContext [
	
	^ resolvedType emitReturn: anIRBuilder resultTempVar: aString context: aContext
]

{ #category : #'as yet unclassified' }
FFITypeDeclaration >> externalTypeWithArity [
	
	^ resolvedType externalTypeWithArity
]

{ #category : #accessing }
FFITypeDeclaration >> name [
	^ name
]

{ #category : #accessing }
FFITypeDeclaration >> name: anObject [
	name := anObject
]

{ #category : #resolution }
FFITypeDeclaration >> resolveUsing: aResolver [

	resolvedType := (aResolver resolveType: name) pointerArity: arity
]

{ #category : #resolution }
FFITypeDeclaration >> resolveUsing: aResolver forArgument: aFFIVariableArgument [ 
	
	^ (aResolver resolveType: name)
		pointerArity: arity;
		yourself
]
