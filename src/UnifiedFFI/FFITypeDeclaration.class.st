"
I represent a type declaration in a FFI function signature. I have a type name and an arity (the number of indirections indicated by pointer marker stars *).
I am resolved as an external FFI type with my arity.
"
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

{ #category : #accessing }
FFITypeDeclaration >> defaultReturnOnError [

	^ self resolvedType defaultReturnOnError
]

{ #category : #'emitting code' }
FFITypeDeclaration >> emitReturn: anIRBuilder resultTempVar: aString context: aContext inCallout: aCallout [
	
	^ resolvedType emitReturn: anIRBuilder resultTempVar: aString context: aContext inCallout: aCallout
]

{ #category : #accessing }
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

	resolvedType := (aResolver resolveType: name) pointerArity: arity.
	resolvedType isExternalType
		ifTrue: [ resolvedType validateAfterParse: self asOldArraySpec ].
	^ resolvedType
]

{ #category : #resolution }
FFITypeDeclaration >> resolveUsing: aResolver forArgument: aFFIVariableArgument [ 
	
	^ self resolveUsing: aResolver
]

{ #category : #accessing }
FFITypeDeclaration >> resolvedType [
	^ resolvedType
]
