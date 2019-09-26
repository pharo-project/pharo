Class {
	#name : #FFITypeDeclaration,
	#superclass : #Object,
	#instVars : [
		'name',
		'arity'
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

{ #category : #accessing }
FFITypeDeclaration >> name [
	^ name
]

{ #category : #accessing }
FFITypeDeclaration >> name: anObject [
	name := anObject
]
