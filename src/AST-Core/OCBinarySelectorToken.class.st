"
ASTBinarySelectorToken is the first-class representation of a binary selector (e.g. +)

"
Class {
	#name : 'OCBinarySelectorToken',
	#superclass : 'OCValueToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
OCBinarySelectorToken >> isBinary [
	^true
]

{ #category : 'testing' }
OCBinarySelectorToken >> isBinary: aString [
	^ value = aString
]
