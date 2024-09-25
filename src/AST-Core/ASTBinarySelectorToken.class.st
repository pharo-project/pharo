"
ASTBinarySelectorToken is the first-class representation of a binary selector (e.g. +)

"
Class {
	#name : 'ASTBinarySelectorToken',
	#superclass : 'ASTValueToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
ASTBinarySelectorToken >> isBinary [
	^true
]

{ #category : 'testing' }
ASTBinarySelectorToken >> isBinary: aString [
	^ value = aString
]
