"
ASTBinarySelectorToken is the first-class representation of a binary selector (e.g. +)

"
Class {
	#name : #ASTBinarySelectorToken,
	#superclass : #ASTValueToken,
	#category : #'AST-Core-Tokens'
}

{ #category : #testing }
ASTBinarySelectorToken >> isBinary [
	^true
]
