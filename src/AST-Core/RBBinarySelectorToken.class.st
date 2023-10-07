"
RBBinarySelectorToken is the first-class representation of a binary selector (e.g. +)

"
Class {
	#name : 'RBBinarySelectorToken',
	#superclass : 'RBValueToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
RBBinarySelectorToken >> isBinary [
	^true
]

{ #category : 'testing' }
RBBinarySelectorToken >> isBinary: aString [
	^ value = aString
]
