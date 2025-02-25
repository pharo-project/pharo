"
ASTSpecialCharacterToken is the first-class representation of special characters.


"
Class {
	#name : 'OCSpecialCharacterToken',
	#superclass : 'OCValueToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
OCSpecialCharacterToken >> isSpecial [
	^true
]

{ #category : 'testing' }
OCSpecialCharacterToken >> isSpecial: aValue [
	^self value = aValue
]

{ #category : 'private' }
OCSpecialCharacterToken >> length [
	^1
]
