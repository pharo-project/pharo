"
ASTSpecialCharacterToken is the first-class representation of special characters.


"
Class {
	#name : 'ASTSpecialCharacterToken',
	#superclass : 'ASTValueToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
ASTSpecialCharacterToken >> isSpecial [
	^true
]

{ #category : 'testing' }
ASTSpecialCharacterToken >> isSpecial: aValue [
	^self value = aValue
]

{ #category : 'private' }
ASTSpecialCharacterToken >> length [
	^1
]
