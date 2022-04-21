"
ASTSpecialCharacterToken is the first-class representation of special characters.


"
Class {
	#name : #ASTSpecialCharacterToken,
	#superclass : #ASTValueToken,
	#category : #'AST-Core-Tokens'
}

{ #category : #testing }
ASTSpecialCharacterToken >> isSpecial [
	^true
]

{ #category : #private }
ASTSpecialCharacterToken >> length [
	^1
]
