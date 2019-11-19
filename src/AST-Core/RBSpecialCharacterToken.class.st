"
RBSpecialCharacterToken is the first-class representation of special characters.


"
Class {
	#name : #RBSpecialCharacterToken,
	#superclass : #RBValueToken,
	#category : #'AST-Core-Tokens'
}

{ #category : #testing }
RBSpecialCharacterToken >> isSpecial [
	^true
]

{ #category : #private }
RBSpecialCharacterToken >> length [
	^1
]
