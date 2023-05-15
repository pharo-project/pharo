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

{ #category : #testing }
RBSpecialCharacterToken >> isSpecial: aValue [
	^self value = aValue
]

{ #category : #private }
RBSpecialCharacterToken >> length [
	^1
]
