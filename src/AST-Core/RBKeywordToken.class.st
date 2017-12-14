"
RBKeywordToken is the first-class representation of a keyword token (e.g. add:)
"
Class {
	#name : #RBKeywordToken,
	#superclass : #RBValueToken,
	#category : #'AST-Core-Tokens'
}

{ #category : #testing }
RBKeywordToken >> isKeyword [
	^true
]

{ #category : #testing }
RBKeywordToken >> isPatternVariable [
	^value first = RBScanner patternVariableCharacter
]

{ #category : #testing }
RBKeywordToken >> isPrimitiveKeyword [
	^ self value = #primitive:
]
