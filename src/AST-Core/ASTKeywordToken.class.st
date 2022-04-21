"
ASTKeywordToken is the first-class representation of a keyword token (e.g. add:)
"
Class {
	#name : #ASTKeywordToken,
	#superclass : #ASTValueToken,
	#category : #'AST-Core-Tokens'
}

{ #category : #testing }
ASTKeywordToken >> isKeyword [
	^true
]

{ #category : #testing }
ASTKeywordToken >> isPatternVariable [
	^value first = ASTScanner patternVariableCharacter
]

{ #category : #testing }
ASTKeywordToken >> isPrimitiveKeyword [
	^ self value = #primitive:
]
