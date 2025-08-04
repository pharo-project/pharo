"
RBKeywordToken is the first-class representation of a keyword token (e.g. add:)
"
Class {
	#name : 'OCKeywordToken',
	#superclass : 'OCValueToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
OCKeywordToken >> isKeyword [
	^true
]

{ #category : 'testing' }
OCKeywordToken >> isPatternVariable [
	^value first = OCScanner patternVariableCharacter
]

{ #category : 'testing' }
OCKeywordToken >> isPrimitiveKeyword [
	^ self value = #primitive:
]
