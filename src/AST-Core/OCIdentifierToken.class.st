"
ASTIdentifierToken is the first-class representation of an identifier token (e.g. Class)

"
Class {
	#name : 'OCIdentifierToken',
	#superclass : 'OCValueToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
OCIdentifierToken >> isIdentifier [
	^true
]

{ #category : 'testing' }
OCIdentifierToken >> isKeywordPattern [
	^ self isPatternVariable	and: [ value second = OCScanner keywordPatternCharacter
				and: [ value third ~= OCScanner cascadePatternCharacter ] ]
]

{ #category : 'testing' }
OCIdentifierToken >> isPatternVariable [
	^value first = OCScanner patternVariableCharacter
]
