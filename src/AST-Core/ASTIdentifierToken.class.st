"
RBIdentifierToken is the first-class representation of an identifier token (e.g. Class)

"
Class {
	#name : 'ASTIdentifierToken',
	#superclass : 'ASTValueToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
ASTIdentifierToken >> isIdentifier [
	^true
]

{ #category : 'testing' }
ASTIdentifierToken >> isKeywordPattern [
	^ self isPatternVariable	and: [ value second = RBScanner keywordPatternCharacter
				and: [ value third ~= RBScanner cascadePatternCharacter ] ]
]

{ #category : 'testing' }
ASTIdentifierToken >> isPatternVariable [
	^value first = RBScanner patternVariableCharacter
]
