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

{ #category : 'class initialization' }
ASTIdentifierToken class >> initialize [

	self deprecatedAliases: { #ASTIdentifierToken }
]

{ #category : 'testing' }
ASTIdentifierToken >> isIdentifier [
	^true
]

{ #category : 'testing' }
ASTIdentifierToken >> isKeywordPattern [
	^ self isPatternVariable	and: [ value second = ASTScanner keywordPatternCharacter
				and: [ value third ~= ASTScanner cascadePatternCharacter ] ]
]

{ #category : 'testing' }
ASTIdentifierToken >> isPatternVariable [
	^value first = ASTScanner patternVariableCharacter
]
