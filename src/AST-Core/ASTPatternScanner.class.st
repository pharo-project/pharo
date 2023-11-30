"
RBPatternScanner is a subclass of RBScanner that allows the extended syntax of pattern matching trees.

"
Class {
	#name : 'ASTPatternScanner',
	#superclass : 'ASTScanner',
	#category : 'AST-Core-Parser',
	#package : 'AST-Core',
	#tag : 'Parser'
}

{ #category : 'class initialization' }
ASTPatternScanner class >> initialize [

	self deprecatedAliases: { #ASTPatternScanner }
]

{ #category : 'private - scanning' }
ASTPatternScanner >> scanPatternVariable [
	buffer nextPut: currentCharacter.
	self step.
	currentCharacter = ${
		ifTrue:
			[self step.
			^ASTPatternBlockToken value: '`{' start: tokenStart].
	[characterType = #alphabetic] whileFalse:
			[characterType = #eof
				ifTrue: [ ^ self scanError: 'Meta variable expected'].
			buffer nextPut: currentCharacter.
			self step].
	^self scanIdentifierOrKeyword
]

{ #category : 'accessing' }
ASTPatternScanner >> scanToken [
	currentCharacter = PatternVariableCharacter
		ifTrue: [^self scanPatternVariable].
	currentCharacter = $} ifTrue: [^self scanSpecialCharacter].
	^super scanToken
]
