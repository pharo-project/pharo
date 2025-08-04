"
RBPatternScanner is a subclass of RBScanner that allows the extended syntax of pattern matching trees.

"
Class {
	#name : 'OCPatternScanner',
	#superclass : 'OCScanner',
	#category : 'AST-Core-Parser',
	#package : 'AST-Core',
	#tag : 'Parser'
}

{ #category : 'private - scanning' }
OCPatternScanner >> scanPatternVariable [
	buffer nextPut: currentCharacter.
	self step.
	currentCharacter = ${
		ifTrue:
			[self step.
			^OCPatternBlockToken value: '`{' start: tokenStart].
	[characterType = #alphabetic] whileFalse:
			[characterType = #eof
				ifTrue: [ ^ self scanError: 'Meta variable expected'].
			buffer nextPut: currentCharacter.
			self step].
	^self scanIdentifierOrKeyword
]

{ #category : 'accessing' }
OCPatternScanner >> scanToken [
	currentCharacter = PatternVariableCharacter
		ifTrue: [^self scanPatternVariable].
	currentCharacter = $} ifTrue: [^self scanSpecialCharacter].
	^super scanToken
]
