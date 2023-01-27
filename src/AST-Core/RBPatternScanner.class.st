"
RBPatternScanner is a subclass of RBScanner that allows the extended syntax of pattern matching trees.

"
Class {
	#name : #RBPatternScanner,
	#superclass : #RBScanner,
	#category : #'AST-Core-Parser'
}

{ #category : #'private - scanning' }
RBPatternScanner >> scanPatternVariable [
	buffer nextPut: currentCharacter.
	self step.
	currentCharacter = ${
		ifTrue:
			[self step.
			^RBPatternBlockToken value: '`{' start: tokenStart].
	[characterType = #alphabetic] whileFalse:
			[characterType = #eof
				ifTrue: [ ^ self scanError: 'Meta variable expected'].
			buffer nextPut: currentCharacter.
			self step].
	^self scanIdentifierOrKeyword
]

{ #category : #accessing }
RBPatternScanner >> scanToken [
	currentCharacter = PatternVariableCharacter
		ifTrue: [^self scanPatternVariable].
	currentCharacter = $} ifTrue: [^self scanSpecialCharacter].
	^super scanToken
]
