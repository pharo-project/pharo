bytecode generation
genReturnSpecialLiteral: aLiteral
	"88-91		010110 ii			Return Receiver/true/false/nil"
	| index |
	index := #(true false nil) indexOf: aLiteral ifAbsent: 0.
	index = 0 ifTrue:
		[^self error: 'return special literal: ', aLiteral printString,  ' is not one of true false nil'].
	stream nextPut: 88 + index