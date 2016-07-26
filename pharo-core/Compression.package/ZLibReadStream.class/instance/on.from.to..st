initialize
on: aCollection from: firstIndex to: lastIndex
	"Check the header of the ZLib stream."
	| method byte |
	super on: aCollection from: firstIndex to: lastIndex.
	crc := 1.
	method := self nextBits: 8.
	(method bitAnd: 15) = 8 ifFalse:[^self error:'Unknown compression method'].
	(method bitShift: -4) + 8 > 15 ifTrue:[^self error:'Invalid window size'].
	byte := self nextBits: 8.
	(method bitShift: 8) + byte \\ 31 = 0 ifFalse:[^self error:'Incorrect header'].
	(byte anyMask: 32) ifTrue:[^self error:'Need preset dictionary'].
