parsing-public
nextIntegerBase: aRadix
	"Form an integer with following digits.
	Fail if no digit found"
	
	| isNeg value |
	isNeg := self peekSignIsMinus.
	value := self nextUnsignedIntegerBase: aRadix.
	^isNeg
		ifTrue: [value negated]
		ifFalse: [value]