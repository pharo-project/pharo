parsing-public
nextIntegerBase: aRadix ifFail: aBlock
	"Form an integer with optional sign and following digits from sourceStream."
	
	| isNeg value |
	isNeg := self peekSignIsMinus.
	value := self nextUnsignedIntegerOrNilBase: aRadix.
	value ifNil: [^aBlock value].
	^isNeg
		ifTrue: [value negated]
		ifFalse: [value]