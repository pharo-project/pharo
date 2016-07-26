parsing-public
nextUnsignedIntegerBase: aRadix ifFail: errorBlock
	"Form an unsigned integer with incoming digits from sourceStream.
	Answer this integer, or execute errorBlock if no digit found.
	Count the number of digits and the position of lastNonZero digit and store them in instVar"
	
	| value |
	value := self nextUnsignedIntegerOrNilBase: aRadix.
	value ifNil: [^errorBlock value].
	^value