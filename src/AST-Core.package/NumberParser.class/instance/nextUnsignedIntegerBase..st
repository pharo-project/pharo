parsing-public
nextUnsignedIntegerBase: aRadix 
	"Form an unsigned integer with incoming digits from sourceStream.
	Fail if no digit found.
	Count the number of digits and the lastNonZero digit and store int in instVar "
	
	| value |
	value := self nextUnsignedIntegerOrNilBase: aRadix.
	value ifNil: [^self expected: ('a digit between 0 and ' copyWith: (Character digitValue: aRadix - 1))].
	^value