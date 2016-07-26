conversion
configureForIntegers
	"Configure the receiver to match/convert positive or negative integers only."

	self
		objectClass: Integer;
		regexString: '[-]?[0-9]+';
		stringTransformBlock: [:s |
			(s beginsWith: '-')
				ifTrue: ['-', (s select: [:c | c isDigit])]
				ifFalse: [s select: [:c | c isDigit]]]