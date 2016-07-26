conversion
configureForPositiveIntegers
	"Configure the receiver to match/convert positive integers only."

	self
		objectClass: Integer;
		regexString: '[0-9]+';
		stringTransformBlock: [:s |
			s select: [:c | c isDigit]]