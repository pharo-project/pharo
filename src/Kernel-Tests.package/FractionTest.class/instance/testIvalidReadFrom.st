tests - reading
testIvalidReadFrom
	self should: [Fraction readFromString: '+3'] raise: Error description: 'numerator cannot specify a plus sign'.
	self should: [Fraction readFromString: '-2/+3'] raise: Error description: 'denominator cannot specify a plus sign'.
	self should: [Fraction readFromString: '(3/2)'] raise: Error description: 'parenthesis are not allowed'.
	self should: [Fraction readFromString: ' 3/25'] raise: Error description: 'leading spaces are not allowed before numerator'.
	self should: [Fraction readFromString: '22/ 3'] raise: Error description: 'leading spaces are not allowed before denominator'.
	
	"These behaviours are questionnable, but that's currently how it works:"
	self assert: (Fraction readFromString: '12345with some trailing characters') = 12345 description: 'non numeric trailing characters interrupt decoding'.
	self assert: (Fraction readFromString: '1 / 2') = 1 description: 'A space behind numerator interrupt decoding'.
	self assert: (Fraction readFromString: '22.0/3') = 22 description: 'decimal point interrupt decoding'.
	self assert: (Fraction readFromString: '23s0/3') = 23 description: 'scale specification interrupt decoding'.