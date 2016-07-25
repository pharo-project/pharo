tests - reading
testReadFrom
	self assert: (Fraction readFromString: '3') = 3 description: 'denominator is optional'.
	self assert: (Fraction readFromString: '2/3') = (2/3) description: 'fraction and denominator must follow numerator'.
	self assert: (Fraction readFromString: '-2/3') = (-2/3) description: 'numerator can specify a minus sign'.
	.self assert: (Fraction readFromString: '2/-5') = (-2/5) description: 'denominator can specify a minus sign'.
	self assert: (Fraction readFromString: '-3/-7') = (3/7) description: 'numerator and denominator can both specify a minus sign'.
	self assert: (Fraction readFromString: '2e3/3') = (2000/3) description: 'numerator can specify an exponent'.
	self assert: (Fraction readFromString: '3/1e4') = (3/10000) description: 'denominator can specify an exponent'.
	self assert: (Fraction readFromString: '16rA0/3') = (160/3) description: 'numerator can specify a radix'.
	self assert: (Fraction readFromString: '1/3r22') = (1/8) description: 'denominator can specify a radix'.