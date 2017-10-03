utilities
checkIsCompatibleWithResultOfVersionMessage: aString
	"Checks whether a string is compatible with the result of the existing #version message who returned 
	   Pharomajor.minor[suffix]
	"
	
	| rx |
	rx := '(Pharo)([0-9]+)[.]([0-9]+)([a-z]+)' asRegex.
	self 
		assert: (rx matches: aString);
		assert: (rx subexpression: 2) equals: 'Pharo';
		assert: (rx subexpression: 3) asNumber > 0;
		assert: (rx subexpression: 4) asNumber >= 0;
		assert: (rx subexpression: 5) notEmpty