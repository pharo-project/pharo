comparing
literalEqual: other
	"Testing equality is not enough.
	It is also necessary to test number of decimal places (scale).
	Otherwise we cannot compile both literals 0.5s1 and 0.50s2 in the same method"
	
	^(super literalEqual: other) and: [self scale = other scale]