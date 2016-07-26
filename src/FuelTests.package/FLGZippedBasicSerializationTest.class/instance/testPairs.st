tests
testPairs

	| pairs |
	pairs := (1 to: 1 << 14) 
		collect: [ :i | 
			FLPair new 
				left: i; 
				right: i+1;
				yourself ].

	self assertSerializationEqualityOf: pairs