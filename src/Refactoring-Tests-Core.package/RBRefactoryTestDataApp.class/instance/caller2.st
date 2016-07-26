inline
caller2
	^(1 to: 10) inject: 1 into: [:sum :each | sum * (self foo: each)]