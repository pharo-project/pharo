inline
foo: aValue
	^(1 to: 10) inject: aValue into: [:sum :each | sum + each]