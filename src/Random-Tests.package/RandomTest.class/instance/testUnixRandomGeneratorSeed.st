tests
testUnixRandomGeneratorSeed
	gen useUnixRandomGeneratorSeed.
	100 timesRepeat: [ | next |
			next := gen next.
			self assert: next >= 0.
			self assert: next < 1 ]