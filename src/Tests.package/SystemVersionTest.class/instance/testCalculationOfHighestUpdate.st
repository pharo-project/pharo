tests
testCalculationOfHighestUpdate

	self assert: (SystemVersion fromVersionString: 'Pharo-6.0+alpha.build.510') highestUpdate equals: 60510 