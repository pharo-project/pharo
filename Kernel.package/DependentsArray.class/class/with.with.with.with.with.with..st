instance creation
with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject with: sixthObject
	^(self basicNew: 6)
		basicAt: 1 put: firstObject;
		basicAt: 2 put: secondObject;
		basicAt: 3 put: thirdObject;
		basicAt: 4 put: fourthObject;
		basicAt: 5 put: fifthObject;
		basicAt: 6 put: sixthObject;
		yourself