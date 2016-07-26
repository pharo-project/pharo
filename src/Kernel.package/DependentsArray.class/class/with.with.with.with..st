instance creation
with: firstObject with: secondObject with: thirdObject with: fourthObject 
	^(self basicNew: 4)
		basicAt: 1 put: firstObject;
		basicAt: 2 put: secondObject;
		basicAt: 3 put: thirdObject;
		basicAt: 4 put: fourthObject;
		yourself