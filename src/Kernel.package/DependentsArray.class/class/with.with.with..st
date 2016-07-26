instance creation
with: firstObject with: secondObject with: thirdObject 
	^(self basicNew: 3)
		basicAt: 1 put: firstObject;
		basicAt: 2 put: secondObject;
		basicAt: 3 put: thirdObject;
		yourself