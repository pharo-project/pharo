instance creation
with: firstObject with: secondObject 
	^(self basicNew: 2)
		basicAt: 1 put: firstObject;
		basicAt: 2 put: secondObject;
		yourself