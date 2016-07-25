instance creation
selector: aSelector with: aPropertyOrPragma
	^(self new: 1)
		selector: aSelector;
		basicAt: 1 put: aPropertyOrPragma;
		yourself