tests
testCopy
	| theOriginal theCopy |
	theOriginal := self classToBeTested newFrom: 'abc'.
	theCopy := theOriginal copy.
	self assert: theCopy equals: theOriginal.
	theCopy remove: $a.
	self deny: (theCopy includes: $a).
	self
		assert: (theOriginal includes: $a)
		description: 'Changing the copy should not change the original'