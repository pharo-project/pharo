asserting
assert: aCollection hasElements: anArray
	self assert: (aCollection collect: [:ea | ea token]) asSet = anArray asSet