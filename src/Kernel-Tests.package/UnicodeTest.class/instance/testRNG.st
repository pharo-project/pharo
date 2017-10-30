helper
testRNG
	| chars uniqueCount |
	chars := (1 to: 100)
		collect: [ :_ | unicodeGenerator randomCharacter].
	uniqueCount := chars asSet size.
	self assert: uniqueCount = 100 description: (100 - uniqueCount) asString , ' duplicates'