tests
testLotsOfCharacters

	| all |
	all := OrderedCollection new.
	(1 << 16) timesRepeat: [
	all add: (1 to: 255 ) atRandom asCharacter ].
	self assertSerializationEqualityOf: all.