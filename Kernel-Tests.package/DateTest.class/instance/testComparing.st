tests
testComparing

	| sameDate laterDate earlierDate |
	sameDate := june2nd1973 asString asDate.
	laterDate := june2nd1973 + 1 day.
	earlierDate := june2nd1973 - 1 day.

	self
		assert: june2nd1973 equals: sameDate;
		assert: june2nd1973 equals: june2nd1973 copy;
		assert: june2nd1973 hash equals: sameDate hash.
	self 
		assert: june2nd1973 < laterDate;
		assert: june2nd1973 > earlierDate.
