tests
testNext

	| nextDay |
	nextDay := june2nd1973 next.
	self assert: nextDay equals: '3 June, 1973' asDate.