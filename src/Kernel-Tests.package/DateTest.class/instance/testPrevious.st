tests
testPrevious

	| previousDay |
	previousDay := june2nd1973 previous.
	self assert: previousDay equals: '1 June, 1973' asDate.