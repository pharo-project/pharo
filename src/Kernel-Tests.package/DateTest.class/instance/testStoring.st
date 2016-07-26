tests
testStoring

	| expected actual |
	expected := '''2 June 1973'' asDate'.
	actual := june2nd1973 storeString.
	self	
		assert: actual equals: expected;
		assert: (Smalltalk evaluate: expected) equals: june2nd1973. "Evaluating expected to avoid surprises when evaluating"
