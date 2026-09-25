Extension { #name : 'DateTest' }

{ #category : '*Kernel-Tests-WithCompiler' }
DateTest >> testStoring [

	| expected actual |
	expected := '''2 June 1973'' asDate'.
	actual := self june2nd1973 storeString.
	self
		assert: actual equals: expected;
		assert: (OpalEvaluator new evaluate: expected) equals: self june2nd1973. "Evaluating expected to avoid surprises when evaluating"
]
