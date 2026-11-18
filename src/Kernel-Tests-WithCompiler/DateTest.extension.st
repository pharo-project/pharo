Extension { #name : 'DateTest' }

{ #category : '*Kernel-Tests-WithCompiler' }
DateTest >> testStoring [

	| expected actual |
	expected := '(Date readFrom: ''1973-06-02'' pattern: ''yyyy-mm-dd'')'.
	actual := self june2nd1973 storeString.
	self
		assert: actual equals: expected;
		assert: (OpalEvaluator new evaluate: expected) equals: self june2nd1973 "Evaluating expected to avoid surprises when evaluating"
]
