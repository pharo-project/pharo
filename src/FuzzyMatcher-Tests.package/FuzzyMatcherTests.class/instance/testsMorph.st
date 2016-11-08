tests
testsMorph

	| fuzzy |
	
	fuzzy := self newFuzzer.

	fuzzy setPattern: 'morph'.
	
	self assert: (fuzzy matches: 'Morph').