tests
testIsSeperator

	| fuzzy |
	
	fuzzy := self newFuzzer.
	
	{ $_ . $: } do: [ :each |
		self assert: (fuzzy isSeperator: each)	
	]
