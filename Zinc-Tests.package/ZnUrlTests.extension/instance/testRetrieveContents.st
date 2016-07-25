*zinc-tests
testRetrieveContents
	| numbers |
	numbers := 'http://zn.stfx.eu/zn/numbers.txt' asZnUrl retrieveContents.
	numbers := numbers lines collect: [ :each | each asInteger ].
	self assert: (numbers allSatisfy: [ :each | each isInteger ]).
	self should: [ 'http://zn.stfx.eu/zn/numbers-wrong.txt' asZnUrl retrieveContents ] raise: Error