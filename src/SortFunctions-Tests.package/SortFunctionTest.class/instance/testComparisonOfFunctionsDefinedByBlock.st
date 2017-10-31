tests
testComparisonOfFunctionsDefinedByBlock
	| collatorBlock |
	collatorBlock := [ :a :b | 1 ].
	
	self assert: collatorBlock ascending equals: collatorBlock ascending.
	self assert: collatorBlock ascending hash equals: collatorBlock ascending hash.
	
	self deny: collatorBlock ascending equals: collatorBlock descending.
	self deny: collatorBlock ascending equals: [ :a :b | -1 ] ascending