tests
testComparisonOfFunctionsDefinedBySymbol

	self assert: #name ascending equals: #name ascending.
	self assert: #name ascending hash equals: #name ascending hash.
	
	self deny: #name ascending equals: #name descending.
	self deny: #name ascending equals: #name2 ascending.
	self deny: #name ascending undefinedFirst equals: #name ascending undefinedLast