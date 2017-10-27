tests
testComparisonOfFunctions

	self assert: #name ascending, #surname equals: #name ascending, #surname.
	self assert: (#name ascending, #surname) hash equals: (#name ascending, #surname) hash.

	self deny: #name ascending, #surname equals: #name ascending, #surname2.
	self deny: #name ascending, #surname equals: #name descending, #surname.
	self deny: #name ascending, #surname ascending equals: #name ascending, #surname descending.