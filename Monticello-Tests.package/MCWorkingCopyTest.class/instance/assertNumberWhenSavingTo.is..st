asserting
assertNumberWhenSavingTo: aRepository is: aNumber
	| name |
	name := nil.
	[aRepository storeVersion: (workingCopy newVersionIn: aRepository)]
		on: MCVersionNameAndMessageRequest
		do: [:n | name := n suggestedName. n resume: (Array with: name with: '')].
	self assert: name = (self packageName, '-', Author fullName, '.', aNumber asString)