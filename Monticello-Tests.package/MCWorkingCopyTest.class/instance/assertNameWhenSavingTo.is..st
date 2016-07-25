asserting
assertNameWhenSavingTo: aRepository is: aString
	| name |
	name := nil.
	[aRepository storeVersion: (workingCopy newVersionIn: aRepository)]
		on: MCVersionNameAndMessageRequest
		do: [:n | name := n suggestedName. n resume: (Array with: name with: '')].
	self assert: name = aString