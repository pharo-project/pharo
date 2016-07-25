assertions
assertPreservesElements: oldDictionary comparedTo: newDictionary
	self assert: (oldDictionary keys allSatisfy: [ :key |
		(newDictionary includesKey: key) &
		((newDictionary at: key) == (oldDictionary at: key))
		])