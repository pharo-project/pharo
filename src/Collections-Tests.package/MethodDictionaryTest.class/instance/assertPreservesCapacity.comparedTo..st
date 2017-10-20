assertions
assertPreservesCapacity: oldDictionary comparedTo: rehashedDictionary
	self assert: oldDictionary capacity = rehashedDictionary capacity.