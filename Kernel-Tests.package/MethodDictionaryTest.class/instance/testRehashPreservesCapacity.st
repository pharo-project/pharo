tests - rehashing
testRehashPreservesCapacity
	| oldDictionary rehashedDictionary |
	oldDictionary := self modifiedMethodDictionaryCopy.

	rehashedDictionary := oldDictionary copy rehash.
	
	self assertPreservesCapacity: oldDictionary comparedTo: rehashedDictionary.