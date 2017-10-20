tests - rehashing
testRehashPreservesElements
	| oldDictionary rehashedDictionary |
	oldDictionary := self modifiedMethodDictionaryCopy.
	
	rehashedDictionary := oldDictionary copy rehash.
	
	self assertPreservesElements: oldDictionary comparedTo: rehashedDictionary.
	self assertPreservesElements: rehashedDictionary comparedTo: oldDictionary.