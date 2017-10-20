tests - others
testGrowPreservesElements
	| methodDictionary oldCapacity growedMethodDictionary |
	methodDictionary := self class methodDict copy.
	growedMethodDictionary := methodDictionary copy; grow; yourself.

	self assertPreservesElements: methodDictionary comparedTo: growedMethodDictionary.
	self assertPreservesElements: growedMethodDictionary comparedTo: methodDictionary.