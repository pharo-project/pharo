tests - removing
testRemoveAllPreservesCapacity
	| methodSelector dictionary oldSize |
	methodSelector := #testRemoveAllPreservesCapacity.
	dictionary := MethodDictionary new.
	dictionary at: methodSelector put: self class >> methodSelector.
	
	oldSize := dictionary basicSize.
	dictionary removeAll.
	self assert: oldSize = dictionary basicSize.