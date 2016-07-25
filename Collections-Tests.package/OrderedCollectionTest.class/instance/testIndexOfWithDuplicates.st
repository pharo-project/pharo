tests - enumerating
testIndexOfWithDuplicates

   | collection indices bagOfIndices |
   collection := #('Jim' 'Mary' 'John' 'Andrew' 'Mary' 'John' 'Jim' 'Micheal') asOrderedCollection.
   indices := collection collect: [:item | collection indexOf: item].
   self assert: indices asSet size = collection asSet size.
   bagOfIndices := indices asBag.
   self assert: (indices asSet 
                    allSatisfy: [:index | (bagOfIndices occurrencesOf: index)
	                                       = (collection occurrencesOf: (collection at: index))]).

  "  indexOf:  returns the index of the first occurrence of an item.
     For an item with n occurrences, the index of its first occurrence
     is found  n  times. "