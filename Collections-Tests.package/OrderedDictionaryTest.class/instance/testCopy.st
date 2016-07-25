tests
testCopy
	| dictionary copy |

	dictionary := self dictionaryWithAssociations.
	copy := dictionary copy.
	self
		assertDictionary: dictionary equals: copy;
		deny: dictionary == copy;
		deny: dictionary dictionary == copy dictionary;
		deny: dictionary orderedKeys == copy orderedKeys.

	dictionary at: self singleKey put: self newValue.
	self deny: (copy at: self singleKey) = self newValue.