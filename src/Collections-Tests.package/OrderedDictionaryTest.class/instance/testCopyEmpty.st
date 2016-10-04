tests
testCopyEmpty
	| dictionary copy |

	dictionary := self dictionaryWithOrderedAssociations.
	copy := dictionary copyEmpty.
	self
		assertIsDictionary: dictionary
		withOrderedAssociations: self orderedAssociations.
	self
		assertIsDictionary: copy
		copiedFrom: dictionary
		withOrderedAssociations: #().