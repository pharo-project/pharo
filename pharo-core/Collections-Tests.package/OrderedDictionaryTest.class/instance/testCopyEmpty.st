tests
testCopyEmpty
	| dictionary copy |

	dictionary := self dictionaryWithAssociations.
	copy := dictionary copyEmpty.
	self
		assertDictionary: dictionary doesNotEqual: copy;
		assert: copy isEmpty;
		deny: dictionary isEmpty.