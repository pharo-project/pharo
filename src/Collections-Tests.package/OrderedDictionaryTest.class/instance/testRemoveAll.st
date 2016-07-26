tests
testRemoveAll
	| dictionary |

	dictionary := self dictionaryWithAssociations.
	self deny: dictionary isEmpty.
	dictionary removeAll.
	self
		assert: dictionary isEmpty;
		assert: (dictionary orderedKeys allSatisfy: [:each | each isNil]).
	dictionary addAll: self dictionaryAssociations.
	self deny: dictionary isEmpty.