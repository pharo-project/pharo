tests
testRemoveAll
	| dictionary removedKeys |

	dictionary := self dictionaryWithOrderedAssociations.
	removedKeys := dictionary keys.
	self
		deny: dictionary isEmpty;
		assert: dictionary removeAll == dictionary;
		assert: dictionary isEmpty.
	removedKeys do: [:each |
		self
			assertKey: each
			wasRemovedfrom: dictionary].