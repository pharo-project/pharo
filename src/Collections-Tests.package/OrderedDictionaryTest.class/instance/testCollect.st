tests
testCollect
	| dictionary |

	dictionary := self dictionaryWithAssociations.
	self
		assertIsDictionary: (dictionary collect: [:each | self newValue])
		withAssociations:
			(self dictionaryAssociations collect: [:each | each value: self newValue]).