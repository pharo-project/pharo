tests
testSelect
	| dictionary |

	dictionary := self dictionaryWithAssociations.
	self
		assertIsDictionary: (dictionary select: [:each | each ~= self singleValue])
		withAssociations: (self dictionaryAssociations copyWithout: self singleAssociation).

	(dictionary select: [:each | true]) at: self singleKey put: self newValue.
	self deny: (dictionary at: self singleKey) = self newValue.