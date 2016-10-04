tests
testIncludes
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self deny: (dictionary includes: each value).

		dictionary add: each.
		self assert: (dictionary includes: each value)].