tests
testDo
	| dictionary |

	dictionary := self emptyDictionary.
	self
		should: [:block | dictionary do: block]
		enumerate: #().
	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		self
			should: [:block | dictionary do: block]
			enumerate: (self orderedValuesFirst: i)].