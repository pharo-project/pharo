tests
testBindingsDo
	| dictionary |

	dictionary := self emptyDictionary.
	self
		should: [:block | dictionary bindingsDo: block]
		enumerate: #().
	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		self
			should: [:block | dictionary bindingsDo: block]
			enumerate: (self orderedAssociationsFirst: i)].