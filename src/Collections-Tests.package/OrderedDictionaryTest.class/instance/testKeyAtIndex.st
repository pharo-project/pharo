tests
testKeyAtIndex
	| dictionary |

	dictionary := self emptyDictionary.
	self
		should: [dictionary keyAtIndex: 0]
		raise: Error.
	self orderedAssociations withIndexDo: [:each :i |
		self
			should: [dictionary keyAtIndex: i]
			raise: Error.

		dictionary add: each.
		self assert: (dictionary keyAtIndex: i) = each key].