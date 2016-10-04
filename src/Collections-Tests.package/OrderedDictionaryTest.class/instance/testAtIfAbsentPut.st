tests
testAtIfAbsentPut
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert:
			(dictionary
				at: each key
				ifAbsentPut: [each value]) = each value.
		self assert:
			(dictionary
				at: each key
				ifAbsentPut: [self fail]) = each value].