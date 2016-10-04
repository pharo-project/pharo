tests
testAssociationAtIfPresent
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert:
			(dictionary
				associationAt: each key
				ifPresent: [self fail]) isNil.

		dictionary add: each.
		self assert:
			(dictionary
				associationAt: each key
				ifPresent: [:assoc | self newValue -> assoc]) = (self newValue -> each).
		"ensure cull: is used"
		self assert:
			(dictionary
				associationAt: each key
				ifPresent: [self newValue]) = self newValue].