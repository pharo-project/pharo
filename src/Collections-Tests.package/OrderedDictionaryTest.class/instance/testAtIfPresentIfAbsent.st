tests
testAtIfPresentIfAbsent
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert:
			(dictionary
				at: each key
				ifPresent: [self fail]
				ifAbsent: [self absentValue]) = self absentValue.

		dictionary add: each.
		self assert:
			(dictionary
				at: each key
				ifPresent: [:value | self newValue -> value]
				ifAbsent: [self fail]) = (self newValue -> each value).
		"ensure cull: is used"
		self assert:
			(dictionary
				at: each key
				ifPresent: [self newValue]
				ifAbsent: [self fail]) = self newValue].