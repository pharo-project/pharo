tests
testAssociationsSelect
	| dictionary |

	dictionary := self dictionaryWithOrderedAssociations.
	self orderedAssociations do: [:assoc | | selectedAssociations |
		selectedAssociations := self orderedAssociations copyWithout: assoc.
		self
			assertIsDictionary:
				(dictionary associationsSelect: [:each |
					selectedAssociations includes: each])
			copiedFrom: dictionary
			withOrderedAssociations:
				(self orderedAssociations select: [:each |
					selectedAssociations includes: each])].