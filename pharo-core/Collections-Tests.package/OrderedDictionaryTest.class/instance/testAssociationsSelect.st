tests
testAssociationsSelect
	| dictionary selectedAssociations rejectedAssociation |

	rejectedAssociation := self dictionaryAssociations anyOne.
	selectedAssociations := self dictionaryAssociations copyWithout: rejectedAssociation.
	dictionary :=
		self dictionaryWithAssociations
			associationsSelect: [:each | each ~= rejectedAssociation].
	self
		assertIsDictionary: dictionary withAssociations: selectedAssociations;
		deny: (dictionary includesAssociation: rejectedAssociation).

	(dictionary associationsSelect: [:each | true])
		at: selectedAssociations anyOne key
		put: self newValue.
	self assertIsDictionary: dictionary withAssociations: selectedAssociations.