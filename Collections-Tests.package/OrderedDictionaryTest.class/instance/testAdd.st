tests
testAdd
	| dictionary associationToReplace replacementAssociation |

	dictionary := self emptyDictionary.
	self
		deny: (self dictionaryAssociations anySatisfy: [:each | dictionary includesAssociation: each]);
		assert: (self dictionaryAssociations allSatisfy: [:each | (dictionary add: each) == each]);
		assert: (self dictionaryAssociations allSatisfy: [:each | dictionary includesAssociation: each]);
		assert: dictionary associations = self dictionaryAssociations.

	associationToReplace := self dictionaryAssociations anyOne.
	replacementAssociation := associationToReplace copy.
	replacementAssociation value: self newValue.
	self
		assert: (dictionary add: replacementAssociation) == replacementAssociation;
		assert: (dictionary includesAssociation: replacementAssociation);
		deny: (dictionary includesAssociation: associationToReplace).
	self
		assertIsDictionary: dictionary
		withAssociations: 
			(self dictionaryAssociations
				replaceAll: associationToReplace
				with: replacementAssociation).