assertions
assertIsDictionary: aFirstDictionary copiedFrom: aSecondDictionary withOrderedAssociations: anAssociationCollection
	self
		deny: aFirstDictionary == aSecondDictionary;
		deny: aFirstDictionary dictionary == aSecondDictionary dictionary;
		deny: aFirstDictionary orderedKeys == aSecondDictionary orderedKeys.

	"esnure the associations were copied (the keys and values can be shared)"
	aFirstDictionary associations do: [:each |
		self deny: (aSecondDictionary associations identityIncludes: each)].

	self
		assertIsDictionary: aFirstDictionary
		withOrderedAssociations: anAssociationCollection.