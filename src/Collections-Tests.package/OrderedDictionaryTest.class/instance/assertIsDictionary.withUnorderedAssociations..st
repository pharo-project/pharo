assertions
assertIsDictionary: anObject withUnorderedAssociations: anAssociationCollection
	"tests that anObject is an instance of the correct dictionary class
	with the specified associations, but ignoring the order"
	self
		assert: anObject class == self dictionaryClass;
		assert: anObject size = anAssociationCollection size.
	anAssociationCollection do: [:each |
		self assert: (anObject includesAssociation: each)].