assertions
assertIsDictionary: anObject withAssociations: anAssociationCollection
	"tests that anObject is an instance of the correct dictionary class
	with the specified ordered associations"
	self
		assert: anObject class == self dictionaryClass;
		assert: anObject associations asArray = anAssociationCollection asArray