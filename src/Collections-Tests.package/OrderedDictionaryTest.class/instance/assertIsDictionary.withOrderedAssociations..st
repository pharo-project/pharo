assertions
assertIsDictionary: anObject withOrderedAssociations: anAssociationCollection
	"tests that anObject is an instance of the correct dictionary class
	with the specified ordered associations"
	self
		assert: anObject class == self dictionaryClass;
		assert: anObject orderedKeys size >= anAssociationCollection size;
		assert: anObject associations size = anAssociationCollection size.

	anAssociationCollection withIndexDo: [:each :i |
		self isTestingIdentityDictionary
			ifTrue: [
				self
					assert: (anObject orderedKeys at: i) == each key;
					assert: (anObject associations at: i) key == each key]
			ifFalse: [
				self
					assert: (anObject orderedKeys at: i) = each key;
					assert: (anObject associations at: i) key = each key].
		self assert: (anObject associations at: i) value = each value].