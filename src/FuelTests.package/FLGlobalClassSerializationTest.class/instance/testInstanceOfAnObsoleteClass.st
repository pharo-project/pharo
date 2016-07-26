tests
testInstanceOfAnObsoleteClass
	"Tests that serializer does not serialize an instance of an obsolete class."
	
	| aClass instance |
	aClass := self newClassOrTrait.
	instance := self newInstanceFrom: aClass.
	self removeFromSystem: aClass.

	self 
		should: [self serialize: instance ] 
		raise: FLObsolete
		whoseDescriptionIncludes: aClass name
		description: 'Should raise an error when serializing as global an obsolete class.' 