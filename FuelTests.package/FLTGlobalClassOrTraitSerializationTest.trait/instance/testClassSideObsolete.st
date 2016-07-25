tests
testClassSideObsolete
	"Tests that serializer does not serialize as global the class-side of an obsolete class or trait."
	
	| classOrTrait |
	classOrTrait := self newClassOrTrait.
	self removeFromSystem: classOrTrait.

	self 
		should: [self serialize: classOrTrait ] 
		raise: FLObsolete
		whoseDescriptionIncludes: classOrTrait name
		description: 'Should raise an error when serializing as global an obsolete class or trait.' 