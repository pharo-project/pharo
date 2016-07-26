accessing
createdClassesAndTraits
	createdClassesAndTraits ifNil: [
		createdClassesAndTraits := OrderedCollection new].
	^createdClassesAndTraits