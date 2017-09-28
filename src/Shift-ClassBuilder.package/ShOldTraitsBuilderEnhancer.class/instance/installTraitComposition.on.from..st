building
installTraitComposition: anObject on: aClass from: anOldClass
	| oldComposition aTraitComposition |
	aTraitComposition := anObject asTraitComposition.
	aTraitComposition assertValidUser: aClass.

	oldComposition := #() asTraitComposition.

	aClass traitComposition: aTraitComposition.

	aTraitComposition allSelectors do:[:e | aClass updateMethodDictionarySelector: e].