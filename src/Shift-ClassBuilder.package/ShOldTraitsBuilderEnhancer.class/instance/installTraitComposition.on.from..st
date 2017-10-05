building
installTraitComposition: anObject on: aClass from: anOldClass
	| aTraitComposition |
	aTraitComposition := anObject asTraitComposition.

	aTraitComposition = #() asTraitComposition ifTrue:[^self].

	aTraitComposition assertValidUser: aClass.
	aClass traitComposition: aTraitComposition.

	aTraitComposition allSelectors do:[:e | aClass updateMethodDictionarySelector: e].