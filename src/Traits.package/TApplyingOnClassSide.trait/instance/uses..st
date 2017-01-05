composition
uses: aTraitCompositionOrArray
	| copyOfOldTrait newComposition |
	copyOfOldTrait := self copy.
	newComposition := aTraitCompositionOrArray asTraitComposition.
	self assertConsistantCompositionsForNew: newComposition.
	self setTraitComposition: newComposition.
	SystemAnnouncer uniqueInstance
		traitDefinitionChangedFrom: copyOfOldTrait to: self;
		classModificationAppliedTo: self