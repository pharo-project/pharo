traits
setTraitComposition: aTraitComposition
	| oldComposition |
	(self hasTraitComposition not and: [aTraitComposition isEmpty]) ifTrue: [^self].
	aTraitComposition assertValidUser: self.

	oldComposition := self traitComposition.
	self traitComposition: aTraitComposition.
	self applyChangesOfNewTraitCompositionReplacing: oldComposition.
	
	oldComposition traits do: [:each | each removeUser: self].
	aTraitComposition traits do: [:each | each addUser: self]