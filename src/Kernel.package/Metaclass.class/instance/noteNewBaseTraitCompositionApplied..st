composition
noteNewBaseTraitCompositionApplied: aTraitComposition
	"The argument is the new trait composition of my base trait - add
	the new traits or remove non existing traits on my class side composition.
	(Each class trait in my composition has its base trait on the instance side
	of the composition - manually added traits to the class side are always 
	base traits.)"
	
	| newComposition traitsFromInstanceSide |
	traitsFromInstanceSide := self traitComposition traits
		select: [:each | each isClassTrait]
		thenCollect: [:each | each baseTrait].
		
	newComposition := self traitComposition copyTraitExpression.
	(traitsFromInstanceSide copyWithoutAll: aTraitComposition traits) do: [:each |
		newComposition removeFromComposition: each classTrait].
	(aTraitComposition traits copyWithoutAll: traitsFromInstanceSide) do: [:each |
		newComposition add:  (each classTrait)].

	self setTraitComposition: newComposition