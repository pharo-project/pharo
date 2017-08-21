composition
assertConsistantCompositionsForNew: aTraitComposition
	"Applying or modifying a trait composition on the class side
	of a behavior has some restrictions."

	| baseTraits notAddable message |
	baseTraits := aTraitComposition traits select: [:each | each isBaseTrait].
	baseTraits isEmpty ifFalse: [
		notAddable := (baseTraits reject: [:each | each classSide methodDict isEmpty]).
		notAddable isEmpty ifFalse: [
			message := String streamContents: [:stream |
				stream nextPutAll: 'You can not add the base trait(s)'; cr.
				notAddable
					do: [:each | stream nextPutAll: each name]
					separatedBy: [ stream nextPutAll: ', '].
				stream cr; nextPutAll: 'to this composition because it/they define(s) methods on the class side.'].
		^TraitCompositionException signal: message]].
		
	(self instanceSide traitComposition traits asSet =
			(aTraitComposition traits
				select: [:each | each isClassTrait]
				thenCollect: [:each | each baseTrait]) asSet) ifFalse: [
				^TraitCompositionException signal: 'You can not add or remove class side traits on
				the class side of a composition. (But you can specify aliases or exclusions
				for existing traits or add a trait which does not have any methods on the class side.)']