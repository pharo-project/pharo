private-extensions-traitbehavior
fuelInitializeTraitCompositionForTraitBehavior
	<flExtensionOf: #TraitBehavior selector: #fuelInitializeTraitComposition>
	self traitComposition traits do: [:each | each addUser: self]