testing
addCompositionOnLeft: aTraitComposition
	self transformations do: [ :each | aTraitComposition add: each ].
	^ aTraitComposition