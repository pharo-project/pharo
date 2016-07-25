morph creation
newOverflowRowIn: aThemedMorph forAll: aCollectionOfMorphs
	"Answer a new overflow row morph with the given contents."

	^OverflowRowMorph new
		baseMorphs: aCollectionOfMorphs;
		yourself