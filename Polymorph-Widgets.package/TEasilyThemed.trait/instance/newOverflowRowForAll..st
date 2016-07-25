controls
newOverflowRowForAll: aCollectionOfMorphs
	"Answer a new overflow row morph that provides a drop down for
	the given contents that are unable to fit the bounds."
	
	^self theme
		newOverflowRowIn: self
		forAll: aCollectionOfMorphs