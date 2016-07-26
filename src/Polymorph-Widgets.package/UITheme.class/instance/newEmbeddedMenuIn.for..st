morph creation
newEmbeddedMenuIn: aThemedMorph for: aModel
	"Answer a new menu."

	^EmbeddedMenuMorph new
		defaultTarget: aModel;
		color: (self menuColorFor: aThemedMorph)