morph creation
newMenuIn: aThemedMorph for: aModel
	"Answer a new menu."

	^MenuMorph new
		defaultTarget: aModel;
		color: (self menuColorFor: aThemedMorph)